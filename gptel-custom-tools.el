;;; gptel-custom-tools.el --- Task management tools for gptel  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'pp)
(require 'subr-x)
(require 'gptel)

(defvar gptel-mode)
(defvar gptel-use-header-line)
(defvar gptel--fsm-last)
(defvar gptel--header-line-info)
(defvar gptel--request-alist)


;;;; Customization

(defgroup gptel-custom-tools nil
  "LLM-callable task management tools for gptel."
  :group 'gptel
  :prefix "gptel-custom-tools-")

(defcustom gptel-custom-tools-save-directory "~/Downloads/"
  "Directory used by `TaskSave' and `TaskLoad' when no path is given.

Combined with `gptel-custom-tools-save-filename'.  A relative path
passed to either tool is resolved against the gptel buffer's
`default-directory' instead, so project-scoped saves need no change
here."
  :type 'directory)

(defcustom gptel-custom-tools-save-filename "gptel-tasks.eld"
  "File name used by `TaskSave' and `TaskLoad' when no path is given.

Also appended when the path handed to either tool names an existing
directory.  The .eld extension (\"Emacs Lisp Data\", as used by
org-persist) marks the file as data rather than code; override this
freely."
  :type 'string)

(defcustom gptel-custom-tools-toggle-key "TAB"
  "Key that toggles the task list overlay, read with `kbd'.

Bound in the overlay's own keymap to
`gptel-custom-tools-toggle-display'.  Note that the overlay spans the
whole response block, so whichever key you pick stops doing its
major-mode job across that block -- with the default, that means
losing `org-cycle' over every response in an `org-mode' gptel buffer.
Setting this to a key sequence `org-mode' does not bind avoids that."
  :type 'string)


;;;; Constants

(defconst gptel-custom-tools-gptel-agent-min-version "0.0.1"
  "Oldest gptel-agent this module was checked against.

gptel-agent is optional: without it the tools still register and
return results, and only the overlay and header-line display are
affected.")

(defconst gptel-custom-tools--statuses
  '("pending" "in_progress" "completed" "waiting" "canceled")
  "The five task statuses, in lifecycle order.")

(defconst gptel-custom-tools--glyphs
  '(("pending"     . "○")
    ("in_progress" . "●")
    ("completed"   . "✓")
    ("waiting"     . "⏳")
    ("canceled"    . "✕"))
  "Overlay glyph for each status.")

(defconst gptel-custom-tools--faces
  '(("pending"     . nil)
    ("in_progress" . (:inherit (bold warning)))
    ("completed"   . (:inherit shadow :strike-through t))
    ("waiting"     . (:inherit warning))
    ("canceled"    . (:inherit shadow :strike-through t)))
  "Overlay face for each status.

The `in_progress' face is TodoWrite's, written as a single :inherit
list.  TodoWrite spells it `(:inherit bold :inherit warning)', where
the duplicate key means `warning' is silently dropped.")

(defconst gptel-custom-tools--hrule
  (propertize "\n" 'face '(:inherit shadow :underline t :extend t))
  "Full-width horizontal rule framing the overlay.

A propertized newline rather than a run of dashes, so that it renders
correctly at any window width.")

(defconst gptel-custom-tools--id-regexp "\\`task-\\([0-9]+\\)\\'"
  "Pattern for IDs this module mints.

IDs in a loaded file that do not match are kept verbatim and stay
valid targets for `TaskUpdate' and `TaskGet'; they just do not take
part in the counter computation.")


;;;; Buffer-local state

(defvar-local gptel-custom-tools--tasks nil
  "Task list for this gptel buffer, a list of plists in creation order.

Each task has exactly these keys:

  :id           string, unique within the buffer
  :content      string, imperative form, never empty
  :active-form  string or nil, present continuous form
  :status       string, one of `gptel-custom-tools--statuses'
  :parent-id    string or nil, :id of the parent task
  :note         string or nil, free-text context or blocker reason")

(defvar-local gptel-custom-tools--id-counter 0
  "Highest task number minted in this buffer.

Tracked in its own right, never derived from the length of the task
list, so that cancelling or reloading tasks cannot cause an ID to be
reused.")

(defvar-local gptel-custom-tools--overlay nil
  "The task list overlay in this buffer, if one exists.

Only a fallback for `gptel-custom-tools-toggle-display' when it is
called with point outside the overlay; rendering itself locates the
overlay through its `gptel-custom-tools--todos' property.")


;;;; Feature detection and execution context

(defvar gptel-custom-tools--ux-available 'unknown
  "Cache for `gptel-custom-tools--ux-available-p'.")

(defun gptel-custom-tools--ux-available-p ()
  "Return non-nil if gptel's private rendering interfaces are present.

Evaluated once and cached.  Gates all three private interfaces this
module reaches into -- `gptel--fsm-last' and `gptel-fsm-info' for
overlay anchoring, `gptel--header-line-info' for the in-progress
display -- so that a gptel that has renamed any of them makes the UX
a no-op rather than an error."
  (when (eq gptel-custom-tools--ux-available 'unknown)
    (setq gptel-custom-tools--ux-available
          (and (boundp 'gptel--fsm-last)
               (fboundp 'gptel-fsm-info)
               (fboundp 'gptel-fsm-p)
               (boundp 'gptel--header-line-info)
               t)))
  gptel-custom-tools--ux-available)

(defun gptel-custom-tools--fsm-info ()
  "Return the info plist of the request in flight, or nil."
  (and (gptel-custom-tools--ux-available-p)
       (let ((fsm gptel--fsm-last))
         (and (gptel-fsm-p fsm) (gptel-fsm-info fsm)))))

(defun gptel-custom-tools--inflight-buffer ()
  "Return the gptel buffer of some active request, or nil."
  (and (boundp 'gptel--request-alist)
       (cl-loop for entry in gptel--request-alist
                for fsm = (car-safe (cdr entry))
                for buf = (and (gptel-fsm-p fsm)
                               (plist-get (gptel-fsm-info fsm) :buffer))
                when (buffer-live-p buf) return buf)))

(defun gptel-custom-tools--origin-buffer ()
  "Resolve the gptel buffer whose task list this tool call targets.

Return a live buffer, or the symbol `dead' when the originating
buffer was identified but has since been killed.

A tool callback is not guaranteed to run with the originating buffer
current.  gptel wraps synchronous tool calls in `with-current-buffer',
but tool calls awaiting confirmation -- which is every `TaskSave' --
are applied later by `gptel--accept-tool-calls', outside that form."
  (let ((info (gptel-custom-tools--fsm-info)))
    (cond
     (info (let ((buf (plist-get info :buffer)))
             (cond ((buffer-live-p buf) buf)
                   (buf 'dead)
                   (t (current-buffer)))))
     ((bound-and-true-p gptel-mode) (current-buffer))
     ((gptel-custom-tools--inflight-buffer))
     ;; Nothing identifiable: a direct Elisp or ERT call.  There is no
     ;; other sensible target, and no originating buffer was lost.
     (t (current-buffer)))))

(defmacro gptel-custom-tools--with-tasks (&rest body)
  "Evaluate BODY with the originating gptel buffer current.

Never signals: BODY's value is returned on success, and any error --
including a killed originating buffer -- becomes a string starting
with \"Error: \", so a failing tool hands the model a usable result
instead of breaking the request state machine."
  (declare (indent 0) (debug t))
  (let ((buf (gensym "buf"))
        (err (gensym "err")))
    `(condition-case ,err
         (let ((,buf (gptel-custom-tools--origin-buffer)))
           (if (bufferp ,buf)
               (with-current-buffer ,buf ,@body)
             "Error: the gptel buffer holding this task list has been killed."))
       (error (format "Error: %s" (error-message-string ,err))))))


;;;; Task model

(defun gptel-custom-tools--arg (value)
  "Normalize a tool argument VALUE into a string or nil.

Returns nil for an omitted argument and a trimmed string otherwise.
The empty string survives: per the tool contract it is the explicit
\"clear this field\" signal, and it must stay distinguishable from
omission, which means \"leave unchanged\"."
  (cond
   ((null value) nil)
   ((memq value '(:json-false :null)) nil)
   ((stringp value) (string-trim value))
   (t (format "%s" value))))

(defun gptel-custom-tools--blank-p (string)
  "Return non-nil if STRING is nil or empty."
  (or (null string) (string-empty-p string)))

(defun gptel-custom-tools--find (id &optional tasks)
  "Return the task with ID in TASKS, or nil.

TASKS defaults to the buffer-local task list."
  (and (stringp id)
       (seq-find (lambda (task) (equal (plist-get task :id) id))
                 (or tasks gptel-custom-tools--tasks))))

(defun gptel-custom-tools--ancestors (id tasks)
  "Return ID followed by its ancestors in TASKS, nearest first.

Stops at the first repeated ID, so a corrupt list cannot loop."
  (let ((seen (make-hash-table :test #'equal))
        (chain nil)
        (current id))
    (while (and current (not (gethash current seen)))
      (puthash current t seen)
      (push current chain)
      (setq current (plist-get (gptel-custom-tools--find current tasks) :parent-id)))
    (nreverse chain)))

(defun gptel-custom-tools--cycle-p (id parent-id tasks)
  "Return non-nil if giving ID the parent PARENT-ID would form a cycle.

That is the case when ID is PARENT-ID itself or one of its ancestors
in TASKS."
  (and (member id (gptel-custom-tools--ancestors parent-id tasks)) t))

(defun gptel-custom-tools--cyclic-id (tasks)
  "Return the ID of some task in TASKS caught in a `:parent-id' cycle."
  (cl-loop for task in tasks
           for id = (plist-get task :id)
           when (let ((seen (make-hash-table :test #'equal))
                      (current id)
                      (cyclic nil))
                  (while (and current (not cyclic))
                    (if (gethash current seen)
                        (setq cyclic t)
                      (puthash current t seen)
                      (setq current (plist-get (gptel-custom-tools--find current tasks)
                                               :parent-id))))
                  cyclic)
           return id))

(defun gptel-custom-tools--ordered (tasks)
  "Return TASKS as a list of (TASK . DEPTH) cons cells in render order.

Depth-first from each root, roots in creation order, then orphans --
tasks whose `:parent-id' names no task in TASKS -- at depth 0 in
creation order.  The walk is cycle-safe and never visits a task
twice, and a final pass picks up anything a cycle kept unreachable,
so every task in TASKS appears exactly once."
  (let ((index (make-hash-table :test #'equal))
        (children (make-hash-table :test #'equal))
        (visited (make-hash-table :test #'equal))
        (roots nil) (orphans nil) (ordered nil))
    (dolist (task tasks)
      (puthash (plist-get task :id) task index))
    (dolist (task tasks)
      (let ((parent (plist-get task :parent-id)))
        (cond
         ((null parent) (push task roots))
         ((gethash parent index)
          (puthash parent (cons task (gethash parent children)) children))
         (t (push task orphans)))))
    (setq roots (nreverse roots)
          orphans (nreverse orphans))
    (cl-labels
        ((walk (task depth)
           (let ((id (plist-get task :id)))
             (unless (gethash id visited)
               (puthash id t visited)
               (push (cons task depth) ordered)
               (dolist (child (reverse (gethash id children)))
                 (walk child (1+ depth)))))))
      (dolist (task roots) (walk task 0))
      (dolist (task orphans) (walk task 0))
      (dolist (task tasks) (walk task 0)))
    (nreverse ordered)))

(defun gptel-custom-tools--max-id (tasks)
  "Return the highest N among IDs of the form task-N in TASKS, or 0."
  (let ((highest 0))
    (dolist (task tasks highest)
      (let ((id (plist-get task :id)))
        (when (and (stringp id) (string-match gptel-custom-tools--id-regexp id))
          (setq highest (max highest (string-to-number (match-string 1 id)))))))))

(defun gptel-custom-tools--next-id ()
  "Mint and return the next task ID for the current buffer.

Advances the buffer-local counter, and keeps advancing past any ID
already in use -- a hand-written file can hold, say, both task-03 and
task-3."
  (let ((id nil))
    (while (progn
             (setq gptel-custom-tools--id-counter (1+ gptel-custom-tools--id-counter))
             (setq id (format "task-%d" gptel-custom-tools--id-counter))
             (gptel-custom-tools--find id)))
    id))


;;;; Tool output formatting

(defun gptel-custom-tools--format-list (tasks)
  "Return the `TaskList' rendering of TASKS.

One line per task: two spaces of indentation per depth level, the ID,
the status in brackets, the content, an explicit parent annotation for
any task that has one, and the note for `waiting' tasks."
  (if (null tasks)
      "No tasks."
    (mapconcat
     (pcase-lambda (`(,task . ,depth))
       (let ((parent (plist-get task :parent-id))
             (note (plist-get task :note)))
         (concat (make-string (* 2 depth) ?\s)
                 (plist-get task :id)
                 " [" (plist-get task :status) "] "
                 (plist-get task :content)
                 (and parent (format " (parent: %s)" parent))
                 (and (equal (plist-get task :status) "waiting") note
                      (concat " — " note)))))
     (gptel-custom-tools--ordered tasks)
     "\n")))

(defun gptel-custom-tools--format-task (task)
  "Return the `TaskGet' rendering of TASK.

One snake_case label per line; absent fields read (none)."
  (mapconcat
   (pcase-lambda (`(,label . ,key))
     (format "%s: %s" label (or (plist-get task key) "(none)")))
   '(("id"          . :id)
     ("status"      . :status)
     ("content"     . :content)
     ("active_form" . :active-form)
     ("parent_id"   . :parent-id)
     ("note"        . :note))
   "\n"))


;;;; Overlay rendering

(defun gptel-custom-tools--keymap ()
  "Return the keymap installed on the task list overlay."
  (let ((map (make-sparse-keymap))
        (key (ignore-errors (kbd gptel-custom-tools-toggle-key))))
    (when (and key (not (equal key "")))
      (define-key map key #'gptel-custom-tools-toggle-display)
      ;; <tab> and TAB are the GUI and terminal encodings of one
      ;; physical key; binding only one breaks the toggle in the other
      ;; kind of frame.
      (when (or (equal key (kbd "TAB")) (equal key (kbd "<tab>")))
        (define-key map (kbd "TAB") #'gptel-custom-tools-toggle-display)
        (define-key map (kbd "<tab>") #'gptel-custom-tools-toggle-display)))
    map))

(defun gptel-custom-tools--overlay-string (overlay)
  "Return the `after-string' for OVERLAY, rendering the current task list."
  (let ((body (mapconcat
               (pcase-lambda (`(,task . ,depth))
                 (let* ((status (plist-get task :status))
                        (glyph (or (cdr (assoc status gptel-custom-tools--glyphs)) "•"))
                        (face (cdr (assoc status gptel-custom-tools--faces)))
                        (note (plist-get task :note))
                        (label (if (equal status "in_progress")
                                   ;; :active-form is optional; no
                                   ;; rendering path may emit an empty
                                   ;; label.
                                   (or (plist-get task :active-form)
                                       (plist-get task :content))
                                 (plist-get task :content)))
                        (text (if (and (equal status "waiting") note)
                                  (concat label " — " note)
                                label)))
                   (concat (make-string (* 2 depth) ?\s)
                           glyph " "
                           (if face (propertize text 'face face) text))))
               (gptel-custom-tools--ordered gptel-custom-tools--tasks)
               "\n")))
    (concat
     (unless (eq (char-before (overlay-end overlay)) ?\n) "\n")
     gptel-custom-tools--hrule
     (propertize "Task list: [ " 'face '(:inherit (font-lock-comment-face bold)))
     (propertize (key-description (kbd gptel-custom-tools-toggle-key))
                 'face 'help-key-binding)
     (propertize " to toggle display ]\n" 'face 'font-lock-comment-face)
     body "\n"
     gptel-custom-tools--hrule)))

(defun gptel-custom-tools--in-progress-label ()
  "Return the label of the first in-progress task in render order, or nil."
  (cl-loop for (task . _) in (gptel-custom-tools--ordered gptel-custom-tools--tasks)
           when (equal (plist-get task :status) "in_progress")
           return (or (plist-get task :active-form) (plist-get task :content))))

(defun gptel-custom-tools--header-line-writable-p ()
  "Return non-nil if this buffer's header line can take a task display.

Writing element 2 assumes gptel's own three-element
`header-line-format'; anything else is left alone."
  (and (bound-and-true-p gptel-mode)
       (bound-and-true-p gptel-use-header-line)
       (consp header-line-format)
       (nthcdr 2 header-line-format)
       t))

(defun gptel-custom-tools--update-header-line ()
  "Show the in-progress task right-aligned in the header line."
  (when-let* (((gptel-custom-tools--ux-available-p))
              (label (gptel-custom-tools--in-progress-label))
              ((gptel-custom-tools--header-line-writable-p)))
    (let ((text (concat "Task: " label)))
      (setf (nth 2 header-line-format)
            (concat (propertize
                     " " 'display
                     `(space :align-to (- right ,(+ 5 (string-width text)))))
                    (propertize text 'face 'font-lock-escape-face))))))

(defun gptel-custom-tools--restore-header-line ()
  "Put gptel's own info segment back in the header line."
  (when (and (gptel-custom-tools--ux-available-p)
             (gptel-custom-tools--header-line-writable-p))
    (setf (nth 2 header-line-format) gptel--header-line-info)))

(defun gptel-custom-tools--find-overlay ()
  "Return the task list overlay near point, or the buffer's own, or nil."
  (or (seq-find (lambda (overlay)
                  (overlay-get overlay 'gptel-custom-tools--todos))
                (overlays-in (max (point-min) (1- (point)))
                             (min (point-max) (1+ (point)))))
      (and (overlayp gptel-custom-tools--overlay)
           (overlay-buffer gptel-custom-tools--overlay)
           gptel-custom-tools--overlay)))

;;;###autoload
(defun gptel-custom-tools-toggle-display ()
  "Toggle the display of the gptel task list overlay.

Collapsing stashes the rendered list in the overlay's
`gptel-custom-tools--todos' property, which is where a re-render
writes while the list is hidden -- so updates land without popping
the list back open."
  (interactive)
  (let ((overlay (gptel-custom-tools--find-overlay)))
    (unless overlay
      (user-error "No gptel task list here"))
    (if-let* ((shown (overlay-get overlay 'after-string)))
        (progn (overlay-put overlay 'gptel-custom-tools--todos shown)
               (overlay-put overlay 'after-string nil))
      (let ((stashed (overlay-get overlay 'gptel-custom-tools--todos)))
        (overlay-put overlay 'after-string (and (stringp stashed) stashed))
        (overlay-put overlay 'gptel-custom-tools--todos t)))))

(defun gptel-custom-tools--render-1 ()
  "Re-render the task list overlay and header line.

The unguarded worker behind `gptel-custom-tools--render'."
  (when-let* ((info (gptel-custom-tools--fsm-info))
              (position (plist-get info :position))
              (where-to (if (markerp position) (marker-position position) position))
              (where-from (previous-single-property-change
                           position 'gptel nil (point-min)))
              ;; A zero-length overlay with `evaporate' is deleted the
              ;; moment it is created, so skip the render entirely.
              ((/= where-from where-to)))
    (let ((overlay (cdr (get-char-property-and-overlay
                         where-from 'gptel-custom-tools--todos))))
      (cond
       ((null gptel-custom-tools--tasks)
        ;; Never leave a stale list on screen.
        (when overlay (delete-overlay overlay))
        (setq gptel-custom-tools--overlay nil))
       (t
        (if overlay
            (move-overlay overlay where-from where-to)
          (setq overlay (make-overlay where-from where-to nil t))
          (overlay-put overlay 'gptel-custom-tools--todos t)
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'priority -40)
          (overlay-put overlay 'keymap (gptel-custom-tools--keymap))
          ;; Registered once, at overlay creation.  `plist-put' rather
          ;; than `push': gptel's plist handling requires it.
          (let ((buffer (current-buffer)))
            (plist-put
             info :post
             (cons (lambda (&rest _)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (gptel-custom-tools--restore-header-line))))
                   (plist-get info :post)))))
        (setq gptel-custom-tools--overlay overlay)
        (let ((display (gptel-custom-tools--overlay-string overlay)))
          (if (stringp (overlay-get overlay 'gptel-custom-tools--todos))
              ;; Collapsed: refresh the stash, leave the list hidden.
              (overlay-put overlay 'gptel-custom-tools--todos display)
            (overlay-put overlay 'after-string display)))
        (gptel-custom-tools--update-header-line))))))

(defun gptel-custom-tools--render ()
  "Re-render the task list, if the UX is available.

A rendering failure must never stop a tool from returning its
result, so errors here are demoted to a message."
  (when (gptel-custom-tools--ux-available-p)
    (with-demoted-errors "gptel-custom-tools: could not render task list: %S"
      (gptel-custom-tools--render-1))))


;;;; Persistence

(defun gptel-custom-tools--resolve-path (path)
  "Return the absolute file name PATH resolves to.

Nil or empty means `gptel-custom-tools-save-filename' in
`gptel-custom-tools-save-directory'; an absolute PATH is used as
given; a relative PATH is expanded against the gptel buffer's
`default-directory'; and a PATH naming an existing directory gets the
save filename appended."
  (let* ((path (gptel-custom-tools--arg path))
         (resolved
          (cond
           ((gptel-custom-tools--blank-p path)
            (expand-file-name gptel-custom-tools-save-filename
                              gptel-custom-tools-save-directory))
           ((file-name-absolute-p path) (expand-file-name path))
           (t (expand-file-name path default-directory)))))
    (if (file-directory-p resolved)
        (expand-file-name gptel-custom-tools-save-filename resolved)
      resolved)))

(defun gptel-custom-tools--write-file (file tasks)
  "Write TASKS to FILE as a readable sexp, with a header comment."
  (let ((directory (file-name-directory file)))
    (unless (file-directory-p directory)
      ;; The parent directory is deliberately not created.
      (error "Directory %s does not exist" directory)))
  (let ((print-length nil)                ;never truncate a long list
        (print-level nil)
        (print-circle nil))
    (with-temp-file file
      (insert ";; gptel task list — saved "
              (format-time-string "%Y-%m-%d %H:%M")
              "\n")
      (if tasks
          (pp tasks (current-buffer))
        (insert "()\n")))))

(defun gptel-custom-tools--parse-task (form)
  "Validate FORM as a task plist and return it in canonical shape."
  (unless (and (consp form) (cl-evenp (length form)) (keywordp (car form)))
    (error "Not a task plist: %S" form))
  (let ((id (gptel-custom-tools--arg (plist-get form :id)))
        (content (gptel-custom-tools--arg (plist-get form :content)))
        (active-form (gptel-custom-tools--arg (plist-get form :active-form)))
        (status (gptel-custom-tools--arg (plist-get form :status)))
        (parent-id (gptel-custom-tools--arg (plist-get form :parent-id)))
        (note (gptel-custom-tools--arg (plist-get form :note))))
    (when (gptel-custom-tools--blank-p id)
      (error "Task with no :id: %S" form))
    (when (gptel-custom-tools--blank-p content)
      (error "Task %s has no :content" id))
    (unless (member status gptel-custom-tools--statuses)
      (error "Task %s has an unknown status: %S" id status))
    (list :id id
          :content content
          :active-form (unless (gptel-custom-tools--blank-p active-form) active-form)
          :status status
          :parent-id (unless (gptel-custom-tools--blank-p parent-id) parent-id)
          :note (unless (gptel-custom-tools--blank-p note) note))))

(defun gptel-custom-tools--read-file (file)
  "Read and validate the task list in FILE, and return it.

Signals on anything malformed, so that the caller can leave the
current task list untouched.  FILE is only ever `read'; it is never
passed to `load' or `eval'."
  (unless (file-readable-p file)
    (error "No readable file at %s" file))
  (when (file-directory-p file)
    (error "%s is a directory" file))
  (let ((form (with-temp-buffer
                (insert-file-contents file)
                (goto-char (point-min))
                ;; An empty file is an empty list, but a truncated one
                ;; is an error -- so check for real content first
                ;; rather than reading and treating end-of-file as nil.
                (skip-chars-forward " \t\n\r\f")
                (while (and (not (eobp)) (eq (char-after) ?\;))
                  (forward-line 1)
                  (skip-chars-forward " \t\n\r\f"))
                (unless (eobp)
                  (condition-case nil
                      (read (current-buffer))
                    (error
                     (error "%s does not contain a readable expression" file)))))))
    (unless (proper-list-p form)
      (error "%s does not contain a list of tasks" file))
    (let ((tasks (mapcar #'gptel-custom-tools--parse-task form))
          (seen (make-hash-table :test #'equal)))
      (dolist (task tasks)
        (let ((id (plist-get task :id)))
          (when (gethash id seen)
            (error "Duplicate task id in %s: %s" file id))
          (puthash id t seen)))
      (when-let* ((cyclic (gptel-custom-tools--cyclic-id tasks)))
        (error "Parent cycle in %s, at %s" file cyclic))
      tasks)))


;;;; Tool implementations

(defun gptel-custom-tools-create (content &optional active-form parent-id note)
  "Create a task with CONTENT and return its ID.

ACTIVE-FORM, PARENT-ID and NOTE are optional.  Backs the `TaskCreate'
tool."
  (gptel-custom-tools--with-tasks
    (let ((content (gptel-custom-tools--arg content))
          (active-form (gptel-custom-tools--arg active-form))
          (parent-id (gptel-custom-tools--arg parent-id))
          (note (gptel-custom-tools--arg note)))
      (cond
       ((gptel-custom-tools--blank-p content)
        "Error: content is required and cannot be empty.")
       ((and (not (gptel-custom-tools--blank-p parent-id))
             (not (gptel-custom-tools--find parent-id)))
        (format "Error: no task with id %s to use as parent." parent-id))
       (t
        (let ((task (list :id (gptel-custom-tools--next-id)
                          :content content
                          :active-form (unless (gptel-custom-tools--blank-p active-form)
                                         active-form)
                          :status "pending"
                          :parent-id (unless (gptel-custom-tools--blank-p parent-id)
                                       parent-id)
                          :note (unless (gptel-custom-tools--blank-p note) note))))
          (setq gptel-custom-tools--tasks
                (append gptel-custom-tools--tasks (list task)))
          (gptel-custom-tools--render)
          (format "Created task %s: %S" (plist-get task :id) content)))))))

(defun gptel-custom-tools-update (task-id &optional status content active-form
                                          parent-id note)
  "Patch the task named TASK-ID.

STATUS, CONTENT, ACTIVE-FORM, PARENT-ID and NOTE are optional; an
omitted field is left unchanged and an empty string clears it.  All
validation happens before any mutation, so a rejected call leaves the
task list untouched.  Backs the `TaskUpdate' tool."
  (gptel-custom-tools--with-tasks
    (let* ((task-id (gptel-custom-tools--arg task-id))
           (task (gptel-custom-tools--find task-id))
           (status (gptel-custom-tools--arg status))
           (content (gptel-custom-tools--arg content))
           (active-form (gptel-custom-tools--arg active-form))
           (parent-id (gptel-custom-tools--arg parent-id))
           (note (gptel-custom-tools--arg note)))
      (cond
       ((null task)
        (format "Error: no task with id %s." (or task-id "(none given)")))
       ((and status (not (member status gptel-custom-tools--statuses)))
        (format "Error: unknown status %S.  Use one of: %s."
                status (string-join gptel-custom-tools--statuses ", ")))
       ((and content (gptel-custom-tools--blank-p content))
        "Error: content cannot be cleared; a task always needs a description.")
       ((and parent-id (not (gptel-custom-tools--blank-p parent-id))
             (not (gptel-custom-tools--find parent-id)))
        (format "Error: no task with id %s to use as parent." parent-id))
       ((and parent-id (not (gptel-custom-tools--blank-p parent-id))
             (gptel-custom-tools--cycle-p task-id parent-id gptel-custom-tools--tasks))
        (format "Error: making %s a child of %s would create a cycle."
                task-id parent-id))
       (t
        (when status (plist-put task :status status))
        (when content (plist-put task :content content))
        (when active-form
          (plist-put task :active-form
                     (unless (gptel-custom-tools--blank-p active-form) active-form)))
        (when parent-id
          (plist-put task :parent-id
                     (unless (gptel-custom-tools--blank-p parent-id) parent-id)))
        (when note
          (plist-put task :note
                     (unless (gptel-custom-tools--blank-p note) note)))
        (gptel-custom-tools--render)
        (format "Updated task %s" task-id))))))

(defun gptel-custom-tools-list ()
  "Return every task as a compact, tree-indented one-liner.

Backs the `TaskList' tool."
  (gptel-custom-tools--with-tasks
    (gptel-custom-tools--format-list gptel-custom-tools--tasks)))

(defun gptel-custom-tools-get (task-id)
  "Return every field of the task named TASK-ID.

Backs the `TaskGet' tool."
  (gptel-custom-tools--with-tasks
    (let* ((task-id (gptel-custom-tools--arg task-id))
           (task (gptel-custom-tools--find task-id)))
      (if task
          (gptel-custom-tools--format-task task)
        (format "Error: no task with id %s." (or task-id "(none given)"))))))

(defun gptel-custom-tools-save (&optional path)
  "Write the task list to PATH as a readable sexp file.

Backs the `TaskSave' tool."
  (gptel-custom-tools--with-tasks
    (let ((file (gptel-custom-tools--resolve-path path)))
      (condition-case err
          (progn
            (gptel-custom-tools--write-file file gptel-custom-tools--tasks)
            (format "Saved %d tasks to %s"
                    (length gptel-custom-tools--tasks) file))
        (error (format "Error: could not save to %s: %s"
                       file (error-message-string err)))))))

(defun gptel-custom-tools-load (&optional path)
  "Replace the task list with the one stored at PATH.

The file is read, parsed and validated in full before anything is
replaced, so a failed load leaves the current list intact.  Backs the
`TaskLoad' tool."
  (gptel-custom-tools--with-tasks
    (let ((file (gptel-custom-tools--resolve-path path)))
      (condition-case err
          (let ((tasks (gptel-custom-tools--read-file file)))
            (setq gptel-custom-tools--tasks tasks
                  gptel-custom-tools--id-counter (gptel-custom-tools--max-id tasks))
            (gptel-custom-tools--render)
            (format "Loaded %d tasks from %s" (length tasks) file))
        (error (format "Error: could not load from %s: %s"
                       file (error-message-string err)))))))


;;;; Tool registration

(gptel-make-tool
 :name "TaskCreate"
 :function #'gptel-custom-tools-create
 :description "Create a new task.  Returns the task ID.  Use for each planned step in multi-step work.  Specify parent_id to create a subtask."
 :args
 '(( :name "content"
     :type string
     :minLength 1
     :description "Imperative form describing what needs to be done (e.g., 'Run tests')")
   ( :name "active_form"
     :type string
     :optional t
     :description "Present continuous form shown while the task runs (e.g., 'Running tests')")
   ( :name "parent_id"
     :type string
     :optional t
     :description "ID of an existing task to create this one as a subtask of")
   ( :name "note"
     :type string
     :optional t
     :description "Free-text context, or the reason the task is blocked"))
 :category "gptel-custom-tools"
 :confirm nil
 :async nil
 :include nil)

(gptel-make-tool
 :name "TaskUpdate"
 :function #'gptel-custom-tools-update
 :description "Update a task by ID.  Change status, content, active_form, note, or parent_id.  Use to mark tasks in_progress, completed, waiting, or canceled."
 :args
 '(( :name "task_id"
     :type string
     :minLength 1
     :description "ID of the task to update")
   ( :name "status"
     :type string
     :optional t
     :enum ["pending" "in_progress" "completed" "waiting" "canceled"]
     :description "New status: pending, in_progress, completed, waiting, or canceled")
   ( :name "content"
     :type string
     :optional t
     :description "Updated task description.  Omit to leave unchanged")
   ( :name "active_form"
     :type string
     :optional t
     :description "Updated present continuous form.  Omit to leave unchanged; pass an empty string to clear it")
   ( :name "parent_id"
     :type string
     :optional t
     :description "Updated parent task ID.  Omit to leave unchanged; pass an empty string to promote the task to top level")
   ( :name "note"
     :type string
     :optional t
     :description "Updated note or blocker reason.  Omit to leave unchanged; pass an empty string to clear it"))
 :category "gptel-custom-tools"
 :confirm nil
 :async nil
 :include nil)

(gptel-make-tool
 :name "TaskList"
 :function #'gptel-custom-tools-list
 :description "List all tasks with IDs and statuses.  Call this to review progress or re-ground after context changes."
 :args nil
 :category "gptel-custom-tools"
 :confirm nil
 :async nil
 :include nil)

(gptel-make-tool
 :name "TaskGet"
 :function #'gptel-custom-tools-get
 :description "Get full details of a single task by ID.  Use when you need the complete picture of one task."
 :args
 '(( :name "task_id"
     :type string
     :minLength 1
     :description "ID of the task to retrieve"))
 :category "gptel-custom-tools"
 :confirm nil
 :async nil
 :include nil)

(gptel-make-tool
 :name "TaskSave"
 :function #'gptel-custom-tools-save
 :description "Save the current task list to a file.  Suggest to the user when a session is ending and tasks are tracked."
 :args
 '(( :name "path"
     :type string
     :optional t
     :description "File path, absolute or relative to the buffer's working directory.  Omit to use the configured default"))
 :category "gptel-custom-tools"
 :confirm t
 :async nil
 :include nil)

(gptel-make-tool
 :name "TaskLoad"
 :function #'gptel-custom-tools-load
 :description "Load a task list from a file.  Suggest to the user when starting a session that references prior work."
 :args
 '(( :name "path"
     :type string
     :optional t
     :description "File path, absolute or relative to the buffer's working directory.  Omit to use the configured default"))
 :category "gptel-custom-tools"
 :confirm nil
 :async nil
 :include nil)

(provide 'gptel-custom-tools)
;;; gptel-custom-tools.el ends here
