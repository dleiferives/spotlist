;;; spotlist.el --- Live-tracking, navigable, and editable region list -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Dylan Thoreau Leifer-Ives

;; Author: Dylan Thoreau Leifer-Ives <dleiferives@gmail.com>
;; Maintainer: Dylan Thoreau Leifer-Ives <dleiferives@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, tools, bookmarks
;; URL: https://github.com/dleiferives/spotlist

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; SpotList provides a live-tracking, navigable list of text regions
;; from your buffers.  You can bookmark regions, edit them inline in
;; the SpotList buffer, and changes sync back to the source.
;;
;; Features:
;; - Bookmark arbitrary text regions or lines
;; - View all bookmarks in a dedicated buffer (normal or compact view)
;; - Edit bookmarked text inline with live sync to source (normal view only)
;; - Syntax highlighting preserved from source
;; - Fold/unfold entries (normal view only)
;; - Jump to original locations
;; - Undo/redo for inline edits (normal view only)
;; - Evil mode integration
;;
;; Usage:
;;   M-x spotlist-add-region    Add selected region (C-c C-s r)
;;   M-x spotlist-add-line      Add current line (C-c C-s l)
;;   M-x spotlist-show          Show SpotList buffer (C-c C-s s)
;;
;; In the SpotList buffer:
;;   C-c C-j     Jump to source location
;;   C-c C-d     Delete entry
;;   C-c C-t     Toggle fold/unfold (normal view)
;;   C-c C-a     Toggle fold all/unfold all (normal view)
;;   C-c C-v     Toggle normal/compact view
;;   C-c C-m     Adjust max dimensions for compact view
;;   C-c C-M     Adjust current entry dimensions for compact view
;;   C-c C-r     Reset current entry dimensions to global for compact view
;;   C-c C-g     Refresh
;;   C-c C-q     Quit
;;   C-z, C-/    Undo (normal view)
;;   C-y         Redo (normal view)
;;
;; Evil mode users get additional bindings:
;;   gd      Jump to source
;;   dd      Delete entry
;;   za      Toggle fold (normal view)
;;   zM/zR   Fold/unfold all (normal view)
;;   v       Toggle view
;;   m       Adjust max dimensions for compact view
;;   M       Adjust current entry dimensions for compact view
;;   R       Reset current entry dimensions to global for compact view
;;   gr      Refresh
;;   q       Quit
;;   ZZ      Quit
;;   ZQ      Quit
;;   u       Undo (normal view)
;;   C-r     Redo (normal view)

(require 'cl-lib)

;;; Customization

(defgroup spotlist nil
  "Live-tracking region list."
  :group 'convenience
  :prefix "spotlist-")

(defcustom spotlist-global-prefix "C-c C-s"
  "Global prefix key for SpotList commands."
  :type 'string
  :group 'spotlist)

(defcustom spotlist-visibility-refresh-interval 0.2
  "Seconds between automatic re-renders when the SpotList buffer is visible.
Set to nil to disable."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'spotlist)

(defcustom spotlist-typing-pause-duration 0.25
  "Seconds to wait after typing stops before resuming auto-refresh.
When typing in an editable region, the visibility refresh is paused
to avoid interrupting your edits. After you stop typing for this duration,
automatic refreshing resumes."
  :type 'number
  :group 'spotlist)

(defvar spotlist-visibility-refresh-timer nil
  "Timer used to periodically re-render SpotList when it is visible.")

;;; Data structures

(cl-defstruct spotlist-entry
  "Structure representing a saved region."
  id                ; Numeric identifier
  file              ; File path (if any)
  buf               ; Original buffer
  start-marker      ; Start marker
  end-marker        ; End marker
  text              ; Cached text with properties
  folded            ; Whether entry is folded
  body-overlay      ; Overlay marking the editable body in SpotList (normal view)
  timestamp         ; Creation time
  custom-width      ; Custom width for this entry (nil = use global default)
  custom-height)    ; Custom height for this entry (nil = use global default)

;;; Variables

(defvar spotlist-entries nil
  "List of all SpotList entries.")

(defvar spotlist-next-id 1
  "Next entry ID to assign.")

(defvar spotlist-buffer-name "*SpotList*"
  "Name of the SpotList buffer.")

(defvar spotlist-refresh-timer nil
  "Idle timer for refreshing the SpotList.")

(defvar spotlist-auto-refresh-timer nil
  "Timer for periodic auto-refresh of SpotList display.")

(defvar spotlist-dirty-buffers nil
  "Buffers that have been modified and need refresh.")

(defvar spotlist-inhibit-hooks nil
  "When non-nil, suppress change hooks.")

(defvar-local spotlist-in-editable-region nil
  "Non-nil when point is in an editable region.")

(defvar-local spotlist-undo-in-progress nil
  "Non-nil when an undo operation is in progress.")

(defvar spotlist-edit-history nil
  "History of edits for undo/redo.")

(defvar spotlist-edit-history-position 0
  "Current position in edit history.")

(defvar-local spotlist-last-point nil
  "Last known cursor position.")

(defvar-local spotlist-last-change-tick nil
  "Last buffer modification tick we saw.")

(defvar spotlist-last-typing-time nil
  "Time of last typing activity in an editable region.")

(defvar spotlist-typing-resume-timer nil
  "Timer to resume visibility refresh after typing stops.")

(defvar spotlist-view-mode 'normal
  "Current view mode: 'normal or 'compact.")

;;; Color adjustment helpers

(defun spotlist-color-to-rgb (color)
  "Convert COLOR to RGB values (0.0-1.0)."
  (let ((values (color-values color)))
    (list (/ (nth 0 values) 65535.0)
          (/ (nth 1 values) 65535.0)
          (/ (nth 2 values) 65535.0))))

(defun spotlist-rgb-to-hex (r g b)
  "Convert RGB values (0.0-1.0) to hex color string."
  (format "#%02x%02x%02x"
          (round (* r 255))
          (round (* g 255))
          (round (* b 255))))

(defun spotlist-theme-is-dark-p ()
  "Return t if current theme appears to be dark."
  (let* ((bg (or (face-background 'default nil t) "#ffffff"))
         (rgb (spotlist-color-to-rgb bg))
         (luminance (+ (* 0.299 (nth 0 rgb))
                       (* 0.587 (nth 1 rgb))
                       (* 0.114 (nth 2 rgb)))))
    (< luminance 0.5)))

(defun spotlist-get-editable-background ()
  "Get background color for editable regions.
10% brighter on dark themes, 10% darker on light themes."
  (let* ((bg (or (face-background 'default nil t) "#ffffff"))
         (rgb (spotlist-color-to-rgb bg))
         (dark-theme (spotlist-theme-is-dark-p))
         (factor (if dark-theme 1.01 0.99))
         (adjusted-rgb (mapcar (lambda (v)
                                  (max 0.0 (min 1.0 (* v factor))))
                               rgb)))
    (apply #'spotlist-rgb-to-hex adjusted-rgb)))

;;; Global keymap

(defvar spotlist-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" 'spotlist-add-region)
    (define-key map "l" 'spotlist-add-line)
    (define-key map "s" 'spotlist-show)
    (define-key map "c" 'spotlist-clear-all)
    map)
  "Keymap for SpotList commands.")

;;;###autoload
(with-eval-after-load 'spotlist
(define-key global-map (kbd "C-c C-s") spotlist-command-map))

;;; Mode definition

(defvar spotlist-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Only bind keys that won't interfere with normal typing
    (define-key map (kbd "C-c C-j") 'spotlist-jump)
    (define-key map (kbd "C-c C-d") 'spotlist-delete)
    (define-key map (kbd "C-c C-g") 'spotlist-refresh)
    (define-key map (kbd "C-c C-q") 'quit-window)
    (define-key map (kbd "C-c C-t") 'spotlist-toggle-fold)
    (define-key map (kbd "C-c C-a") 'spotlist-toggle-fold-all)
    (define-key map (kbd "C-c C-v") 'spotlist-toggle-view) ; New view toggle
    (define-key map (kbd "C-c C-m") 'spotlist-adjust-compact-dimensions) ; Adjust global compact dimensions
    (define-key map (kbd "C-c C-M") 'spotlist-adjust-entry-dimensions) ; Adjust current entry dimensions
    (define-key map (kbd "C-c C-r") 'spotlist-reset-entry-dimensions) ; Reset current entry dimensions
    (define-key map (kbd "m") 'spotlist-adjust-compact-dimensions) ; For convenience
    (define-key map (kbd "M") 'spotlist-adjust-entry-dimensions) ; For convenience
    (define-key map (kbd "R") 'spotlist-reset-entry-dimensions) ; For convenience
    ;; Undo and Redo keys
    (define-key map (kbd "C-/") 'spotlist-undo)
    (define-key map (kbd "C-?") 'spotlist-redo) ; Shifted C-/ on many systems
    (define-key map (kbd "C-z") 'spotlist-undo)
    (define-key map (kbd "C-y") 'spotlist-redo) ; For users familiar with Windows shortcuts
    map)
  "Keymap for SpotList mode.")

(define-derived-mode spotlist-mode fundamental-mode "SpotList"
  "Major mode for SpotList buffer.
\\{spotlist-mode-map}"
  (setq buffer-read-only nil)
  (add-hook 'after-change-functions #'spotlist-after-change nil t)
  (add-hook 'post-command-hook #'spotlist-post-command nil t)
  (add-hook 'before-change-functions #'spotlist-before-change nil t)
  (when (and (fboundp 'evil-mode) (boundp 'evil-mode) evil-mode)
    (spotlist-setup-evil))
  ;; Start auto-refresh timer when entering mode
  (spotlist-start-visibility-refresh))

;;; Custom undo/redo implementation

(defun spotlist-snapshot-state ()
  "Take a snapshot of current SpotList state."
  (mapcar (lambda (entry)
            (let ((ov (spotlist-entry-body-overlay entry)))
              (cons (spotlist-entry-id entry)
                    (when (and ov (overlay-buffer ov))
                      (buffer-substring-no-properties
                       (overlay-start ov)
                       (overlay-end ov))))))
          spotlist-entries))

(defun spotlist-restore-state (state saved-point)
  "Restore SpotList state from STATE and move to SAVED-POINT."
  (let ((spotlist-inhibit-hooks t)
        (spotlist-undo-in-progress t))
    (dolist (entry-state state)
      (let* ((id (car entry-state))
             (text (cdr entry-state))
             (entry (cl-find id spotlist-entries :key #'spotlist-entry-id)))
        (when (and entry text)
          (let ((ov (spotlist-entry-body-overlay entry)))
            (when (and ov (overlay-buffer ov))
              (save-excursion
                (goto-char (overlay-start ov))
                (delete-region (overlay-start ov) (overlay-end ov))
                (insert text))
              ;; Update overlay end after insertion
              (move-overlay ov (overlay-start ov) (+ (overlay-start ov) (length text)))
              ;; Sync back to source buffer
              (spotlist-sync-to-source entry))))))
    (goto-char (min saved-point (point-max)))))

(defun spotlist-push-undo-state ()
  "Push current state to undo history."
  (let ((current-state (spotlist-snapshot-state))
        (current-point (point)))
    ;; Truncate history if we're not at the top
    (when (> spotlist-edit-history-position 0)
      (setq spotlist-edit-history
            (nthcdr spotlist-edit-history-position spotlist-edit-history))
      (setq spotlist-edit-history-position 0))
    ;; Push new state
    (push (cons current-point current-state) spotlist-edit-history)
    ;; Limit history size
    (when (> (length spotlist-edit-history) 100)
      (setq spotlist-edit-history (butlast spotlist-edit-history 1)))))

(defun spotlist-undo ()
  "Undo the last edit in SpotList buffer."
  (interactive)
  (if (eq spotlist-view-mode 'compact)
      (message "Undo is not available in compact view.")
    (if (> (length spotlist-edit-history) (1+ spotlist-edit-history-position))
        (progn
          (setq spotlist-edit-history-position
                (1+ spotlist-edit-history-position))
          (let* ((state-with-point (nth spotlist-edit-history-position
                                        spotlist-edit-history))
                 (saved-point (car state-with-point))
                 (state (cdr state-with-point)))
            (spotlist-restore-state state saved-point)
            (message "Undo!")))
      (message "No further undo information"))))

(defun spotlist-redo ()
  "Redo the last undone edit in SpotList buffer."
  (interactive)
  (if (eq spotlist-view-mode 'compact)
      (message "Redo is not available in compact view.")
    (if (> spotlist-edit-history-position 0)
        (progn
          (setq spotlist-edit-history-position
                (1- spotlist-edit-history-position))
          (let* ((state-with-point (nth spotlist-edit-history-position
                                        spotlist-edit-history))
                 (saved-point (car state-with-point))
                 (state (cdr state-with-point)))
            (spotlist-restore-state state saved-point)
            (message "Redo!")))
      (message "No further redo information"))))

;;; Evil integration

(defun spotlist-setup-evil ()
  "Setup Evil mode bindings for SpotList."
  (evil-set-initial-state 'spotlist-mode 'normal)

  ;; These only work in NORMAL mode - won't interfere with insert mode
  (evil-define-key 'normal spotlist-mode-map
    (kbd "gd") 'spotlist-jump
    "j" 'evil-next-line
    "k" 'evil-previous-line
    "gg" 'evil-goto-first-line
    "G" 'evil-goto-line
    "za" 'spotlist-toggle-fold
    "zM" 'spotlist-fold-all
    "zR" 'spotlist-unfold-all
    "v" 'spotlist-toggle-view          ; Added for view toggle
    "m" 'spotlist-adjust-compact-dimensions ; Adjust global compact dimensions
    "M" 'spotlist-adjust-entry-dimensions ; Adjust current entry dimensions
    "R" 'spotlist-reset-entry-dimensions ; Reset current entry dimensions
    "gr" 'spotlist-refresh
    "q" 'quit-window
    "ZZ" 'quit-window
    "ZQ" 'quit-window
    "u" 'spotlist-undo
    (kbd "C-r") 'spotlist-redo)

  ;; Visual mode
  (evil-define-key 'visual spotlist-mode-map
    (kbd "RET") 'spotlist-jump)

  ;; Make sure insert mode can exit with ESC
  (evil-define-key 'insert spotlist-mode-map
    (kbd "ESC") 'evil-normal-state
    (kbd "C-c C-c") 'evil-normal-state))

(defun spotlist-fold-all ()
  "Fold all entries."
  (interactive)
  (if (eq spotlist-view-mode 'compact)
      (message "Fold/unfold is not available in compact view.")
    (when (spotlist-check-not-editing)
      (dolist (entry spotlist-entries)
        (setf (spotlist-entry-folded entry) t))
      (spotlist-render))))

(defun spotlist-unfold-all ()
  "Unfold all entries."
  (interactive)
  (if (eq spotlist-view-mode 'compact)
      (message "Fold/unfold is not available in compact view.")
    (when (spotlist-check-not-editing)
      (dolist (entry spotlist-entries)
        (setf (spotlist-entry-folded entry) nil))
      (spotlist-render))))

(defun spotlist-check-not-editing ()
  "Check if we're not currently editing. Return t if safe to proceed."
  (if (eq spotlist-view-mode 'compact)
      t ; Always safe in compact view as it's read-only
    (if spotlist-in-editable-region
        (progn
          (message "Exit insert mode first (ESC in Evil, or move cursor away)")
          nil)
      t)))

;;; Core functionality - Adding entries

(defun spotlist-fontify-region-if-needed (buf start end)
  "Ensure region from START to END in BUF is fontified."
  (with-current-buffer buf
    ;; Ensure font-lock is enabled
    (unless font-lock-mode
      (font-lock-mode 1))
    ;; Force fontification of the region
    (when (fboundp 'font-lock-ensure)
      (font-lock-ensure start end))
    ;; Fallback for older Emacs
    (when (and (not (fboundp 'font-lock-ensure))
               (fboundp 'font-lock-fontify-region))
      (font-lock-fontify-region start end))))

;;;###autoload
(defun spotlist-add-region (start end)
  "Add region from START to END to SpotList."
  (interactive "r")
  (unless (or (use-region-p)
              (and (fboundp 'evil-visual-state-p) (evil-visual-state-p)))
    (user-error "No active region"))
  (when (and (fboundp 'evil-visual-state-p) (evil-visual-state-p))
    (setq start (region-beginning)
          end (region-end))
    (when (fboundp 'evil-normal-state)
      (evil-normal-state)))
  (let* ((buf (current-buffer))
         (file (buffer-file-name buf))
         (start-marker (copy-marker start))
         (end-marker (copy-marker end t)))
    ;; Fontify before capturing
    (spotlist-fontify-region-if-needed buf start end)
    (let* ((text (buffer-substring start end))  ; Use buffer-substring to preserve properties
           (entry (make-spotlist-entry
                   :id spotlist-next-id
                   :file file
                   :buf buf
                   :start-marker start-marker
                   :end-marker end-marker
                   :text text
                   :folded nil
                   :timestamp (current-time)
                   :custom-width nil    ; Initialize with nil for global default
                   :custom-height nil))) ; Initialize with nil for global default
      (setq spotlist-next-id (1+ spotlist-next-id))
      (push entry spotlist-entries)
      (spotlist-install-source-hook buf)
      (spotlist-show-without-switch)
      (message "Added region to SpotList (ID: %d)" (spotlist-entry-id entry)))))

;;;###autoload
(defun spotlist-add-line ()
  "Add current line to SpotList."
  (interactive)
  (save-excursion
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (spotlist-add-region start end))))

;;; Source buffer tracking

(defun spotlist-install-source-hook (buf)
  "Install change tracking hook in source buffer BUF."
  (with-current-buffer buf
    (unless (memq 'spotlist-source-changed after-change-functions)
      (add-hook 'after-change-functions #'spotlist-source-changed nil t))
    (add-hook 'kill-buffer-hook #'spotlist-source-killed nil t)))

(defun spotlist-source-changed (&rest _)
  "Mark current buffer as dirty and schedule refresh."
  (unless spotlist-inhibit-hooks
    (add-to-list 'spotlist-dirty-buffers (current-buffer))
    (spotlist-schedule-refresh)))

(defun spotlist-source-killed ()
  "Handle source buffer being killed."
  (let ((buf (current-buffer)))
    (setq spotlist-entries
          (cl-remove-if (lambda (entry)
                          (eq (spotlist-entry-buf entry) buf))
                        spotlist-entries)))
  (when (get-buffer spotlist-buffer-name)
    (spotlist-refresh)))

(defun spotlist-schedule-refresh ()
  "Schedule an idle refresh of the SpotList buffer."
  (when spotlist-refresh-timer
    (cancel-timer spotlist-refresh-timer))
  (setq spotlist-refresh-timer
        (run-with-idle-timer 0.05 nil #'spotlist-auto-refresh)))

(defun spotlist-auto-refresh ()
  "Auto-refresh handler for dirty buffers."
  (when (get-buffer spotlist-buffer-name)
    (with-current-buffer spotlist-buffer-name
      (let ((spotlist-inhibit-hooks t))
        (spotlist-refresh))))
  (when spotlist-dirty-buffers
    (dolist (entry spotlist-entries)
      (when (and (memq (spotlist-entry-buf entry) spotlist-dirty-buffers)
                 (buffer-live-p (spotlist-entry-buf entry)))
        (spotlist-recompute-entry-text entry)))
    (setq spotlist-dirty-buffers nil)
    (when (get-buffer spotlist-buffer-name)
      (with-current-buffer spotlist-buffer-name
        (let ((spotlist-inhibit-hooks t))
          (spotlist-render))))))

(defun spotlist-recompute-entry-text (entry)
  "Recompute text for ENTRY from its markers."
  (when (and (buffer-live-p (spotlist-entry-buf entry))
             (marker-buffer (spotlist-entry-start-marker entry)))
    (with-current-buffer (spotlist-entry-buf entry)
      (let ((start (spotlist-entry-start-marker entry))
            (end (spotlist-entry-end-marker entry)))
        (when (and start end (marker-position start) (marker-position end))
          ;; Fontify before capturing
          (spotlist-fontify-region-if-needed (current-buffer)
                                            (marker-position start)
                                            (marker-position end))
          (setf (spotlist-entry-text entry)
                (buffer-substring start end)))))))  ; Preserve properties

;;; SpotList buffer display

;;;###autoload
(defun spotlist-show ()
  "Show the SpotList buffer."
  (interactive)
  (let ((buf (get-buffer-create spotlist-buffer-name)))
    (spotlist-start-visibility-refresh)
    (with-current-buffer buf
      (unless (eq major-mode 'spotlist-mode)
        (spotlist-mode))
      (spotlist-render)
      ;; Initialize undo history with current state
      (when (null spotlist-edit-history)
        (setq spotlist-edit-history (list (cons (point) (spotlist-snapshot-state))))
        (setq spotlist-edit-history-position 0)))
    (pop-to-buffer buf)))


(defun spotlist-show-without-switch ()
  "Show the SpotList buffer without switching to it."
  (interactive)
  (let ((buf (get-buffer-create spotlist-buffer-name)))
    (spotlist-start-visibility-refresh)
    (with-current-buffer buf
      (unless (eq major-mode 'spotlist-mode)
        (spotlist-mode))
      (spotlist-render)
      ;; Initialize undo history with current state
      (when (null spotlist-edit-history)
        (setq spotlist-edit-history (list (cons (point) (spotlist-snapshot-state))))
        (setq spotlist-edit-history-position 0)))
    (display-buffer buf)))

(defun spotlist-render ()
  "Render the SpotList buffer content, dispatching based on view mode."
  (if (eq spotlist-view-mode 'compact)
      (spotlist-render-compact)
    (spotlist-render-normal)))

(defun spotlist-render-normal ()
  "Render the SpotList buffer content in normal mode."
  (let ((old-point (point))
        (spotlist-inhibit-hooks t))

    ;; Clear old overlays
    (remove-overlays (point-min) (point-max) 'spotlist-protected t)
    (dolist (entry spotlist-entries)
      (when (spotlist-entry-body-overlay entry)
        (delete-overlay (spotlist-entry-body-overlay entry))
        (setf (spotlist-entry-body-overlay entry) nil)))

    (let ((buffer-read-only nil))
      (erase-buffer)

      ;; Header
      (let ((start (point)))
        ;; Use 'default for general text, 'bold for the title
        (insert (propertize "SpotList\n" 'face 'bold))
        (if (and (fboundp 'evil-mode) (boundp 'evil-mode) evil-mode)
            ;; Use 'font-lock-constant-face for documentation-like text
            (insert (propertize "RET/gd:jump dd:delete za:fold zM/zR:fold-all v:toggle-view M:adjust-entry m:adjust-global R:reset gr:refresh q:quit | i/a/c:edit inline\n"
                                'face 'font-lock-constant-face)
                    (propertize "u:undo C-r:redo\n\n"
                                'face 'font-lock-constant-face))
          (insert (propertize "C-c C-j:jump C-c C-d:delete C-c C-t:fold C-c C-a:fold-all C-c C-v:toggle-view C-c C-M:adjust-entry C-c C-m:adjust-global C-c C-r:reset C-c C-g:refresh C-c C-q:quit\n"
                              'face 'font-lock-constant-face)
                  (propertize "C-z/C-/:undo C-y:redo\n\n"
                              'face 'font-lock-constant-face)))
        (spotlist-make-protected start (point)))

      (if (null spotlist-entries)
          (let ((start (point)))
            ;; Use 'font-lock-constant-face for informational messages
            (insert (propertize "No entries. Add with: C-c C-s r (region) or C-c C-s l (line)\n"
                                'face 'font-lock-constant-face))
            (spotlist-make-protected start (point)))
        (dolist (entry (reverse spotlist-entries))
          (spotlist-render-entry entry))))

    (goto-char (min old-point (point-max)))))

(defun spotlist-make-protected (start end)
  "Make region from START to END protected from editing."
  (let* ((bg (face-background 'default nil t))
         (is-dark (spotlist-theme-is-dark-p))
         (ov (make-overlay start end)))
    (overlay-put ov 'spotlist-protected t)
    (overlay-put ov 'read-only t)
    (overlay-put ov 'evaporate t)))

(defun spotlist-render-entry (entry)
  "Render ENTRY in the current buffer."
  (let* ((id (spotlist-entry-id entry))
         (buf (spotlist-entry-buf entry))
         (file (spotlist-entry-file entry))
         (buf-name (if (buffer-live-p buf)
                       (buffer-name buf)
                     (if file (file-name-nondirectory file) "<killed>")))
         (text (spotlist-entry-text entry))
         (folded (spotlist-entry-folded entry)))

    ;; Header line (protected)
    (let ((header-start (point)))
      (insert (propertize (format "[%d] %s (%d chars)\n"
                                  id buf-name (length (substring-no-properties text)))
                          'face 'font-lock-warning-face
                          'spotlist-entry-id id))
      (spotlist-make-protected header-start (point)))

    ;; Body
    (let ((body-start (point)))
      (if folded
          (progn
            (let ((first-line (car (split-string (or text "") "\n" t))) ; Preserve properties
                  (has-more (string-match-p "\n" (or text ""))))
              (insert first-line)
              (when has-more
                (insert (propertize " …" 'face 'shadow))))
            (spotlist-make-protected body-start (point)))
        ;; Unfolded - EDITABLE with visual indicator
        (let ((edit-start (point)))
          (insert text)  ; This now includes text properties (colors)
          (let ((edit-end (point)))
            ;; Create overlay for editable region with visual cue
            (let ((ov (make-overlay edit-start edit-end)))
              (overlay-put ov 'spotlist-entry-id id)
              (overlay-put ov 'spotlist-body t)
              (overlay-put ov 'face `(:background ,(spotlist-get-editable-background) :extend t))
              (setf (spotlist-entry-body-overlay entry) ov)))))

      (let ((body-end (point)))
        (when (not folded)
          (put-text-property body-start body-end 'spotlist-editable t))))

    ;; Separator (protected)
    (let ((sep-start (point)))
      (insert "\n\n")
      (spotlist-make-protected sep-start (point)))))

;;; Post-command hook - track if we're in editable region

(defun spotlist-post-command ()
  "Update whether point is in an editable region."
  (setq spotlist-in-editable-region
        (and (eq spotlist-view-mode 'normal) ; Only editable in normal mode
             (spotlist-find-entry-at-pos (point))
             (not (spotlist-in-protected-region-p (point))))))

(defun spotlist-in-protected-region-p (pos)
  "Check if POS is in a protected region."
  (let ((overlays (overlays-at pos)))
    (cl-some (lambda (ov) (overlay-get ov 'spotlist-protected))
             overlays)))

;;; Change hooks for inline editing

(defun spotlist-before-change (beg end)
  "Before change hook - save state before edit."
  (when (and (eq spotlist-view-mode 'normal) ; Only react in normal mode
             (not spotlist-inhibit-hooks)
             (not spotlist-undo-in-progress)
             (spotlist-find-entry-at-pos beg))
    ;; This is a real edit in an editable region - save state!
    (spotlist-push-undo-state)))

(defun spotlist-after-change (beg end old-len)
  "After change hook - sync edits to source buffer."
  (unless (or spotlist-inhibit-hooks spotlist-undo-in-progress
              (eq spotlist-view-mode 'compact)) ; Don't react in compact view
    ;; Find which entry was edited
    (let ((entry (spotlist-find-entry-at-pos beg)))
      (when entry
        ;; Record typing time
        (setq spotlist-last-typing-time (current-time))

        ;; Cancel and reschedule the resume timer
        (when spotlist-typing-resume-timer
          (cancel-timer spotlist-typing-resume-timer))
        (setq spotlist-typing-resume-timer
              (run-with-timer spotlist-typing-pause-duration nil
                              #'spotlist-clear-typing-state))

        ;; Sync to source
        (spotlist-sync-to-source entry)))))

(defun spotlist-clear-typing-state ()
  "Clear typing state and allow refresh to resume."
  (setq spotlist-last-typing-time nil)
  (when spotlist-typing-resume-timer
    (cancel-timer spotlist-typing-resume-timer)
    (setq spotlist-typing-resume-timer nil)))

(defun spotlist-find-entry-at-pos (pos)
  "Find entry whose body overlay contains POS."
  (cl-find-if (lambda (entry)
                (let ((ov (spotlist-entry-body-overlay entry)))
                  (and ov
                       (overlay-buffer ov)
                       (<= (overlay-start ov) pos)
                       (>= (overlay-end ov) pos))))
              spotlist-entries))

(defun spotlist-sync-to-source (entry)
  "Sync ENTRY's current text in SpotList back to source buffer."
  (let ((ov (spotlist-entry-body-overlay entry)))
    (when (and ov (overlay-buffer ov))
      (let* ((new-text (buffer-substring
                        (overlay-start ov)
                        (overlay-end ov)))
             (buf (spotlist-entry-buf entry))
             (start-marker (spotlist-entry-start-marker entry))
             (end-marker (spotlist-entry-end-marker entry)))

        (when (and (buffer-live-p buf)
                   (marker-buffer start-marker)
                   (marker-buffer end-marker))
          ;; Update source buffer
          (with-current-buffer buf
            (let ((spotlist-inhibit-hooks t))
              (save-excursion
                (goto-char (marker-position start-marker))
                (delete-region (marker-position start-marker)
                              (marker-position end-marker))
                (insert new-text))))

          ;; Schedule refresh to update text with properties
          (run-with-timer 0.1 nil
                         (lambda ()
                           (when (buffer-live-p buf)
                             (with-current-buffer buf)
                             (spotlist-recompute-entry-text entry)))))))))

;;; Navigation and interaction

(defun spotlist-jump ()
  "Jump to the original location of the entry at point."
  (interactive)
  (let* ((entry (spotlist-entry-near-point))
         (buf (and entry (spotlist-entry-buf entry)))
         (file (and entry (spotlist-entry-file entry)))
         (marker (and entry (spotlist-entry-start-marker entry))))
    (unless entry
      (user-error "No entry at point"))
    (cond
     ((and (buffer-live-p buf) (marker-buffer marker))
      (pop-to-buffer buf)
      (goto-char marker)
      (recenter))
     ((and file (file-exists-p file))
      (find-file file)
      (message "Buffer was killed; opened file"))
     (t
      (user-error "Cannot jump: buffer killed and no file available")))))

(defun spotlist-delete ()
  "Delete the entry at point."
  (interactive)
  (let ((entry (spotlist-entry-near-point)))
    (unless entry
      (user-error "No entry at point"))
    (when (spotlist-entry-start-marker entry)
      (set-marker (spotlist-entry-start-marker entry) nil))
    (when (spotlist-entry-end-marker entry)
      (set-marker (spotlist-entry-end-marker entry) nil))
    (when (spotlist-entry-body-overlay entry)
      (delete-overlay (spotlist-entry-body-overlay entry)))
    (setq spotlist-entries (delq entry spotlist-entries))
    (let ((spotlist-inhibit-hooks t))
      (spotlist-render))
    (message "Deleted entry %d" (spotlist-entry-id entry))))

(defun spotlist-refresh ()
  "Refresh the SpotList buffer."
  (interactive)
  (dolist (entry spotlist-entries)
    (spotlist-recompute-entry-text entry))
  (let ((spotlist-inhibit-hooks t))
    (spotlist-render)))

(defun spotlist--buffer-visible-p ()
  "Return non-nil if the SpotList buffer is visible in any window (any frame)."
  (let* ((buf (get-buffer spotlist-buffer-name)))
    (and buf (get-buffer-window buf t))))

(defun spotlist-is-typing-p ()
  "Return non-nil if user is currently typing in an editable region."
  (and (eq spotlist-view-mode 'normal) ; Only typing in normal mode
       spotlist-last-typing-time
       spotlist-in-editable-region
       (< (float-time (time-since spotlist-last-typing-time))
          spotlist-typing-pause-duration)))

(defun spotlist--safe-rerender ()
  "Safely re-render SpotList from cached entry text only."
  (when (get-buffer spotlist-buffer-name)
    (with-current-buffer spotlist-buffer-name
      ;; Don't require spotlist-mode, but if present, prefer it.
      (let ((inhibit-read-only t)
            (spotlist-inhibit-hooks t))
        (condition-case err
            (progn
              ;; Only re-render (no recompute from source)
              (spotlist-render))
          (error
           (message "SpotList refresh error: %S" err)))))))


(defun spotlist-start-visibility-refresh ()
  "Start the periodic visibility-based re-render timer."
  (when spotlist-visibility-refresh-timer
    (cancel-timer spotlist-visibility-refresh-timer)
    (setq spotlist-visibility-refresh-timer nil))
  (when spotlist-visibility-refresh-interval
    (setq spotlist-visibility-refresh-timer
          (run-with-timer
           spotlist-visibility-refresh-interval
           spotlist-visibility-refresh-interval
           (lambda ()
             ;; Only refresh if buffer is visible AND user is not actively typing
             (when (and (spotlist--buffer-visible-p)
                       (not (spotlist-is-typing-p)))
               (spotlist--safe-rerender)))))))

(defun spotlist-stop-visibility-refresh ()
  "Stop the visibility-based refresh timer."
  (when spotlist-visibility-refresh-timer
    (cancel-timer spotlist-visibility-refresh-timer)
    (setq spotlist-visibility-refresh-timer nil))
  ;; Also clean up typing timers
  (when spotlist-typing-resume-timer
    (cancel-timer spotlist-typing-resume-timer)
    (setq spotlist-typing-resume-timer nil))
  (setq spotlist-last-typing-time nil))

(defun spotlist-toggle-fold ()
  "Toggle fold state of entry at point."
  (interactive)
  (if (eq spotlist-view-mode 'compact)
      (message "Fold/unfold is not available in compact view.")
    (let ((entry (spotlist-entry-near-point)))
      (unless entry
        (user-error "No entry at point"))
      (setf (spotlist-entry-folded entry)
            (not (spotlist-entry-folded entry)))
      (let ((spotlist-inhibit-hooks t))
        (spotlist-render)))))

(defun spotlist-toggle-fold-all ()
  "Toggle fold state of all entries."
  (interactive)
  (if (eq spotlist-view-mode 'compact)
      (message "Fold/unfold is not available in compact view.")
    (let ((any-unfolded (cl-some (lambda (e) (not (spotlist-entry-folded e)))
                                 spotlist-entries)))
      (dolist (entry spotlist-entries)
        (setf (spotlist-entry-folded entry) any-unfolded))
      (let ((spotlist-inhibit-hooks t))
        (spotlist-render)))))

;;; Compact/Tiled View

(defcustom spotlist-compact-max-entry-width 50
  "Maximum width for an entry in compact view."
  :type 'integer
  :group 'spotlist)

(defcustom spotlist-compact-max-entry-height 6
  "Maximum height for an entry in compact view (in lines)."
  :type 'integer
  :group 'spotlist)

(cl-defstruct spotlist-layout-box
  "Structure representing a positioned entry in the layout."
  entry        ; The spotlist-entry
  x            ; Column position
  y            ; Row position (in lines)
  width        ; Width in characters
  height       ; Height in lines
  text-lines)  ; Pre-truncated text lines

(cl-defstruct spotlist-layout
  "Structure representing the complete layout solution."
  boxes        ; List of spotlist-layout-box
  total-width  ; Total width needed
  total-height) ; Total height needed

;;; Layout Solver

(defun spotlist-expand-tabs (text &optional tab-width)
  "Expand tabs in TEXT to spaces using TAB-WIDTH (default 4).
Preserves text properties."
  (let ((tw (or tab-width 4)))
    (with-temp-buffer
      (insert text)
      (untabify (point-min) (point-max))
      (buffer-substring (point-min) (point-max)))))

(defun spotlist-dedent-text (text)
  "Remove common leading whitespace from all lines in TEXT.
Similar to Python's textwrap.dedent. Preserves empty lines and text properties."
  (setq text (or text ""))
  (let* ((lines (split-string text "\n" t))  ; Split but keep properties
         (non-empty-lines (cl-remove-if
                           (lambda (line) (string-match-p "\\`[ \t]*\\'" (or line "")))
                           lines)))
    (if (null non-empty-lines)
        text
      (let ((min-indent nil))
        ;; Find minimum indent
        (dolist (line non-empty-lines)
          (when (string-match "\\`[ \t]*" line)
            (let ((indent (length (match-string 0 line))))
              (setq min-indent (if (numberp min-indent)
                                   (min min-indent indent)
                                 indent)))))
        ;; Remove indent from all lines, preserving properties
        (if (and (numberp min-indent) (> min-indent 0))
            (with-temp-buffer
              (insert text)
              (goto-char (point-min))
              (while (not (eobp))
                (when (>= (- (line-end-position) (line-beginning-position)) min-indent)
                  (delete-char min-indent))
                (forward-line 1))
              (buffer-substring (point-min) (point-max)))
          text)))))

(defun spotlist-calculate-entry-dimensions (text max-width max-height)
  "Return cons of (width . height) for TEXT, within MAX-WIDTH/HEIGHT.
TEXT should be dedented already."
  (setq text (or text "")) ;; ensure string
  (let* ((lines (split-string text "\n" nil))
         (actual-height (min (length lines) (max 0 max-height)))
         (actual-width 0))
    (dotimes (i actual-height)
      (let* ((l (or (nth i lines) ""))
             (len (length (substring-no-properties l)))) ; Use display length
        (setq actual-width (max actual-width len))))
    (cons (max 15 (+ actual-width 4))
          (max 3 (+ actual-height 2)))))

(defun spotlist-solve-compact-layout (entries window-width)
  "Shelf-pack ENTRIES for WINDOW-WIDTH."
  (let* ((gap 1)
         (boxes nil)
         (shelf-x 0)
         (shelf-y 0)
         (shelf-height 0)
         (total-width 0)
         (total-height 0)
         (win-w (max 20 (or window-width 80))))
    (dolist (entry (reverse entries))
      (let* ((max-width (or (spotlist-entry-custom-width entry)
                           spotlist-compact-max-entry-width
                           50))
             (max-height (or (spotlist-entry-custom-height entry)
                            spotlist-compact-max-entry-height
                            6))
             (text (or (spotlist-entry-text entry) ""))
             ;; Keep properties through the entire pipeline
             (expanded (spotlist-expand-tabs text))
             (dedented (spotlist-dedent-text expanded))
             (dims (spotlist-calculate-entry-dimensions
                    (substring-no-properties dedented) max-width max-height)) ; Only strip for measuring
             (box-w (car dims))
             (box-h (cdr dims))
             (tlines (spotlist-truncate-text dedented (max 0 (- box-w 4))
                                             (max 0 (- box-h 2)))))
        (when (and (> shelf-x 0)
                   (> (+ shelf-x gap box-w) win-w))
          (setq shelf-y (+ shelf-y shelf-height gap)
                shelf-x 0
                shelf-height 0))
        (push (make-spotlist-layout-box
               :entry entry :x shelf-x :y shelf-y
               :width box-w :height box-h :text-lines tlines)
              boxes)
        (setq shelf-height (max shelf-height box-h)
              total-width (max total-width (+ shelf-x box-w))
              total-height (max total-height (+ shelf-y box-h))
              shelf-x (+ shelf-x box-w gap))))
    (make-spotlist-layout
     :boxes (nreverse boxes)
     :total-width total-width
     :total-height total-height)))

(defun spotlist-truncate-text (text max-width max-height)
  "Truncate TEXT to fit within MAX-WIDTH columns and MAX-HEIGHT lines.
Returns a list of strings (lines) with text properties preserved."
  (setq text (or text ""))
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((result nil)
          (line-count 0))
      (catch 'done
        (while (not (eobp))
          (when (>= line-count max-height)
            (throw 'done nil))

          (let* ((line-start (point))
                 (line-end (line-end-position))
                 (line-text (buffer-substring line-start line-end))
                 (line-length (length (substring-no-properties line-text))))

            (cond
             ;; Line fits completely
             ((<= line-length max-width)
              (push line-text result)
              (setq line-count (1+ line-count))
              (forward-line 1))

             ;; Line needs truncation
             (t
              ;; Truncate to max-width characters (counting actual chars, not display width)
              (let ((truncated-end (min (+ line-start max-width) line-end)))
                (push (buffer-substring line-start truncated-end) result)
                (setq line-count (1+ line-count)))
              ;; Skip rest of line if we've hit max height, otherwise continue to next line
              (if (>= line-count max-height)
                  (throw 'done nil)
                (forward-line 1)))))))

      (setq result (nreverse result))

      ;; Add ellipsis if we truncated (either height or width)
      (when (or (not (eobp))
                (and (> (length result) 0)
                     (let* ((last-line (nth (1- (length result)) result))
                            (orig-line-at-last (progn
                                                 (goto-char (point-min))
                                                 (forward-line (1- (length result)))
                                                 (buffer-substring (point) (line-end-position)))))
                       (< (length (substring-no-properties last-line))
                          (length (substring-no-properties orig-line-at-last))))))
        (when (> (length result) 0)
          (let* ((last-idx (1- (length result)))
                 (last-line (nth last-idx result))
                 (last-line-plain (substring-no-properties last-line))
                 ;; Ensure we have room for ellipsis
                 (trim-to (max 0 (min (1- max-width) (1- (length last-line))))))
            (setf (nth last-idx result)
                  (concat (substring last-line 0 trim-to)
                          (propertize "…" 'face 'shadow))))))

      result)))


;;; Layout Renderer

(defun spotlist-render-compact ()
  "Render SpotList in compact tiled view."
  (let ((old-point (point))
        (spotlist-inhibit-hooks t))

    ;; Clear old overlays
    (remove-overlays (point-min) (point-max))
    (dolist (entry spotlist-entries)
      (when (spotlist-entry-body-overlay entry)
        (delete-overlay (spotlist-entry-body-overlay entry))
        (setf (spotlist-entry-body-overlay entry) nil)))

    (let ((buffer-read-only nil))
      (erase-buffer)

      ;; Header
      (let ((start (point)))
        (insert (propertize "SpotList (Compact View - Read Only)\n" 'face 'bold))
        (if (and (fboundp 'evil-mode) (boundp 'evil-mode) evil-mode)
            (insert (propertize "gd:jump dd:delete v:toggle-view M:adjust-entry m:adjust-global R:reset gr:refresh q:quit\n\n"
                                'face 'font-lock-constant-face))
          (insert (propertize "C-c C-j:jump C-c C-d:delete C-c C-v:toggle-view C-c C-M:adjust-entry C-c C-m:adjust-global C-c C-r:reset C-c C-g:refresh C-c C-q:quit\n\n"
                              'face 'font-lock-constant-face)))
        (spotlist-make-protected start (point)))

      (if (null spotlist-entries)
          (let ((start (point)))
            (insert (propertize "No entries. Add with: C-c C-s r (region) or C-c C-s l (line)\n"
                                'face 'font-lock-constant-face))
            (spotlist-make-protected start (point)))

        ;; Solve layout
        (let* ((window-width (max 80 (- (window-width) 2)))
               (layout (spotlist-solve-compact-layout spotlist-entries window-width)))

          ;; Render the solved layout
          (spotlist-render-layout layout))))

    (goto-char (min old-point (point-max)))))

(defun spotlist-render-layout (layout)
  "Render a solved LAYOUT (spotlist-layout)."
  (let* ((boxes (or (spotlist-layout-boxes layout) '()))
         (total-height (max 0 (or (spotlist-layout-total-height layout) 0)))
         (grid (make-vector (1+ total-height) ""))) ;; prefill with empty strings
    (dolist (box boxes)
      (spotlist-render-box-to-grid box grid))
    ;; Render grid without trailing newline
    (dotimes (i (length grid))
      (let ((line (aref grid i)))
        (when (and line (not (string-empty-p line)))
          (let ((start (point)))
            (insert line)
            (when (< i (1- (length grid))) ; Only add newline if not last line
              (insert "\n"))
            (spotlist-make-protected start (point))))))))

(defun spotlist-render-box-to-grid (box grid)
  "Render a single BOX into the GRID array."
  (let* ((entry (spotlist-layout-box-entry box))
         (x (spotlist-layout-box-x box))
         (y (spotlist-layout-box-y box))
         (width (spotlist-layout-box-width box))
         (height (spotlist-layout-box-height box))
         (text-lines (or (spotlist-layout-box-text-lines box) '()))
         (id (spotlist-entry-id entry))
         (buf (spotlist-entry-buf entry))
         (raw-name (if (buffer-live-p buf) (buffer-name buf) "<killed>"))
         (buf-name (format "%s" raw-name)) ; Ensure buf-name is string
         (title-text (format "[%d] %s"
                             (or id 0) ; Ensure ID is number, default to 0
                             (substring buf-name 0 (min (length buf-name)
                                                        (max 0 (- width 10))))))
         (original-text (or (spotlist-entry-text entry) "")) ; Ensure string
         (plain-original (spotlist-expand-tabs original-text))  ; Keep properties through expansion
         (dedented-original (spotlist-dedent-text plain-original))
         (is-dedented (not (string= (substring-no-properties plain-original)
                                    (substring-no-properties dedented-original))))
         (tooltip (format "%s%s%s"
                          (if (or (spotlist-entry-custom-width entry)
                                 (spotlist-entry-custom-height entry))
                              (format "This entry: %dx%d • "
                                     (or (spotlist-entry-custom-width entry)
                                         spotlist-compact-max-entry-width)
                                     (or (spotlist-entry-custom-height entry)
                                         spotlist-compact-max-entry-height))
                            "")
                          (format "Global max: %dx%d • Press 'M' for entry, 'm' for global"
                                  (or spotlist-compact-max-entry-width 50)
                                  (or spotlist-compact-max-entry-height 6))
                          (if is-dedented " • Indentation stripped" ""))))

    ;; Top border with title
    (spotlist-append-to-grid-line
     grid y x
     (propertize (format "┌%s%s┐"
                        title-text
                        (make-string (max 0 (- width 2 (string-width title-text))) ?─))
                'face 'font-lock-keyword-face
                'spotlist-entry-id id
                'help-echo tooltip))

    ;; Content lines - ensure ALL content has the entry-id property
    (dotimes (i (length text-lines))
      (let* ((line (nth i text-lines))
             (line-display-length (length (substring-no-properties (or line ""))))
             (padding-needed (max 0 (- width 2 line-display-length)))
             (padding (propertize (make-string padding-needed ?\s)
                                  'spotlist-entry-id id
                                  'help-echo tooltip))
             ;; Add the entry-id property to the content line itself
             (line-with-id (if line
                               (propertize (copy-sequence line)
                                          'spotlist-entry-id id
                                          'help-echo tooltip)
                             "")))
        (spotlist-append-to-grid-line
         grid (+ y i 1) x
         (concat
          (propertize "│" 'face 'font-lock-keyword-face
                      'spotlist-entry-id id
                      'help-echo tooltip)
          line-with-id  ; Insert line with entry-id property
          padding       ; Add padding with entry-id property
          (propertize "│" 'face 'font-lock-keyword-face
                      'spotlist-entry-id id
                      'help-echo tooltip)))))

    ;; Pad remaining content lines
    (dotimes (i (- (max 0 (- height 2)) (length text-lines)))
      (let ((line-idx (+ i 1 (length text-lines))))
        (spotlist-append-to-grid-line
         grid (+ y line-idx) x
         (concat
          (propertize "│" 'face 'font-lock-keyword-face
                      'spotlist-entry-id id
                      'help-echo tooltip)
          (propertize (make-string (max 0 (- width 2)) ?\s)
                      'spotlist-entry-id id
                      'help-echo tooltip)
          (propertize "│" 'face 'font-lock-keyword-face
                      'spotlist-entry-id id
                      'help-echo tooltip)))))

    ;; Bottom border
    (spotlist-append-to-grid-line
     grid (+ y height -1) x
     (propertize (format "└%s┘" (make-string (max 0 (- width 2)) ?─))
                'face 'font-lock-keyword-face
                'spotlist-entry-id id
                'help-echo tooltip))))

(defun spotlist-append-to-grid-line (grid y x text)
  "Append TEXT (string) to GRID line Y at column X, padding with spaces."
  (let ((text (or text "")))
    (when (and (integerp y) (>= y 0) (< y (length grid)))
      (let* ((current (or (aref grid y) ""))
             (current-display-len (string-width current)) ; Use display length
             (pad-needed (max 0 (- x current-display-len)))
             (pad (make-string pad-needed ?\s))
             (new-line (concat current pad text)))
        (aset grid y new-line)))))

;;; View Toggle

(defun spotlist-toggle-view ()
  "Toggle between normal and compact view."
  (interactive)
  (setq spotlist-view-mode
        (if (eq spotlist-view-mode 'normal) 'compact 'normal))
  (let ((spotlist-inhibit-hooks t))
    (spotlist-render))
  (message "View mode: %s" spotlist-view-mode))

(defun spotlist-adjust-compact-dimensions ()
  "Interactively adjust maximum dimensions for compact view entries (global)."
  (interactive)
  (let* ((new-width (read-number
                     (format "Max entry width (current: %d): "
                            spotlist-compact-max-entry-width)
                     spotlist-compact-max-entry-width))
         (new-height (read-number
                      (format "Max entry height (current: %d): "
                             spotlist-compact-max-entry-height)
                      spotlist-compact-max-entry-height)))
    (setq spotlist-compact-max-entry-width (max 15 new-width)) ; Min width for title
    (setq spotlist-compact-max-entry-height (max 3 new-height)) ; Min height for borders
    (when (eq spotlist-view-mode 'compact)
      (let ((spotlist-inhibit-hooks t))
        (spotlist-render)))
    (message "Global compact view dimensions: %dx%d"
             spotlist-compact-max-entry-width
             spotlist-compact-max-entry-height)))

(defun spotlist-adjust-entry-dimensions ()
  "Interactively adjust dimensions for the entry at point."
  (interactive)
  (let ((entry (spotlist-entry-near-point)))
    (unless entry
      (user-error "No entry at point"))
    (let* ((current-width (or (spotlist-entry-custom-width entry)
                             spotlist-compact-max-entry-width))
           (current-height (or (spotlist-entry-custom-height entry)
                              spotlist-compact-max-entry-height))
           (new-width (read-number
                       (format "Entry [%d] width (current: %d, global: %d): "
                              (spotlist-entry-id entry)
                              current-width
                              spotlist-compact-max-entry-width)
                       current-width))
           (new-height (read-number
                        (format "Entry [%d] height (current: %d, global: %d): "
                               (spotlist-entry-id entry)
                               current-height
                               spotlist-compact-max-entry-height)
                        current-height)))
      (setf (spotlist-entry-custom-width entry) (max 15 new-width))
      (setf (spotlist-entry-custom-height entry) (max 3 new-height))
      (when (eq spotlist-view-mode 'compact)
        (let ((spotlist-inhibit-hooks t))
          (spotlist-render)))
      (message "Entry [%d] dimensions: %dx%d"
               (spotlist-entry-id entry)
               (spotlist-entry-custom-width entry)
               (spotlist-entry-custom-height entry)))))

(defun spotlist-reset-entry-dimensions ()
  "Reset the current entry's dimensions to use global defaults."
  (interactive)
  (let ((entry (spotlist-entry-near-point)))
    (unless entry
      (user-error "No entry at point"))
    (setf (spotlist-entry-custom-width entry) nil)
    (setf (spotlist-entry-custom-height entry) nil)
    (when (eq spotlist-view-mode 'compact)
      (let ((spotlist-inhibit-hooks t))
        (spotlist-render)))
    (message "Entry [%d] reset to global dimensions (%dx%d)"
             (spotlist-entry-id entry)
             spotlist-compact-max-entry-width
             spotlist-compact-max-entry-height)))


;;; Helper functions

(defun spotlist-entry-near-point ()
  "Get entry at or near point.
In compact view, uses the `spotlist-entry-id` property on the rendered box."
  (if (eq spotlist-view-mode 'compact)
      (let ((id (get-text-property (point) 'spotlist-entry-id)))
        (when id
          (cl-find id spotlist-entries :key #'spotlist-entry-id)))
    (or (spotlist-find-entry-at-pos (point))
        ;; Try looking backward for an entry
        (save-excursion
          (let ((id nil))
            (while (and (not id) (not (bobp)))
              (forward-line -1)
              (setq id (get-text-property (point) 'spotlist-entry-id)))
            (when id
              (cl-find id spotlist-entries :key #'spotlist-entry-id))))
        ;; Try looking forward
        (save-excursion
          (let ((id nil))
            (while (and (not id) (not (eobp)))
              (forward-line 1)
              (setq id (get-text-property (point) 'spotlist-entry-id)))
            (when id
              (cl-find id spotlist-entries :key #'spotlist-entry-id)))))))

;;; Cleanup

(defun spotlist-clear-all ()
  "Clear all entries from SpotList."
  (interactive)
  (when (yes-or-no-p "Clear all SpotList entries? ")
    (dolist (entry spotlist-entries)
      (when (spotlist-entry-start-marker entry)
        (set-marker (spotlist-entry-start-marker entry) nil))
      (when (spotlist-entry-end-marker entry)
        (set-marker (spotlist-entry-end-marker entry) nil))
      (when (spotlist-entry-body-overlay entry)
        (delete-overlay (spotlist-entry-body-overlay entry))))
    (setq spotlist-entries nil)
    (setq spotlist-edit-history nil)
    (setq spotlist-edit-history-position 0)
        (spotlist-stop-visibility-refresh)
    (when (get-buffer spotlist-buffer-name)
      (with-current-buffer spotlist-buffer-name
        (let ((spotlist-inhibit-hooks t))
          (spotlist-render))))
    (message "Cleared all entries")))

;;; Provide

(provide 'spotlist)

;;; spotlist.el ends here
