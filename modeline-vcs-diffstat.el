;;; modeline-vcs-diffstat.el --- Show staged and unstaged VCS diff stats in the modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2025 rgkirch
;;
;; Author: rgkirch
;; Maintainer: rgkirch
;; Created: June 10, 2025
;; Version: 0.2
;; Package-Version: 20250611.2
;; Package-Requires: ((emacs "26.1") (doom-modeline "2.0") (magit "3.3"))
;; Homepage: https://github.com/rgkirch/modeline-vcs-diffstat
;;
;;; Commentary:
;;
;; This package provides a modeline segment for doom-modeline that displays
;; version control changes for the current file.
;;
;; It distinguishes between staged and unstaged changes, showing a visual
;; representation of lines added and deleted. This allows for a quick,
;; at-a-glance summary of the file's status without leaving the editor.

;;; Code:

(require 'magit-diff)
(require 'doom-modeline)
(require 'cl-lib)

(defface modeline-vcs-diffstat-staged-del-face
  '((((class color) (min-colors 88) (background light)) :foreground "#CD5C5C")
    (((class color) (min-colors 88) (background dark)) :foreground "dark red")
    (t :inherit magit-diffstat-removed :weight normal))
  "Default face for STAGED deletions in the vcs-diffstat modeline segment."
  :group 'modeline-vcs-diffstat)

(defface modeline-vcs-diffstat-staged-add-face
  '((((class color) (min-colors 88) (background light)) :foreground "#6B8E23")
    (((class color) (min-colors 88) (background dark)) :foreground "dark green")
    (t :inherit magit-diffstat-added :weight normal))
  "Default face for STAGED additions in the vcs-diffstat modeline segment."
  :group 'modeline-vcs-diffstat)

(defgroup modeline-vcs-diffstat nil
  "Customizations for the vcs-diffstat modeline segment."
  :group 'doom-modeline)

(defcustom modeline-vcs-diffstat-char-del ?-
  "The character to display for deleted lines."
  :type 'character
  :group 'modeline-vcs-diffstat)

(defcustom modeline-vcs-diffstat-char-add ?+
  "The character to display for added lines."
  :type 'character
  :group 'modeline-vcs-diffstat)

(defcustom modeline-vcs-diffstat-face-staged-del 'modeline-vcs-diffstat-staged-del-face
  "Face for STAGED deletions (dim red)."
  :type 'face
  :group 'modeline-vcs-diffstat)

(defcustom modeline-vcs-diffstat-face-unstaged-del 'magit-diffstat-removed
  "Face for UNSTAGED deletions (bright red)."
  :type 'face
  :group 'modeline-vcs-diffstat)

(defcustom modeline-vcs-diffstat-face-unstaged-add 'magit-diffstat-added
  "Face for UNSTAGED additions (bright green)."
  :type 'face
  :group 'modeline-vcs-diffstat)

(defcustom modeline-vcs-diffstat-face-staged-add 'modeline-vcs-diffstat-staged-add-face
  "Face for STAGED additions (dim green)."
  :type 'face
  :group 'modeline-vcs-diffstat)

(defun modeline-vcs-diffstat--default-count-function (lines)
  "Default function to calculate display symbols. Divides LINES by 10 and rounds up."
  (ceiling (/ (float lines) 10.0)))

(defcustom modeline-vcs-diffstat-count-function
  #'modeline-vcs-diffstat--default-count-function
  "Function to calculate how many symbols to display for a given number of lines.
The function should accept one argument (the number of lines) and return an
integer (the number of symbols)."
  :type 'function
  :group 'modeline-vcs-diffstat)

(defun modeline-vcs-diffstat--line-counts ()
  "Fetch staged and unstaged line counts for the current file.
If the file is not tracked by Git, treats all lines as unstaged additions.
Returns a plist with keys :staged-deleted, :unstaged-deleted,
:staged-added, and :unstaged-added."
  (if (and buffer-file-name (vc-backend buffer-file-name))
      ;; File is tracked by VC, so get diffs.
      (let* ((dir (file-name-directory buffer-file-name))
             (file (file-name-nondirectory buffer-file-name))
             ;; Command for Staged (--cached) changes
             (staged-cmd (format "git -C %s diff --cached --numstat -- %s"
                                 (shell-quote-argument dir)
                                 (shell-quote-argument file)))
             ;; Command for Unstaged changes
             (unstaged-cmd (format "git -C %s diff --numstat -- %s"
                                   (shell-quote-argument dir)
                                   (shell-quote-argument file)))
             ;; Run both commands
             (staged-out (shell-command-to-string staged-cmd))
             (unstaged-out (shell-command-to-string unstaged-cmd))
             ;; Parse the results
             (staged-lines (unless (string-empty-p staged-out) (split-string staged-out "\t")))
             (unstaged-lines (unless (string-empty-p unstaged-out) (split-string unstaged-out "\t"))))
        ;; Construct the final list, defaulting to 0 if no changes exist
        (list :staged-deleted (if staged-lines (string-to-number (nth 1 staged-lines)) 0)
              :unstaged-deleted (if unstaged-lines (string-to-number (nth 1 unstaged-lines)) 0)
              :staged-added (if staged-lines (string-to-number (nth 0 staged-lines)) 0)
              :unstaged-added (if unstaged-lines (string-to-number (nth 0 unstaged-lines)) 0)))
    ;; File is not tracked; treat all lines as new unstaged additions.
    (when buffer-file-name
      (list :staged-deleted 0
            :unstaged-deleted 0
            :staged-added 0
            :unstaged-added (count-lines (point-min) (point-max))))))

(cl-defun modeline-vcs-diffstat--calculate-display-metrics ((&whole diffs &key staged-deleted unstaged-deleted staged-added unstaged-added &allow-other-keys))
  "Calculate and augment DIFFS plist with display metrics.
Adds totals, symbol counts, and staged/unstaged counts to the plist."
  (when diffs
    (let* ((total-del (+ staged-deleted unstaged-deleted))
           (total-add (+ staged-added unstaged-added))
           (minus-count (funcall modeline-vcs-diffstat-count-function total-del))
           (plus-count (funcall modeline-vcs-diffstat-count-function total-add))
           (staged-minus-count (if (> total-del 0) (round (* minus-count (/ (float staged-deleted) total-del))) 0))
           (staged-plus-count (if (> total-add 0) (round (* plus-count (/ (float staged-added) total-add))) 0)))
      (cl-list*
       :total-deleted total-del
       :total-added total-add
       :staged-minus-count staged-minus-count
       :unstaged-minus-count (- minus-count staged-minus-count)
       :staged-plus-count staged-plus-count
       :unstaged-plus-count (- plus-count staged-plus-count)
       diffs))))

(defvar modeline-vcs-diffstat-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'magit-diff-buffer-file)
    map)
  "A keymap for the git-diff-status modeline segment.")

(doom-modeline-def-segment modeline-vcs-diffstat
  "A clickable segment showing staged and unstaged changes."
  (when-let* ((metrics (modeline-vcs-diffstat--calculate-display-metrics (modeline-vcs-diffstat--line-counts)))
              (total-changes (+ (plist-get metrics :total-deleted)
                                (plist-get metrics :total-added))))
    (when (> total-changes 0)
      (let ((staged-del-str (make-string (plist-get metrics :staged-minus-count) modeline-vcs-diffstat-char-del))
            (unstaged-del-str (make-string (plist-get metrics :unstaged-minus-count) modeline-vcs-diffstat-char-del))
            (unstaged-add-str (make-string (plist-get metrics :unstaged-plus-count) modeline-vcs-diffstat-char-add))
            (staged-add-str (make-string (plist-get metrics :staged-plus-count) modeline-vcs-diffstat-char-add)))
        (propertize
         (concat " "
                 (propertize staged-del-str 'face modeline-vcs-diffstat-face-staged-del)
                 (propertize unstaged-del-str 'face modeline-vcs-diffstat-face-unstaged-del)
                 (propertize unstaged-add-str 'face modeline-vcs-diffstat-face-unstaged-add)
                 (propertize staged-add-str 'face modeline-vcs-diffstat-face-staged-add)
                 " ")
         'mouse-face 'mode-line-highlight
         'local-map modeline-vcs-diffstat-keymap
         'help-echo (concat
                     "Staged: "
                     (propertize (format "-%d" (plist-get metrics :staged-deleted)) 'face 'magit-diffstat-removed) " "
                     (propertize (format "+%d" (plist-get metrics :staged-added)) 'face 'magit-diffstat-added)
                     " | Unstaged: "
                     (propertize (format "-%d" (plist-get metrics :unstaged-deleted)) 'face 'magit-diffstat-removed) " "
                     (propertize (format "+%d" (plist-get metrics :unstaged-added)) 'face 'magit-diffstat-added)))))))

(provide 'modeline-vcs-diffstat)

;;; modeline-vcs-diffstat.el ends here
