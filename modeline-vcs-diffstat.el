;;; modeline-vcs-diffstat.el --- Show staged and unstaged VCS diff stats in the modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Richie Kirchofer

;; Author: Richie Kirchofer
;; Version: 0.6
;; Package-Requires: ((emacs "26.1") doom-modeline magit)
;; Keywords:
;; URL: https://github.com/rgkirch/modeline-vcs-diffstat

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides a modeline segment for doom-modeline that displays
;; version control changes for the current file.
;;
;; It distinguishes between staged and unstaged changes, showing a visual
;; representation of lines added and removed. This allows for a quick,
;; at-a-glance summary of the file's status without leaving the editor.
;;
;; The display method is chosen based on customizable thresholds. By default:
;; - 1-99 changes: Symbols (e.g., --+++)
;; - 100+ changes: Human-readable with suffixes (e.g., -1.2K, +123M, -1.5B)
;;
;; Clicking (`mouse-1`) on the segment calls `magit-diff-buffer-file`
;; to show a diff of the current file.
;;
;; This version uses an asynchronous update model with caching to prevent
;; the "call-process invoked recursively" error. It caches only the raw
;; metrics data, and the modeline segment itself handles the dynamic rendering.

;;; Code:

(require 'magit-diff)
(require 'doom-modeline)
(require 'cl-lib)

(defgroup modeline-vcs-diffstat nil
  "Customizations for the vcs-diffstat modeline segment."
  :group 'doom-modeline)

(defcustom modeline-vcs-diffstat-char-del ?-
  "The character to display for removed lines."
  :type 'character
  :group 'modeline-vcs-diffstat)

(defcustom modeline-vcs-diffstat-char-add ?+
  "The character to display for added lines."
  :type 'character
  :group 'modeline-vcs-diffstat)

(defface modeline-vcs-diffstat-face-staged-add-default
  `((t :foreground ,(face-foreground 'magit-diff-added)))
  "Face for STAGED additions.")

(defcustom modeline-vcs-diffstat-face-staged-add 'modeline-vcs-diffstat-face-staged-add-default
  "Face for STAGED additions."
  :type 'face
  :group 'modeline-vcs-diffstat)

(defface modeline-vcs-diffstat-face-staged-del-default
  `((t :foreground ,(face-foreground 'magit-diff-removed)))
  "Face for STAGED deletions.")

(defcustom modeline-vcs-diffstat-face-staged-del 'modeline-vcs-diffstat-face-staged-del-default
  "Face for STAGED deletions."
  :type 'face
  :group 'modeline-vcs-diffstat)

(defface modeline-vcs-diffstat-face-unstaged-add-default
  `((t :foreground ,(face-foreground 'magit-diff-added-highlight)))
  "Face for UNSTAGED additions.")

(defcustom modeline-vcs-diffstat-face-unstaged-add 'modeline-vcs-diffstat-face-unstaged-add-default
  "Face for UNSTAGED additions."
  :type 'face
  :group 'modeline-vcs-diffstat)

(defface modeline-vcs-diffstat-face-unstaged-del-default
  `((t :foreground ,(face-foreground 'magit-diff-removed-highlight)))
  "Face for UNSTAGED deletions.")

(defcustom modeline-vcs-diffstat-face-unstaged-del 'modeline-vcs-diffstat-face-unstaged-del-default
  "Face for UNSTAGED deletions."
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

(defcustom modeline-vcs-diffstat-number-suffixes
  '((12 . "T") (9 . "B") (6 . "M") (3 . "K"))
  "An alist mapping powers of 10 to their suffix.
Used for formatting large numbers in a human-readable way. The list
should be sorted with the highest power first. For powers greater
than the largest entry (e.g., 15 for Peta), E-notation is used."
  :type '(alist :key-type integer :value-type string))

(declare-function modeline-vcs-diffstat--symbol-pstring "modeline-vcs-diffstat")
(declare-function modeline-vcs-diffstat--metrics-numeric-pstring "modeline-vcs-diffstat")

(defcustom modeline-vcs-diffstat-display-methods
  `((100 . ,#'modeline-vcs-diffstat--metrics-numeric-pstring)
    (1 . ,#'modeline-vcs-diffstat--symbol-pstring))
  "An alist mapping change thresholds to display functions.
The list should be sorted with the highest threshold first. The segment will
choose the first method for which the total number of changes is
greater than or equal to the threshold.

Each element is a cons cell `(THRESHOLD . FUNCTION)`.
- THRESHOLD is an integer.
- FUNCTION is a function that accepts one argument (the metrics plist)
  and returns a formatted, propertized string for the modeline."
  :type '(alist :key-type integer :value-type function)
  :group 'modeline-vcs-diffstat)

;; --- Core Data Fetching and Formatting Functions ---

(defun modeline-vcs-diffstat--num-lines-changed ()
  "Fetch staged and unstaged line counts for the current file.
If the file is not tracked by Git, treats all lines as unstaged additions.
Returns a plist with keys :staged-removed, :unstaged-removed,
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
        (list :staged-removed (if staged-lines (string-to-number (nth 1 staged-lines)) 0)
              :unstaged-removed (if unstaged-lines (string-to-number (nth 1 unstaged-lines)) 0)
              :staged-added (if staged-lines (string-to-number (nth 0 staged-lines)) 0)
              :unstaged-added (if unstaged-lines (string-to-number (nth 0 unstaged-lines)) 0)))
    ;; File is not tracked; treat all lines as new unstaged additions.
    (when buffer-file-name
      (list :staged-removed 0
            :unstaged-removed 0
            :staged-added 0
            :unstaged-added (count-lines (point-min) (point-max))))))

(cl-defun modeline-vcs-diffstat--metrics ((&whole diffs &key (staged-removed 0) (unstaged-removed 0) (staged-added 0) (unstaged-added 0) &allow-other-keys))
  "Calculate and augment DIFFS plist with display metrics.
Adds totals, symbol counts, and staged/unstaged counts to the plist."
  (when diffs
    (let* ((total-del (+ staged-removed unstaged-removed))
           (total-add (+ staged-added unstaged-added))
           (minus-count (funcall modeline-vcs-diffstat-count-function total-del))
           (plus-count (funcall modeline-vcs-diffstat-count-function total-add))
           (staged-minus-count (if (> total-del 0) (round (* minus-count (/ (float staged-removed) total-del))) 0))
           (staged-plus-count (if (> total-add 0) (round (* plus-count (/ (float staged-added) total-add))) 0)))
      (cl-list*
       :total-removed total-del
       :total-added total-add
       :staged-minus-count staged-minus-count
       :unstaged-minus-count (- minus-count staged-minus-count)
       :staged-plus-count staged-plus-count
       :unstaged-plus-count (- plus-count staged-plus-count)
       diffs))))

(defun modeline-vcs-diffstat--symbol-pstring (metrics)
  "Format the display string using symbols based on METRICS."
  (let ((staged-del-str (make-string (plist-get metrics :staged-minus-count) modeline-vcs-diffstat-char-del))
        (unstaged-del-str (make-string (plist-get metrics :unstaged-minus-count) modeline-vcs-diffstat-char-del))
        (unstaged-add-str (make-string (plist-get metrics :unstaged-plus-count) modeline-vcs-diffstat-char-add))
        (staged-add-str (make-string (plist-get metrics :staged-plus-count) modeline-vcs-diffstat-char-add)))
    (concat (propertize staged-del-str 'face (doom-modeline-face modeline-vcs-diffstat-face-staged-del))
            (propertize unstaged-del-str 'face (doom-modeline-face modeline-vcs-diffstat-face-unstaged-del))
            (propertize unstaged-add-str 'face (doom-modeline-face modeline-vcs-diffstat-face-unstaged-add))
            (propertize staged-add-str 'face (doom-modeline-face modeline-vcs-diffstat-face-staged-add)))))

(defun modeline-vcs-diffstat--format-number-human-readable (num)
  "Format NUM into a human-readable string with custom suffixes."
  (if (< num 1000)
      (format "%d" num)
    (let* ((power (floor (/ (log10 num) 3)))
           (suffix-entry (assoc (* power 3) modeline-vcs-diffstat-number-suffixes)))
      (if suffix-entry
          (format "%.1f%s" (/ (float num) (expt 1000 power)) (cdr suffix-entry))
        (let ((num-str (format "%d" num)))
          (format "%sE%d"
                  (substring num-str 0 2)
                  (- (length num-str) 2)))))))

(defun modeline-vcs-diffstat--metrics-numeric-pstring (metrics)
  "Format the display string using human-readable numbers based on METRICS."
  (let ((del-str (modeline-vcs-diffstat--format-number-human-readable (plist-get metrics :total-removed)))
        (add-str (modeline-vcs-diffstat--format-number-human-readable (plist-get metrics :total-added))))
    (concat (propertize (format "-%s" del-str) 'face (doom-modeline-face 'magit-diffstat-removed))
            (propertize (format " +%s" add-str) 'face (doom-modeline-face 'magit-diffstat-added)))))

(defun modeline-vcs-diffstat--select-formatter (display-methods metrics)
  "Given METRICS, return formatter selected from modeline-vcs-diffstat-display-methods."
  (when metrics
    (let* ((total-changes (+ (or (plist-get metrics :total-removed) 0)
                             (or (plist-get metrics :total-added) 0))))
      (when (> total-changes 0)
        (let* ((method-entry (cl-find-if (lambda (entry) (>= total-changes (car entry)))
                                         display-methods))
               (formatter (cdr method-entry)))
          formatter)))))

(defun modeline-vcs-diffstat--format-metrics (metrics)
  "Select formatter from METRICS and return a propertized string.
Internal helper that contains the core display logic."
  (let ((formatter (modeline-vcs-diffstat--select-formatter modeline-vcs-diffstat-display-methods metrics)))
    (when formatter
      (funcall formatter metrics))))

(defvar modeline-vcs-diffstat-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'magit-diff-buffer-file)
    map)
  "A keymap for the git-diff-status modeline segment.")

(defvar-local modeline-vcs-diffstat--git-numstat-cache nil
  "Buffer-local cache for the raw vcs-diffstat metrics plist.
This is updated asynchronously. The modeline segment reads from this
cache and formats the string dynamically on each render.")

(defvar-local modeline-vcs-diffstat--update-timer nil
  "Buffer-local idle timer to update the diffstat metrics cache.")

(defun modeline-vcs-diffstat--update-cache ()
  "Calculate the diff-stat metrics and update the cache.
This function performs the slow `git` work and is meant to be called
asynchronously. It does NOT do any string formatting."
  (setq modeline-vcs-diffstat--git-numstat-cache (modeline-vcs-diffstat--num-lines-changed))
  (force-mode-line-update))

;; --- Modeline Segment Definition and Setup ---

(doom-modeline-def-segment modeline-vcs-diffstat
  "A clickable segment showing staged and unstaged changes."
  (when-let ((pstring (modeline-vcs-diffstat--format-metrics (modeline-vcs-diffstat--metrics modeline-vcs-diffstat--git-numstat-cache))))
    (propertize pstring
     ;; Common properties for the whole segment.
     'mouse-face 'mode-line-highlight
     'local-map modeline-vcs-diffstat-keymap
     'help-echo (concat
                 "Staged: "
                 (propertize (format "-%d" (plist-get modeline-vcs-diffstat--git-numstat-cache :staged-removed)) 'face 'magit-diffstat-removed) " "
                 (propertize (format "+%d" (plist-get modeline-vcs-diffstat--git-numstat-cache :staged-added)) 'face 'magit-diffstat-added)
                 " | Unstaged: "
                 (propertize (format "-%d" (plist-get modeline-vcs-diffstat--git-numstat-cache :unstaged-removed)) 'face 'magit-diffstat-removed) " "
                 (propertize (format "+%d" (plist-get modeline-vcs-diffstat--git-numstat-cache :unstaged-added)) 'face 'magit-diffstat-added)))))

(defun modeline-vcs-diffstat--setup-update-timer ()
  "Start or restart the idle timer to update VCS diff stats for the current buffer."
  ;; Cancel any existing timer for this buffer first.
  (when (timerp modeline-vcs-diffstat--update-timer)
    (cancel-timer modeline-vcs-diffstat--update-timer))
  (modeline-vcs-diffstat--update-cache)
  (setq modeline-vcs-diffstat--update-timer
        (run-with-idle-timer 1 t #'modeline-vcs-diffstat--update-cache)))

(add-hook 'find-file-hook #'modeline-vcs-diffstat--setup-update-timer)
(add-hook 'revert-buffer-hook #'modeline-vcs-diffstat--setup-update-timer)
(add-hook 'after-save-hook #'modeline-vcs-diffstat--update-cache)
(add-hook 'magit-post-refresh-hook #'modeline-vcs-diffstat--update-cache)

(provide 'modeline-vcs-diffstat)

;;; modeline-vcs-diffstat.el ends here
