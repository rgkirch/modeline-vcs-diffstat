;;; modeline-vcs-diffstat-tests.el --- Tests for modeline-vcs-diffstat -*- lexical-binding: t; -*-

;; Copyright (C) 2025 rgkirch
;;
;; Author: rgkirch
;;
;;; Commentary:
;;
;; ERT tests for the `modeline-vcs-diffstat' package.
;; To run these tests, make sure `modeline-vcs-diffstat.el` is in your
;; `load-path`, then open this file and run `M-x ert-run-tests-in-buffer`.

;;; Code:

(require 'ert)
(require 'modeline-vcs-diffstat)

;;;; Test Helper Functions

(ert-deftest modeline-vcs-diffstat--custom-human-readable-tests ()
  "Test the custom human-readable number formatting function."
  ;; Test standard suffixes
  (should (string= (modeline-vcs-diffstat--custom-human-readable 999) "999"))
  (should (string= (modeline-vcs-diffstat--custom-human-readable 1000) "1.0K"))
  (should (string= (modeline-vcs-diffstat--custom-human-readable 1234) "1.2K"))
  (should (string= (modeline-vcs-diffstat--custom-human-readable 999999) "1000.0K")) ; Stays K before 1M
  (should (string= (modeline-vcs-diffstat--custom-human-readable 1000000) "1.0M"))
  (should (string= (modeline-vcs-diffstat--custom-human-readable 1234567) "1.2M"))
  (should (string= (modeline-vcs-diffstat--custom-human-readable 1000000000) "1.0B"))
  (should (string= (modeline-vcs-diffstat--custom-human-readable 1000000000000) "1.0T"))

  ;; Test E-notation fallback for Peta (10^15) and beyond
  (should (string= (modeline-vcs-diffstat--custom-human-readable (expt 10 15)) "10E13"))
  (should (string= (modeline-vcs-diffstat--custom-human-readable (expt 10 18)) "10E16"))
  )

(ert-deftest modeline-vcs-diffstat--calculate-display-metrics-tests ()
  "Test the calculation of display metrics from raw diffs."
  (let ((diffs '(:staged-deleted 10
                 :unstaged-deleted 20
                 :staged-added 30
                 :unstaged-added 40)))
    (let ((metrics (modeline-vcs-diffstat--calculate-display-metrics diffs)))
      ;; Test total counts
      (should (= (plist-get metrics :total-deleted) 30))
      (should (= (plist-get metrics :total-added) 70))

      ;; Test symbol counts (using the default count function: ceiling(lines/10))
      (should (= (plist-get metrics :staged-minus-count) 1))
      (should (= (plist-get metrics :unstaged-minus-count) 2))
      (should (= (plist-get metrics :staged-plus-count) 3))
      (should (= (plist-get metrics :unstaged-plus-count) 4)))))

(ert-deftest modeline-vcs-diffstat--display-method-selection-tests ()
  "Test that the correct display formatter is chosen based on thresholds."
  (let ((modeline-vcs-diffstat-display-methods
         `((1000 . ,#'modeline-vcs-diffstat--format-human-readable)
           (1 . ,#'modeline-vcs-diffstat--format-symbols))))

    (let* ((small-changes '(:total-deleted 10 :total-added 20))
           (large-changes '(:total-deleted 500 :total-added 600)))
      (let* ((small-method-entry
              (cl-find-if (lambda (entry)
                            (>= (+ (plist-get small-changes :total-deleted)
                                   (plist-get small-changes :total-added))
                                (car entry)))
                          modeline-vcs-diffstat-display-methods))
             (large-method-entry
              (cl-find-if (lambda (entry)
                            (>= (+ (plist-get large-changes :total-deleted)
                                   (plist-get large-changes :total-added))
                                (car entry)))
                          modeline-vcs-diffstat-display-methods)))
        (should (eq (cdr small-method-entry) #'modeline-vcs-diffstat--format-symbols))
        (should (eq (cdr large-method-entry) #'modeline-vcs-diffstat--format-human-readable))))))

(ert-deftest modeline-vcs-diffstat--format-symbols-test ()
  "Test the symbolic formatting function with an exact match."
  (let ((metrics '(:staged-minus-count 1
                   :unstaged-minus-count 2
                   :staged-plus-count 3
                   :unstaged-plus-count 4)))
    (let* ((staged-del-str (make-string (plist-get metrics :staged-minus-count) modeline-vcs-diffstat-char-del))
           (unstaged-del-str (make-string (plist-get metrics :unstaged-minus-count) modeline-vcs-diffstat-char-del))
           (unstaged-add-str (make-string (plist-get metrics :unstaged-plus-count) modeline-vcs-diffstat-char-add))
           (staged-add-str (make-string (plist-get metrics :staged-plus-count) modeline-vcs-diffstat-char-add))
           (expected-string (concat staged-del-str
                                    unstaged-del-str
                                    unstaged-add-str
                                    staged-add-str))
           (formatted-string (modeline-vcs-diffstat--format-symbols metrics)))
      (should (string= (substring-no-properties formatted-string) expected-string)))))

(provide 'modeline-vcs-diffstat-tests)

;;; modeline-vcs-diffstat-tests.el ends here
