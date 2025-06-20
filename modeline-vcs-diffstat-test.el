;;; modeline-vcs-diffstat-test.el --- Tests for modeline-vcs-diffstat -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Richie Kirchofer
;;
;; Author: Richie Kirchofer
;; Maintainer: Richie Kirchofer
;;
;;; Commentary:
;;
;; ERT tests for the `modeline-vcs-diffstat' package.
;; To run these tests, make sure `modeline-vcs-diffstat.el` is in your
;; `load-path`, then open this file and run `M-x ert-run-tests-in-buffer`.
;;
;; This test suite has been updated to reflect the asynchronous, cache-based
;; architecture of the main package.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'modeline-vcs-diffstat)

;; Make the internal segment function available for testing.
(eval-when-compile
  (require 'doom-modeline))
(declare-function doom-modeline-segment--modeline-vcs-diffstat "modeline-vcs-diffstat")

;;;; Unit Tests for Helper Functions

(ert-deftest modeline-vcs-diffstat--format-number-human-readable-tests ()
  "Test the custom human-readable number formatting function."
  ;; Test standard suffixes
  (should (string= (modeline-vcs-diffstat--format-number-human-readable 999) "999"))
  (should (string= (modeline-vcs-diffstat--format-number-human-readable 1000) "1.0K"))
  (should (string= (modeline-vcs-diffstat--format-number-human-readable 1234) "1.2K"))
  ;; The format "%.1f" rounds 999.999 up to 1000.0. This is expected.
  (should (string= (modeline-vcs-diffstat--format-number-human-readable 999999) "1000.0K"))
  (should (string= (modeline-vcs-diffstat--format-number-human-readable 1000000) "1.0M"))
  (should (string= (modeline-vcs-diffstat--format-number-human-readable 1234567) "1.2M"))
  (should (string= (modeline-vcs-diffstat--format-number-human-readable 1000000000) "1.0B"))
  (should (string= (modeline-vcs-diffstat--format-number-human-readable 1000000000000) "1.0T"))

  ;; Test E-notation fallback for Peta (10^15) and beyond
  (should (string= (modeline-vcs-diffstat--format-number-human-readable (expt 10 15)) "10E14"))
  (should (string= (modeline-vcs-diffstat--format-number-human-readable (expt 10 18)) "10E17")))

(ert-deftest modeline-vcs-diffstat--metrics-tests ()
  "Test the calculation of display metrics from raw diffs."
  (let ((diffs '(:staged-removed 10
                 :unstaged-removed 20
                 :staged-added 30
                 :unstaged-added 40)))
    (let ((metrics (modeline-vcs-diffstat--metrics diffs)))
      ;; Test total counts
      (should (= (plist-get metrics :total-removed) 30))
      (should (= (plist-get metrics :total-added) 70))

      ;; Test symbol counts (using the default count function: ceiling(lines/10))
      (should (= (plist-get metrics :staged-minus-count) 1))   ; 10/30 of 3 total '-' symbols
      (should (= (plist-get metrics :unstaged-minus-count) 2)) ; 20/30 of 3 total '-' symbols
      (should (= (plist-get metrics :staged-plus-count) 3))    ; 30/70 of 7 total '+' symbols, rounded
      (should (= (plist-get metrics :unstaged-plus-count) 4))))) ; 40/70 of 7 total '+' symbols, rounded

;;;; Integration Test for Segment Rendering

(ert-deftest modeline-vcs-diffstat--segment-rendering-test ()
  "Test the complete segment rendering from cached metrics."
  ;; Bind customizable variables to their default values to ensure
  ;; the test is independent of user configuration.
  (let ((modeline-vcs-diffstat-display-methods
         `((100 . ,#'modeline-vcs-diffstat--metrics-numeric-pstring)
           (1 . ,#'modeline-vcs-diffstat--symbol-pstring)))
        (doom-modeline--active t)) ; Simulate an active window for face checking

    ;; --- Test Scenario 1: Symbol Display (total changes < 100) ---
    (let ((modeline-vcs-diffstat--git-numstat-cache
           '(:staged-removed 5 :unstaged-removed 12 :staged-added 25 :unstaged-added 38
             :total-removed 17 :total-added 63 ; total = 80
             :staged-minus-count 1 :unstaged-minus-count 1
             :staged-plus-count 3 :unstaged-plus-count 4)))
      (let ((result (doom-modeline-segment--modeline-vcs-diffstat)))
        (should (string= (substring-no-properties result) "--+++++++"))
        ;; Check that the staged deletions face is correct
        (should (equal (get-text-property 0 'face result)
                       (doom-modeline-face 'modeline-vcs-diffstat-staged-del-face)))
        ;; Check that the help-echo tooltip is correctly formatted
        (let ((help-echo (get-text-property 0 'help-echo result)))
          (should (string= (substring-no-properties help-echo)
                           "Staged: -5 +25 | Unstaged: -12 +38")))))

    ;; --- Test Scenario 2: Human-Readable Display (total changes >= 100) ---
    (let ((modeline-vcs-diffstat--git-numstat-cache
           '(:staged-removed 50 :unstaged-removed 120 :staged-added 250 :unstaged-added 380
             :total-removed 170 :total-added 630 ; total = 800
             )))
      (let ((result (doom-modeline-segment--modeline-vcs-diffstat)))
        (should (string= (substring-no-properties result) "-170 +630"))
        ;; Check the face on the additions part
        (should (equal (get-text-property 5 'face result)
                       (doom-modeline-face 'magit-diffstat-added)))
        (let ((help-echo (get-text-property 0 'help-echo result)))
          (should (string= (substring-no-properties help-echo)
                           "Staged: -50 +250 | Unstaged: -120 +380")))))

    ;; --- Test Scenario 3: No Changes ---
    (let ((modeline-vcs-diffstat--git-numstat-cache
           '(:staged-removed 0 :unstaged-removed 0 :staged-added 0 :unstaged-added 0
             :total-removed 0 :total-added 0)))
      (should (null (doom-modeline-segment--modeline-vcs-diffstat))))))

(provide 'modeline-vcs-diffstat-test)

;;; modeline-vcs-diffstat-test.el ends here
