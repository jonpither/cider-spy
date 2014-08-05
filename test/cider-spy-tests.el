;;; cider-tests.el

;; Copyright © 2012-2014 Jon Pither

;; Author: Jon Pither <jon.pither@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; This file is part of CIDER-SPY

;; To run these tests:
;;   All tests: M-x ert t
;;
;;; Code:

(require 'ert)
(require 'cider-spy)

(defun cider-spy-test-grab-section (buffer k)
  (with-current-buffer buffer
    (car (-filter (lambda (section) (eq k (cider-spy-section-type section)))
                  (cider-spy-descendent-sections cider-spy-root-section)))))

(defun cider-spy-test-grab-section-as-string (buffer k)
  (with-current-buffer buffer
    (let ((section (cider-spy-test-grab-section (current-buffer) k)))
      (buffer-substring-no-properties (cider-spy-section-beginning section)
                                      (- (cider-spy-section-end section) 1)))))

(ert-deftest test-fns-section ()
  (with-temp-buffer
    (cider-spy-refresh-buffer (current-buffer) "{\"fns\":{\"clojure.core/println\":2, \"clojure.core/str\":1}}")
    (should (equal "Your Function Calls:\n  clojure.core/println (2 times)\n  clojure.core/str (1 times)"
                   (cider-spy-test-grab-section-as-string (current-buffer) 'fns)))))

(ert-deftest test-session-section ()
  (with-temp-buffer
    (cider-spy-refresh-buffer (current-buffer) "{\"session\":{\"started\":\"08:59:34\",\"seconds\":21}}")
    (should (equal "Your Session:\n  Started 08:59:34, uptime: 21 seconds."
                   (cider-spy-test-grab-section-as-string (current-buffer) 'session)))))

(ert-deftest test-navigate-around-sections ()
  (with-temp-buffer
    (cider-spy-refresh-buffer (current-buffer)
                              "{\"devs\":{\"Awesomedude\":\"foo.bar\"},
                              \"ns-trail\":[{\"ns\":\"proja.core\"}],
                              \"nses-loaded\":{\"proja.corea\":1},
                              \"fns\":{\"clojure.core/println\":2},
                              \"session\":{\"started\":\"08:59:34\",\"seconds\":21}}")
    (goto-char 1)

    (cider-spy-next-section (current-buffer))
    (should (eq 'dev (cider-spy-section-type (cider-spy-find-section-at-point))))

    (cider-spy-next-section (current-buffer))
    (should (eq 'session (cider-spy-section-type (cider-spy-find-section-at-point))))
    (cider-spy-next-section (current-buffer))
    (should (eq 'nses-loaded (cider-spy-section-type (cider-spy-find-section-at-point))))
    (cider-spy-next-section (current-buffer))
    (message (cider-spy-find-section-at-point))
    (should (eq 'ns-breadcrumb (cider-spy-section-type (cider-spy-find-section-at-point))))

    ;; (cider-spy-previous-section (current-buffer))
    ;; (should (eq 'nses-loaded (cider-spy-section-type (cider-spy-find-section-at-point))))
    ;; (should (eq 'nses-loaded (cider-spy-section-type (cider-spy-find-section-at-point))))
    ;; (cider-spy-previous-section (current-buffer))
    ;; (should (eq 'session (cider-spy-section-type (cider-spy-find-section-at-point))))
    ;; (cider-spy-previous-section (current-buffer))
    ;; (should (eq 'devs (cider-spy-section-type (cider-spy-find-section-at-point)))
            )
)
