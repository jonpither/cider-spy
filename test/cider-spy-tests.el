;;; cider-tests.el

;; Copyright © 2012-2014 Jon Pither

;; Author: Tim King <jon.pither@gmail.com>

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
                  cider-spy-sections))))

(defun cider-spy-test-grab-section-as-string (buffer k)
  (with-current-buffer buffer
    (let ((section (cider-spy-test-grab-section (current-buffer) k)))
      (buffer-substring-no-properties (cider-spy-section-beginning section)
                                      (- (cider-spy-section-end section) 1)))))

(ert-deftest test-developers-hacking-section ()
  (with-temp-buffer
    (cider-spy-refresh-buffer (current-buffer) "{\"devs\":[\"Awesomedude\"]}")

    (should (equal "Devs Hacking:\n  Awesomedude"
                   (cider-spy-test-grab-section-as-string (current-buffer) 'devs)))))

(ert-deftest test-ns-trail-section ()
  (with-temp-buffer
    (cider-spy-refresh-buffer (current-buffer) "{\"ns-trail\":[{\"ns\":\"proja.core\"},{\"ns\":\"proja.core2\", \"seconds\":100}]}")
    (should (equal "Your Namespace Trail:\n  proja.core (Am here)\n  proja.core2 (100 seconds)"
                   (cider-spy-test-grab-section-as-string (current-buffer) 'ns-trail)))))

(ert-deftest test-nses-loaded-section ()
  (with-temp-buffer
    (cider-spy-refresh-buffer (current-buffer) "{\"nses-loaded\":{\"proja.corea\":1, \"proja.coreb\":2}}")
    (should (equal "Your Namespaces Loaded:\n  proja.coreb (2 times)\n  proja.corea (1 times)"
                   (cider-spy-test-grab-section-as-string (current-buffer) 'nses-loaded)))))

(ert-deftest test-fns-section ()
  (with-temp-buffer
    (cider-spy-refresh-buffer (current-buffer) "{\"fns\":{\"clojure.core/println\":1, \"clojure.core/str\":2}}")
    (should (equal "Your Function Calls:\n  clojure.core/str (2 times)\n  clojure.core/println (1 times)"
                   (cider-spy-test-grab-section-as-string (current-buffer) 'fns)))))

(ert-deftest test-session-section ()
  (with-temp-buffer
    (cider-spy-refresh-buffer (current-buffer) "{\"session\":{\"started\":\"08:59:34\",\"seconds\":21}}")
    (should (equal "Your Session:\n  Started 08:59:34, uptime: 21 seconds."
                   (cider-spy-test-grab-section-as-string (current-buffer) 'session)))))
