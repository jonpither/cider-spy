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

;; whack this JSON in and check the buffer after for various sections.
;;{"ns-trail":[{"ns":"proja.core"}],"nses-loaded":{"proja.core":1},"fns":null,"devs":["Awesomedude"],"session":{"started":"08:59:34","seconds":21}}
;;

(defun cider-spy-test-grab-section (buffer k)
  (with-current-buffer buffer
    (car (-filter (lambda (section) (eq k (cider-spy-section-type section)))
                  cider-spy-sections))))

(defun cider-spy-test-grab-section-as-string (buffer k)
  (with-current-buffer buffer
    (let ((section (cider-spy-test-grab-section (current-buffer) k)))
      (buffer-substring-no-properties (cider-spy-section-beginning section)
                                      (- (cider-spy-section-end section) 1)))))

(ert-deftest test-foo ()
  (with-temp-buffer
    (cider-spy-refresh-buffer
     (current-buffer)
     "{\"ns-trail\":[{\"ns\":\"proja.core\"}],\"nses-loaded\":{\"proja.core\":1},\"fns\":null,\"devs\":[\"Awesomedude\"],\"session\":{\"started\":\"08:59:34\",\"seconds\":21}}")

    (let ((devs-section (cider-spy-test-grab-section (current-buffer) 'devs)))
      (message (buffer-substring (cider-spy-section-beginning devs-section)
                                 (- (cider-spy-section-end devs-section) 1)))
      (should (equal "Devs Hacking:\n  Awesomedude"
                     (cider-spy-test-grab-section-as-string (current-buffer) 'devs))))))
