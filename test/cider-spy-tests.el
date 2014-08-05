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
