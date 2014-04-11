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

(ert-deftest test-foo ()
  (should t))
