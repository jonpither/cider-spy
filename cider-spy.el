;;; cider-inspect.el --- Spy on CIDER to get info

;; Copyright © 2014 Jon Pither

;; Author: Jon Pither <jon.pither@gmail.com>
;; URL: http://www.github.com/jonpither/cider-spy
;; Version: 0.1.0
;; Keywords: languages, clojure, cider, nrepl
;; Package-Requires: ((clojure-mode "2.0.0") (cider "0.5.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

(require 'cider-interaction)

(defun cider-spy-summary ()
  (interactive)
  (let ((buffer (cider-popup-buffer "*cider spy*" t)))
    (with-current-buffer buffer
        (cider-spy-buffer-mode))
    (nrepl-send-request (list "op" "summary")
                        (nrepl-make-response-handler
                         buffer
                         (lambda (buffer str)
                           (cider-emit-into-popup-buffer buffer str))
                         '()
                         (lambda (buffer _str)
                           (cider-emit-into-popup-buffer buffer "Oops"))
                         '()))))

(defun cider-spy-refresh ()
  "Refresh current buffer to match spy data."
  (interactive)
  (with-current-buffer "*cider spy*"
    (cider-emit-into-popup-buffer (current-buffer) "Oops")))

(defvar cider-spy-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'cider-spy-refresh)
    map))

(define-derived-mode cider-spy-buffer-mode cider-popup-buffer-mode
  "Cider-Spy"
  "Cider Spy Buffer Mode.
\\{cider-spy-mode-buffer-mode-map}
\\{cider-popup-buffer-mode-map}"
  (setq-local truncate-lines t))

(provide 'cider-spy)
