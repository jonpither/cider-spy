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

(defcustom cider-spy-auto-refresh t
  "When `cider-spy-auto-refresh' is set to t, updates from the nREPL server
will appear automatically in the CIDER SPY buffer."
  :type 'boolean
  :group 'cider-spy)

(defcustom cider-spy-hub-endpoint t
  "Set `cider-spy-hub-endpoint' to designate a CIDER-SPY hub for sharing information
between independent REPL sessions.
The format is '(host port)."
  :type 'list
  :group 'cider-spy)

(defcustom cider-spy-hub-alias t
  "Set `cider-spy-hub-alias' for a handle to identify REPL session owner in the
CIDER-SPY hub."
  :type 'string
  :group 'cider-spy)

(defun cider-spy-refresh-buffer (buffer str)
  "Emit into the cider spy popup buffer, wiping it first."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (cider-emit-into-popup-buffer buffer str))

;; Current thinking (with kids in a coffee shop, never a good time_
;;  CIDER-SPY in emacs should control hub connection behavour.
;;  This way it's more transparent and configurable, and less complex
;;  It also makes cider-nrepl-server dumber.
;;  I'll need to handle the user bombarding the server with connection requests,
;;  perhaps only ensure one is in progress at any one time..
;;  One ideas is that server keeps trying if this connect-command is run.
;;  TODO - probably not a popup buffer, more of a background buffer
;;  TODO - test the error handling - be quite nice to report mal-connection there

(defun cider-spy-connect-to-hub ()
  "Connect to the CIDER-SPY-HUB"
  (when cider-spy-hub-endpoint
    (let ((buffer (cider-popup-buffer "*cider spy hub*" t)))
      (cider-emit-into-popup-buffer buffer "CIDER SPY asked CIDER-SPY-NREPL to connect to CIDER SPY HUB...")
      (nrepl-send-request
       (append (list "op" "spy-hub-connect"
                     "session" (nrepl-current-session)
                     "hub-host" (car cider-spy-hub-endpoint)
                     "hub-port" (number-to-string (cadr cider-spy-hub-endpoint)))
               (when cider-spy-hub-alias
                 (list "hub-alias" cider-spy-hub-alias)))
       (nrepl-make-response-handler
        buffer
        (lambda (buffer str)
          (cider-emit-into-popup-buffer buffer (concat "\n" str)))
        '()
        (lambda (buffer _str)
          (cider-emit-into-popup-buffer "Oops"))
        '())))))

(defun cider-spy-attach-nrepl-response-handler ()
  "Attach an nREPL response handler.
When a response comes from nREPL relevant to the CIDER-SPY summary operation,
the current buffer will be updated accordingly."
  (let ((buffer (current-buffer)))
    (nrepl-send-request (list "op" "summary"
                              "session" (nrepl-current-session)
                              "auto-refresh" (if cider-spy-auto-refresh "true" "false"))
                        (nrepl-make-response-handler
                         buffer
                         (lambda (buffer str)
                           (cider-spy-refresh-buffer buffer (concat str "\n")))
                         '()
                         (lambda (buffer _str)
                           (cider-spy-refresh-buffer buffer "Oops"))
                         '()))))

(defun cider-spy-summary ()
  "Create *cider-spy* buffer and attach listener."
  (interactive)
  (with-current-buffer (cider-popup-buffer "*cider spy*" t)
    (cider-spy-buffer-mode)
    (cider-spy-attach-nrepl-response-handler)))

(defvar cider-spy-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'cider-spy-summary)
    map))

(define-derived-mode cider-spy-buffer-mode cider-popup-buffer-mode
  "Cider-Spy"
  "Cider Spy Buffer Mode.
\\{cider-spy-mode-buffer-mode-map}
\\{cider-popup-buffer-mode-map}"
  (setq-local truncate-lines t))

;; TODO use a regexp
(font-lock-add-keywords 'cider-spy-buffer-mode
                        '(("Your namespace trail:" . font-lock-keyword-face)
                          ("Your function calls:" . font-lock-function-name-face)
                          ("Your files loaded:" . font-lock-function-name-face)
                          ("Devs hacking:" . font-lock-function-name-face)))

;; (after-init-hook)?
(add-hook 'cider-repl-mode-hook 'cider-spy-connect-to-hub)

(provide 'cider-spy)
