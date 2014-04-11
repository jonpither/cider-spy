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
(require 'json)
(require 'dash)

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

(defconst cider-spy-summary-sections
  '((devs "Devs Hacking:" cider-spy-section-devs-hacking)
    (session "Your Session:" cider-spy-section-session)
    (nses-loaded "Your Namespaces Loaded:" cider-spy-section-nses-loaded)
    (ns-trail "Your Namespace Trail:" cider-spy-section-ns-trail)
;;    (fns "Your Function Calls:" identity)
    )
  "The CIDER-SPY summary sections used for presentation.")

(defun cider-spy-section-devs-hacking (section-data)
  "Display string for devs hacking."
  (car (mapcar 'identity section-data)))

(defun cider-spy-section-nses-loaded (section-data)
  "Display string for namespaces loaded."
  (setq foo section-data)
  (mapconcat (lambda (ns-freq)
               (format "%s (%s times)"
                       (car ns-freq)
                       (cdr ns-freq)))
             section-data "\n  "))

(defun cider-spy-section-ns-trail (section-data)
  "Display string for namespace trail."
  (mapconcat (lambda (m)
               (format "%s (%s)"
                       (cdr (assoc 'ns m))
                       (let ((seconds (cdr (assoc 'seconds m))))
                         (if seconds
                             (format "%s seconds" seconds)
                           "Am here"))))
             section-data "\n  "))

(defun cider-spy-section-session (section-data)
  "Display info about session."
  (format "Started %s, uptime: %s seconds."
          (cdr (assoc 'started section-data))
          (cdr (assoc 'seconds section-data))))

;; todo indent-region?

(defvar-local cider-spy-sections nil
  "CIDER SPY sections in the *CIDER-SPY-BUFFER*")

(cl-defstruct cider-spy-section
  beginning end)

(defun cider-spy-insert-buffer-contents
  (buffer spy-data)
  "Insert SPY-DATA summary information into BUFFER.
   We reset cider-spy-sections, and add sections as children."
  (with-current-buffer buffer
    (setq cider-spy-sections '())
    (dolist (section-def cider-spy-summary-sections)
      (let ((section (assoc (car section-def) spy-data)))
        (when (> (point) 1)
          (insert-string "\n"))
        (when section
          (let ((spy-section (make-cider-spy-section
                              :beginning (point))))
            (insert-string
             (format "%s\n  %s\n"
                     (cadr section-def)
                     (funcall (cadr (cdr section-def))
                              (cdr section))))
            (setf (cider-spy-section-end spy-section) (point-marker))
            (setf cider-spy-sections
                  (nconc cider-spy-sections (list spy-section)))))))))

(defun cider-spy-next-section ()
  (interactive)
  (with-current-buffer (get-buffer "*cider spy*")
    (let ((next-s (car (-filter (lambda (s)
                                  (> (cider-spy-section-beginning s)
                                     (point)))
                                cider-spy-sections))))
      (when next-s
        (goto-char (cider-spy-section-beginning next-s))))))

(defun cider-spy-previous-section ()
  (interactive)
  (with-current-buffer (get-buffer "*cider spy*")
    (let ((next-s (car (-filter (lambda (s)
                                  (< (cider-spy-section-beginning s)
                                     (point)))
                                (reverse cider-spy-sections)))))
      (when next-s
        (goto-char (cider-spy-section-beginning next-s))))))

(defun cider-spy-find-section-at-point ()
  "Find the CIDER-SPY secton at point."
  (car (-filter (lambda (s)
                  (= (cider-spy-section-beginning s)
                     (point)))
                cider-spy-sections)))

(defun cider-spy-toggle-section-hidden ()
  "Hide everything after the first line of a section."
  (interactive)
  (let ((section (cider-spy-find-section-at-point)))
    (let ((inhibit-read-only t)
          (beg (save-excursion
                 (goto-char (cider-spy-section-beginning section))
                 (forward-line)
                 (point)))
          (end (cider-spy-section-end section)))
      (when (< beg end)
        (put-text-property beg end 'invisible t)))))

(defun cider-spy-refresh-buffer (buffer str)
  "Update the cider spy popup buffer, wiping it first."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cider-spy-insert-buffer-contents
       buffer (json-read-from-string str))
      (font-lock-fontify-buffer))))

;; TODO check indent-sexp, maybe I don't have to manually indent like I am
(defun cider-spy-connect-to-hub ()
  "Connect to the CIDER-SPY-HUB"
  (when cider-spy-hub-endpoint
    (let ((buffer (cider-popup-buffer "*cider spy hub*" t)))
      (cider-emit-into-popup-buffer buffer "CIDER SPY asked CIDER-SPY-NREPL to connect to CIDER SPY HUB...")
      (nrepl-send-request
       (append (list "op" "cider-spy-hub-connect"
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
    (nrepl-send-request (list "op" "cider-spy-summary"
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

(defun cider-spy-reset ()
  "Reset CIDER-SPY tracking used for *cider-spy* buffer."
  (interactive)

  (nrepl-send-request
   (append (list "op" "cider-spy-reset"
                 "session" (nrepl-current-session)))
   nil))

(defvar cider-spy-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "g" 'cider-spy-summary)
    (define-key map "r" 'cider-spy-reset)
    (define-key map "n" 'cider-spy-next-section)
    (define-key map "p" 'cider-spy-previous-section)
    (define-key map "TAB" 'cider-spy-toggle-section-hidden)
    (define-key map "t" 'cider-spy-toggle-section-hidden)
    map))

(define-derived-mode cider-spy-buffer-mode cider-popup-buffer-mode
  "Cider-Spy"
  "Cider Spy Buffer Mode.
\\{cider-spy-mode-buffer-mode-map}
\\{cider-popup-buffer-mode-map}"
  (setq-local truncate-lines t))

;; TODO use a regexp
(font-lock-add-keywords 'cider-spy-buffer-mode
                        '(("Your .*:" . font-lock-function-name-face)
                          ("Devs Hacking:" . font-lock-keyword-face)))

;; (after-init-hook)?
(add-hook 'cider-repl-mode-hook 'cider-spy-connect-to-hub)

(provide 'cider-spy)
