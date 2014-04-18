;;; cider-spy.el --- Spy on CIDER to get info

;; Copyright © 2014 Jon Pither

;; Author: Jon Pither <jon.pither@gmail.com>
;; URL: http://www.github.com/jonpither/cider-spy
;; Version: 0.1.0
;; Keywords: languages, clojure, cider, nrepl
;; Package-Requires: ((cider "0.5.0"))

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

(cl-defstruct cider-spy-section-def
  type label extract-fn display-fn)

(defconst cider-spy-summary-sections
  (list (make-cider-spy-section-def
         :type 'devs
         :label "Devs Hacking:"
         :display-fn 'cider-spy-section-devs)
        (make-cider-spy-section-def
         :type 'session
         :label "Your Session:"
         :extract-fn 'list
         :display-fn 'cider-spy-section-session)
        (make-cider-spy-section-def
         :type 'nses-loaded
         :label "Your Namespaces Loaded:"
         :display-fn 'cider-spy-section-nses-loaded)
        (make-cider-spy-section-def
         :type 'ns-trail
         :label "Your Namespace Trail:"
         :display-fn 'cider-spy-section-ns-trail)
        (make-cider-spy-section-def
         :type 'fns
         :label "Your Function Calls:"
         :display-fn 'cider-spy-section-fns))
  "The CIDER-SPY summary sections used for presentation.")

(defvar-local cider-spy-root-section nil
  "CIDER SPY sections are a hierarchy in the *CIDER-SPY-BUFFER*")

(cl-defstruct cider-spy-section
  type beginning end hidden children)

(defmacro cider-spy-with-section (parent type &rest body)
  `(let ((spy-section (make-cider-spy-section
              :beginning (point)
              :type ,type)))
     ,@body
     (setf (cider-spy-section-end spy-section) (point-marker))
     (setf (cider-spy-section-children ,parent)
           (nconc (cider-spy-section-children ,parent)
                  (list spy-section)))))

(defun cider-spy-section-extract-freqencies (section-data)
  "Expects a list of pairs, the second of which is the metric value."
  (-sort (lambda (v1 v2)
           (> (cdr v1) (cdr v2))) section-data))

(defun cider-spy-section-frequency (v)
  "Display frequency metric."
  (format "%s (%s times)" (car v) (cdr v)))

(defun cider-spy-section-session (cider-spy-section section-data)
  "Display info about session."
  (insert-string
   (format "\n  Started %s, uptime: %s seconds."
           (cdr (assoc 'started section-data))
           (cdr (assoc 'seconds section-data)))))

(defun cider-spy-section-ns-trail (cider-spy-section section-data)
  "Display string for namespace trail."
  (dolist (m (mapcar 'identity section-data))
    (insert-string "\n")
    (cider-spy-with-section
     cider-spy-section 'ns-breadcrumb
     (insert-string
      (format "%s (%s)"
              (cdr (assoc 'ns m))
              (let ((seconds (cdr (assoc 'seconds m))))
                (if seconds
                    (format "%s seconds" seconds)
                  "Am here")))))))

(defun cider-spy-section-frequencies (cider-spy-section section-data child-type)
  (dolist (s (cider-spy-section-extract-freqencies section-data))
    (insert-string "\n")
    (cider-spy-with-section
     cider-spy-section child-type
     (insert-string (cider-spy-section-frequency s)))))

(defun cider-spy-section-nses-loaded (cider-spy-section section-data)
  (cider-spy-section-frequencies cider-spy-section section-data 'ns-loaded))

(defun cider-spy-section-fns (cider-spy-section section-data)
  (cider-spy-section-frequencies cider-spy-section section-data 'fn))

(defun cider-spy-section-devs (cider-spy-section section-data)
  (dolist (s (mapcar 'identity section-data))
    (insert-string "\n")
    (cider-spy-with-section
     cider-spy-section 'dev (insert-string s))))

(defun cider-spy-insert-buffer-contents
  (buffer spy-data)
  "insert SPY-DATA summary information into BUFFER.
   We reset cider-spy-sections and add sections as children."
  (with-current-buffer buffer
    (setq cider-spy-root-section (make-cider-spy-section
                                  :type 'root))
    (dolist (section-def cider-spy-summary-sections)
      (let* ((section (assoc (cider-spy-section-def-type section-def) spy-data))
             (section-data (and section (cdr section))))
        (when (> (point) 1)
          (insert-string "\n"))
        (when section-data
          (cider-spy-with-section
           cider-spy-root-section (cider-spy-section-def-type section-def)
           (insert-string (cider-spy-section-def-label section-def))
           (funcall (cider-spy-section-def-display-fn section-def)
                    spy-section section-data)
           (dolist (s (cider-spy-section-children spy-section))
             (indent-region
              (cider-spy-section-beginning s)
              (cider-spy-section-end s) 2))
           (insert-string "\n")))))))

(defun cider-spy-refresh-buffer (buffer str)
  "Update the cider spy popup buffer, wiping it first."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cider-spy-insert-buffer-contents
       buffer (json-read-from-string str))
      (font-lock-fontify-buffer))))

(defun cider-spy-descendent-sections (section)
  "Return a flattened list of sections."
  (append (list section)
          (-mapcat 'cider-spy-descendent-sections (cider-spy-section-children section))))

(defun cider-spy-next-section (buffer)
  (interactive (list (get-buffer "*cider spy*")))
  (with-current-buffer buffer
    (let ((next-s (car (-filter (lambda (s)
                                  (and (cider-spy-section-beginning s)
                                       (> (cider-spy-section-beginning s)
                                          (point))))
                                (cider-spy-descendent-sections cider-spy-root-section)))))
      (when next-s
        (goto-char (cider-spy-section-beginning next-s))))))

(defun cider-spy-previous-section (buffer)
  (interactive (list (get-buffer "*cider spy*")))
  (with-current-buffer buffer
    (let ((next-s (car (-filter (lambda (s)
                                  (and (cider-spy-section-beginning s)
                                       (< (cider-spy-section-beginning s)
                                          (point))))
                                (reverse (cider-spy-descendent-sections cider-spy-root-section))))))
      (when next-s
        (goto-char (cider-spy-section-beginning next-s))))))

(defun cider-spy-find-section-at-point ()
  "Find the CIDER-SPY secton at point."
  (car (-filter (lambda (s)
                  (and (cider-spy-section-beginning s)
                       (= (cider-spy-section-beginning s)
                          (point))))
                (cider-spy-descendent-sections cider-spy-root-section))))

(defun cider-spy-toggle-section-hidden ()
  "Hide everything after the first line of a section."
  (interactive)
  (let* ((section (cider-spy-find-section-at-point)))
    (when section
      (let ((hidden (not (cider-spy-section-hidden section))))
        (setf (cider-spy-section-hidden section) hidden)
        (let ((inhibit-read-only t)
              (beg (save-excursion
                     (goto-char (cider-spy-section-beginning section))
                     (forward-line)
                     (point)))
              (end (cider-spy-section-end section)))
          (when (< beg end)
            (put-text-property beg end 'invisible hidden)))))))

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
                              "session" (nrepl-current-session))
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
    (define-key map (kbd "g") 'cider-spy-summary)
    (define-key map (kbd "r") 'cider-spy-reset)
    (define-key map (kbd "n") 'cider-spy-next-section)
    (define-key map (kbd "p") 'cider-spy-previous-section)
    (define-key map (kbd "TAB") 'cider-spy-toggle-section-hidden)
    map))

(define-derived-mode cider-spy-buffer-mode cider-popup-buffer-mode
  "Cider-Spy"
  "Cider Spy Buffer Mode.
\\{cider-spy-mode-buffer-mode-map}
\\{cider-popup-buffer-mode-map}"
  (setq-local truncate-lines t))

(font-lock-add-keywords 'cider-spy-buffer-mode
                        '(("Your .*:" . font-lock-function-name-face)
                          ("Devs Hacking:" . font-lock-keyword-face)))

(add-hook 'cider-repl-mode-hook 'cider-spy-connect-to-hub)

(provide 'cider-spy)

;;; cider-spy.el ends here
