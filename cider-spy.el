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
(require 'cider-spy-msg)
(require 'json)
(require 'dash)

(defcustom cider-spy-hub-alias nil
  "Set `cider-spy-hub-alias' for a handle to identify REPL session owner in the
CIDER-SPY hub."
  :type 'string
  :group 'cider-spy)

(cl-defstruct cider-spy-section-def
  type label extract-fn display-fn jump-fn)

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
         :display-fn 'cider-spy-section-fns)
        (make-cider-spy-section-def
         :type 'fn
         :jump-fn 'cider-spy-visit-form)
        (make-cider-spy-section-def
         :type 'ns-breadcrumb
         :jump-fn 'cider-spy-visit-ns)
        (make-cider-spy-section-def
         :type 'ns-loaded
         :jump-fn 'cider-spy-visit-form)
        (make-cider-spy-section-def
         :type 'dev))
  "The CIDER-SPY summary sections used for presentation.")

(defconst cider-spy-root-sections
  '(devs session nses-loaded ns-trail fns))

(defvar-local cider-spy-root-section nil
  "CIDER SPY sections are a hierarchy in the *CIDER-SPY-BUFFER*")

(cl-defstruct cider-spy-section
  type data beginning end hidden children)

(defmacro cider-spy-with-section (parent type data &rest body)
  `(let ((spy-section (make-cider-spy-section
              :beginning (point)
              :type ,type
              :data ,data)))
     ,@body
     (setf (cider-spy-section-end spy-section) (point-marker))
     (setf (cider-spy-section-children ,parent)
           (nconc (cider-spy-section-children ,parent)
                  (list spy-section)))))

(defun cider-spy-section-session (cider-spy-section section-data)
  "Display info about session."
  (insert-string
   (format "\n  Started %s, uptime: %s seconds."
           (cdr (assoc 'started section-data))
           (cdr (assoc 'seconds section-data)))))

(defun cider-spy-section-ns-trail (cider-spy-section section-data)
  "Display string for namespace trail."
  (let ((section-data (mapcar 'identity section-data)))
    (when section-data
      (dolist (m section-data)
        (insert-string "\n")
        (cider-spy-with-section
         cider-spy-section 'ns-breadcrumb m
         (insert-string
          (format "%s (%s)"
                  (cdr (assoc 'ns m))
                  (let ((seconds (cdr (assoc 'seconds m))))
                    (if seconds
                        (format "%s seconds" seconds)
                      "Am here"))))
         (indent-region
          (cider-spy-section-beginning spy-section)
          (max-char) 2))))))

(defun cider-spy-section-frequency (v)
  "Display frequency metric."
  (format "%s (%s times)" (car v) (cdr v)))

(defun cider-spy-section-extract-freqencies (section-data)
  "Expects a list of pairs, the second of which is the metric value."
  (-sort (lambda (v1 v2)
           (> (cdr v1) (cdr v2))) section-data))

(defun cider-spy-section-frequencies (cider-spy-section section-data child-type)
  (dolist (s (cider-spy-section-extract-freqencies section-data))
    (insert-string "\n")
    (cider-spy-with-section
     cider-spy-section child-type s
     (insert-string (cider-spy-section-frequency s))
     (indent-region
      (cider-spy-section-beginning spy-section)
      (max-char) 2))))

(defun cider-spy-section-nses-loaded (cider-spy-section section-data)
  (cider-spy-section-frequencies cider-spy-section section-data 'ns-loaded))

(defun cider-spy-section-fns (cider-spy-section section-data)
  (cider-spy-section-frequencies cider-spy-section section-data 'fn))

(defun cider-spy-section-devs (cider-spy-section section-data)
  (dolist (s (mapcar 'identity section-data))
    (insert-string "\n")
    (cider-spy-with-section
     cider-spy-section 'dev s
     (insert-string
      (format "%s: %s" (cdr (assoc 'alias (cdr s))) (cdr (assoc 'nses (cdr s)))))
     (indent-region
      (cider-spy-section-beginning spy-section)
      (max-char) 2))))

(defun cider-spy-def-for-type (type)
  "Returns the CIDER-SPY section definition for the given type."
  (car (-filter (lambda (s)
                  (eq (cider-spy-section-def-type s) type))
                cider-spy-summary-sections)))

(defun cider-spy-insert-buffer-contents
  (buffer spy-data)
  "insert SPY-DATA summary information into BUFFER.
   We reset cider-spy-sections and add sections as children."
  (with-current-buffer buffer
    (setq cider-spy-root-section (make-cider-spy-section
                                  :type 'root))
    (dolist (root-section-type cider-spy-root-sections)
      (let* ((section-def (cider-spy-def-for-type root-section-type))
             (section (assoc (cider-spy-section-def-type section-def) spy-data))
             (section-data (and section (cdr section))))
        (when (car (mapcar 'identity section-data))
          (when (> (point) 1)
            (insert-string "\n"))
          (cider-spy-with-section
           cider-spy-root-section (cider-spy-section-def-type section-def) section-data
           (insert-string (cider-spy-section-def-label section-def))
           (funcall (cider-spy-section-def-display-fn section-def)
                    spy-section section-data)
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

(defmacro cider-spy-with-section-at-point (&rest body)
  `(let ((section (cider-spy-find-section-at-point)))
     (when section
       ,@body)))

(defun cider-spy-visit-ns ()
  (cider-spy-with-section-at-point
   (cider-jump-to-def
    (cdr (assoc 'ns (cider-spy-section-data section))))))

(defun cider-spy-send-to-dev ()
  (interactive)
  (cider-spy-with-section-at-point
   (when (eq 'dev (cider-spy-section-type section))
     (cider-spy-msg-edit (car (cider-spy-section-data section))
                         (cdr (assoc 'alias (cdr (cider-spy-section-data section))))))))

(defun cider-spy-visit-form ()
  (cider-spy-with-section-at-point
   (cider-jump-to-def
    (car (cider-spy-section-data section)))))

(defun cider-spy-visit-section ()
  (interactive)
  (cider-spy-with-section-at-point
   (let ((jump-fn (cider-spy-section-def-jump-fn
                   (cider-spy-def-for-type
                    (cider-spy-section-type
                     section)))))
     (when jump-fn
       (funcall jump-fn)))))

(defun cider-spy-connect-to-hub ()
  "Connect to the CIDER-SPY-HUB"
  (interactive)
  (nrepl-send-request
   (append (list "op" "cider-spy-hub-connect"
                 "session" (nrepl-current-session))
           (when cider-spy-hub-alias
             (list "hub-alias" cider-spy-hub-alias)))
   (lambda (response)
     (nrepl-dbind-response response (value err)
       (cond (value
              (cider-emit-into-popup-buffer (get-buffer-create "*cider spy hub*") (concat value "\n")))
             (err
              (cider-emit-into-popup-buffer (get-buffer-create "*cider spy hub*") (concat "OOPS\n" err "\n"))))))))

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
   (list "op" "cider-spy-reset"
         "session" (nrepl-current-session))
   nil))

(defun cider-spy-alias ()
  "Reset CIDER-SPY tracking used for *cider-spy* buffer."
  (interactive)

  (let ((alias (read-string "Set Alias: ")))
    (nrepl-send-request
     (list "op" "cider-spy-hub-alias"
           "session" (nrepl-current-session)
           "alias" alias)
     nil)))

(defvar cider-spy-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "g") 'cider-spy-summary)
    (define-key map (kbd "r") 'cider-spy-reset)
    (define-key map (kbd "n") 'cider-spy-next-section)
    (define-key map (kbd "p") 'cider-spy-previous-section)
    (define-key map (kbd "a") 'cider-spy-alias)
    (define-key map (kbd "s") 'cider-spy-send-to-dev)
    (define-key map (kbd "TAB") 'cider-spy-toggle-section-hidden)
    (define-key map (kbd "RET") 'cider-spy-visit-section)
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
