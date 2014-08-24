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
(require 'nrepl-client)
(require 'json)
(require 'dash)

(defcustom cider-spy-hub-alias nil
  "Set `cider-spy-hub-alias' for a handle to identify REPL session owner in the
CIDER-SPY hub."
  :type 'string
  :group 'cider-spy)

(defvar-local cider-spy-summary-buffer nil
  "Current CIDER SPY SUMMARY BUFFER for nrepl-connection.")

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
  (lexical-let ((connection-buffer-name (generate-new-buffer-name "*cider spy hub*")))
    (nrepl-send-request
     (append (list "op" "cider-spy-hub-connect"
                   "session" (nrepl-current-session))
             (when cider-spy-hub-alias
               (list "hub-alias" cider-spy-hub-alias)))
     (lambda (response)
       (nrepl-dbind-response response (value err from msg)
         (cond (msg
                (cider-spy-msg-popup from msg))
               (value
                (cider-emit-into-popup-buffer (get-buffer-create connection-buffer-name) (concat value "\n")))
               (err
                (cider-emit-into-popup-buffer (get-buffer-create connection-buffer-name) (concat "OOPS\n" err "\n")))))))))

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

(defun cider-spy-refresh-summary ()
  "Refresh the cider spy summary buffer."
  (interactive)
  (cider-spy-attach-nrepl-response-handler))

(defun cider-spy-summary ()
  "Create *cider-spy* buffer and attach listener.
   We assign a cider-spy-summary buffer to the nrepl-connection-buffer."
  (interactive)
  (with-current-buffer (nrepl-current-connection-buffer)
    (unless (and cider-spy-summary-buffer (buffer-name cider-spy-summary-buffer))
      (let ((summary-buffer (get-buffer-create (generate-new-buffer-name "*cider spy*"))))
        (with-current-buffer summary-buffer
          (cider-spy-buffer-mode))
        (setq cider-spy-summary-buffer summary-buffer)))
    (with-current-buffer cider-spy-summary-buffer
      (cider-spy-refresh-summary))
    (pop-to-buffer cider-spy-summary-buffer)))

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

(defun cider-spy-hub-disconnect ()
  "Disconnect from CIDER-SPY-HUB."
  (interactive)
  (nrepl-send-request
   (list "op" "cider-spy-hub-disconnect"
         "session" (nrepl-current-session))
   nil))

(defvar cider-spy-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "g") 'cider-spy-refresh-summary)
    (define-key map (kbd "r") 'cider-spy-reset)
    (define-key map (kbd "n") 'cider-spy-next-section)
    (define-key map (kbd "p") 'cider-spy-previous-section)
    (define-key map (kbd "a") 'cider-spy-alias)
    (define-key map (kbd "s") 'cider-spy-send-to-dev)
    (define-key map (kbd "d") 'cider-spy-hub-disconnect)
    (define-key map (kbd "TAB") 'cider-spy-toggle-section-hidden)
    (define-key map (kbd "RET") 'cider-spy-visit-section)
    map))

(define-derived-mode cider-spy-buffer-mode cider-popup-buffer-mode
  "Cider-Spy"
  "Cider Spy Buffer Mode.
\\{cider-spy-mode-buffer-mode-map}"
  (setq-local truncate-lines t))

(font-lock-add-keywords 'cider-spy-buffer-mode
                        '(("Your .*:" . font-lock-function-name-face)
                          ("Devs Hacking:" . font-lock-keyword-face)))

(add-hook 'cider-repl-mode-hook 'cider-spy-connect-to-hub)

;; cider-spy msg:

(defvar cider-spy-msg-popup-buffer-name-template "*hub %s*"
  "Buffer name for message popup.")

(defvar cider-spy-recipient-id nil)

(defvar-local cider-spy-msg-prompt-start nil
  "Marker for the start of prompt.")

(defvar-local cider-spy-msg-input-start nil
  "Marker for the start of input.")

(defun cider-spy-msg-send (recipient-id msg)
  (interactive)
  (nrepl-send-request
   (list "op" "cider-spy-hub-send-msg"
         "session" (nrepl-current-session)
         "recipient" (symbol-name recipient-id)
         "message" msg)
   nil)
  (message "Sent message to %s." cider-spy-recipient-id))

(defun cider-spy-msg-reset-markers ()
  "Reset all CIDER-SPY-MSG markers."
  (dolist (markname '(cider-spy-msg-prompt-start
                      cider-spy-msg-input-start))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))

(defun cider-spy-msg-get-popup (from)
  (let ((buffer-name (format cider-spy-msg-popup-buffer-name-template from)))
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        ;; Initialise message buffer
        (cider-spy-popup-mode)
        (cider-spy-msg-reset-markers)
        (cider-spy-msg--insert-prompt)))
    (get-buffer buffer-name)))

(defun cider-spy-msg--insert-msg (from msg)
  "Insert the msg into msg buffer"
  (goto-char cider-spy-msg-prompt-start)
  (unless (bolp) (insert-before-markers "\n"))
  (let ((prompt-start (point)))
    (insert-before-markers from)
    (insert-before-markers " >> ")
    (let ((overlay (make-overlay prompt-start (- (point) 1))))
      (overlay-put overlay 'face 'font-lock-keyword-face)))
  (insert-before-markers msg)
  (goto-char (max-char)))

(defun cider-spy-msg--insert-prompt ()
  "Insert a prompt into msg buffer"
  (goto-char (max-char))
  (set-marker cider-spy-msg-prompt-start (point))
  (unless (bolp) (insert "\n"))
  (insert "Me >> ")
  (set-marker cider-spy-msg-input-start (point))
  (let ((overlay (make-overlay cider-spy-msg-prompt-start
                               cider-spy-msg-input-start)))
    (overlay-put overlay 'face 'font-lock-keyword-face)))

(defun cider-spy-msg-return ()
  "Hit return to send a message to user."
  (interactive)
  (goto-char (point-max))
  (let ((msg (buffer-substring cider-spy-msg-input-start (point))))
    (cider-spy-msg--insert-prompt)
    (cider-spy-msg-send cider-spy-recipient-id msg)))

(defun cider-spy-msg-popup (from msg)
  (with-current-buffer (cider-spy-msg-get-popup from)
    (cider-spy-msg--insert-msg from msg)))

(defun cider-spy-msg-edit (id alias)
  (interactive)
  (with-current-buffer (cider-spy-msg-get-popup alias)
    (setq cider-spy-recipient-id id)
    (pop-to-buffer (current-buffer))))

(defvar cider-spy-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'cider-spy-msg-return)
    map))

(define-derived-mode cider-spy-popup-mode text-mode "Cider Spy Popup")

(provide 'cider-spy)

;;; cider-spy.el ends here


;; What is goingon here:

;; This does funky shit:

;; (with-current-buffer (get-buffer-create "foo")
;;   (process-file "git" nil t nil "diff-index" "HEAD")
;;   (goto-char (point-min)))


;; (magit-git-insert-section (staged "Staged changes:")
;;             (apply-partially #'magit-wash-raw-diffs t)
;;   "diff-index" "--cached" base)

;; ;; git diff-index
;; ;; Need funkage from magit-wash-diffs
;; (defun foo-diff ()
;;   (when (looking-at
;;          ":\\([0-7]+\\) \\([0-7]+\\) [0-9a-f]+ [0-9a-f]+ \\(.\\)[0-9]*\t\\([^\t\n]+\\)$")
;;     (let ((file (magit-decode-git-path (match-string-no-properties 4)))
;;           (status (cl-ecase (string-to-char (match-string-no-properties 3))
;;                     (?A 'new)
;;                     (?C 'copy)
;;                     (?D 'deleted)
;;                     (?M 'modified)
;;                     (?T 'typechange)
;;                     (?U 'unmerged)
;;                     (?X 'unknown))))
;;       (list file status))))

;; (with-current-buffer (get-buffer-create "foo")
;;   (foo-diff)
;;   (when (re-search-forward "^ ?\\([0-9]+ +files? change[^\n]*\n\\)" nil t)
;;     (message "sad")))
