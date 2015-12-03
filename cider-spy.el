;;; cider-spy.el --- Spy on CIDER to get info

;; Copyright © 2014 Jon Pither

;; Author: Jon Pither <jon.pither@gmail.com>
;; URL: http://www.github.com/jonpither/cider-spy
;; Version: 0.1.0
;; Keywords: languages, clojure, cider, nrepl
;; Package-Requires: ((cider "0.5.0") (dash "2.5.0") (cl-lib "0.5"))

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

;;; Commentary:

;; Get visibility on CIDER nREPL sessions and help developers in teams
;; to share information, send code snippets and text exchanges to each
;; other etc.

;; # Installation
;;
;; ## Prerequisites
;;
;; You need the [`CIDER-SPY-NREPL`](https://github.com/jonpither/cider-spy-nrepl)
;; middleware. See installation instructions there.

;; ## Basic configuration
;;
;; It's available on [melpa](http://melpa.milkbox.net/):
;;
;;     M-x package-install cider-spy
;;
;; You can also install the dependencies on your own, and just dump
;; clj-refactor in your path somewhere:
;;
;;  - <a href="https://github.com/magnars/dash.el">dash.el</a>
;;  - <a href="https://github.com/clojure-emacs/cider">cider</a>
;;
;; # Setup
;;
;;     (require 'cider-spy)
;;
;; All actions in `CIDER-SPY` are triggered from the `CIDER-SPY` summary page.
;; To access the summary page:
;;
;;     M-x cider-spy-summary
;;
;; It can be useful to setup a global binding for the summary page for frequent
;; access, such as <kbd>C-c C-s</kbd>.

;; ## Configuration for the HUB

;; If you want the developer interactivity behavours then you need a run a `CIDER-SPY-HUB`.
;; See the documentation for how to set one up.
;;
;; Give yourself an alias on the hub:
;;
;;     (setq cider-spy-hub-alias "my-alias")
;;

;; # Keyboard Shortcuts
;;
;; These shortcuts are available on the `CIDER-SPY` summary buffer:
;;
;;  - `g` : Refresh the `*cider-spy*` buffer
;;  - `r` : Reset the tracking data underpinning the `*cider-spy*` buffer
;;  - `n` : Goto to next section
;;  - `p` : Goto to previous section
;;  - `a` : Set `CIDER-SPY-HUB` alias
;;  - `s` : Send message to another dev (when cursor is on a dev)
;;  - `d` : Disconnect from the `CIDER-SPY-HUB`
;;  - `RETURN` : Visit section
;;  - `TAB` : Toggle section visibility

;;; Code:

(defgroup cider-spy nil
  "Accompaniment to CIDER allowing developers to share information."
  :prefix "cider-spy"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/jonpither/cider-spy")
  :link '(emacs-commentary-link :tag "Commentary" "cider-spy"))

(require 'cider-interaction)
(require 'nrepl-client)
(require 'json)
(require 'dash)
(require 'cl-lib)
(require 'bookmark)
(eval-when-compile
  (require 'cl))

(defcustom cider-spy-hub-alias nil
  "Set `cider-spy-hub-alias' for a handle to identify REPL session owner in the
CIDER-SPY hub."
  :type 'string
  :group 'cider-spy)

(make-variable-buffer-local
 (defvar cider-spy-summary-buffer nil
   "Current CIDER SPY SUMMARY BUFFER for nrepl-connection."))

(make-variable-buffer-local
 (defvar cider-spy-hub-connection-buffer nil
   "Current CIDER SPY CONNECTION BUFFER for nrepl-connection."))

(make-variable-buffer-local
 (defvar cider-spy-summary-buffer-nrepl-connection nil
   "Current nrepl-connection for CIDER SPY SUMMARY BUFFER."))

(make-variable-buffer-local
 (defvar cider-spy-hub-registered-alias nil
   "The registered alias on the CIDER SPY HUB."))

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

(make-variable-buffer-local
 (defvar cider-spy-root-section nil
   "CIDER SPY sections are a hierarchy in the *CIDER-SPY-BUFFER*"))

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
  (insert
   (format "\n  Started %s, uptime: %s seconds."
           (cdr (assoc 'started section-data))
           (cdr (assoc 'seconds section-data)))))

(defun cider-spy-section-ns-trail (cider-spy-section section-data)
  "Display string for namespace trail."
  (let ((section-data (mapcar 'identity section-data)))
    (when section-data
      (dolist (m section-data)
        (insert "\n")
        (cider-spy-with-section
         cider-spy-section 'ns-breadcrumb m
         (insert
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
    (insert "\n")
    (cider-spy-with-section
     cider-spy-section child-type s
     (insert (cider-spy-section-frequency s))
     (indent-region
      (cider-spy-section-beginning spy-section)
      (max-char) 2))))

(defun cider-spy-section-nses-loaded (cider-spy-section section-data)
  (cider-spy-section-frequencies cider-spy-section section-data 'ns-loaded))

(defun cider-spy-section-fns (cider-spy-section section-data)
  (cider-spy-section-frequencies cider-spy-section section-data 'fn))

(defun cider-spy-section-devs (cider-spy-section section-data)
  (dolist (s (mapcar 'identity section-data))
    (insert "\n")
    (cider-spy-with-section
     cider-spy-section 'dev s
     (insert
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
            (insert "\n"))
          (cider-spy-with-section
           cider-spy-root-section (cider-spy-section-def-type section-def) section-data
           (insert (cider-spy-section-def-label section-def))
           (funcall (cider-spy-section-def-display-fn section-def)
                    spy-section section-data)
           (insert "\n")))))))

(defun cider-spy-refresh-buffer (buffer str)
  "Update the cider spy popup buffer, wiping it first."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cider-spy-insert-buffer-contents
       buffer (json-read-from-string (json-read-from-string str)))
      (font-lock-fontify-buffer))))

(defun cider-spy-descendent-sections (section)
  "Return a flattened list of sections."
  (append (list section)
          (-mapcat 'cider-spy-descendent-sections (cider-spy-section-children section))))

(defun cider-spy-next-section (buffer)
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (let ((next-s (car (-filter (lambda (s)
                                  (and (cider-spy-section-beginning s)
                                       (> (cider-spy-section-beginning s)
                                          (point))))
                                (cider-spy-descendent-sections cider-spy-root-section)))))
      (when next-s
        (goto-char (cider-spy-section-beginning next-s))))))

(defun cider-spy-previous-section (buffer)
  (interactive (list (current-buffer)))
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
   (cider-find-var
    (cdr (assoc 'ns (cider-spy-section-data section))))))

(defun cider-spy-send-to-dev ()
  (interactive)
  (let ((my-alias
         (with-current-buffer cider-spy-summary-buffer-nrepl-connection
           (with-current-buffer cider-spy-hub-connection-buffer
             cider-spy-hub-registered-alias))))
    (cider-spy-with-section-at-point
     (when (eq 'dev (cider-spy-section-type section))
       (cider-spy-msg-edit my-alias
                           (cdr (assoc 'alias (cdr (cider-spy-section-data section)))))))))

(defun cider-spy-visit-form ()
  (cider-spy-with-section-at-point
   (cider-find-var
    (symbol-name (car (cider-spy-section-data section))))))

(defun cider-spy-visit-section ()
  (interactive)
  (cider-spy-with-section-at-point
   (let ((jump-fn (cider-spy-section-def-jump-fn
                   (cider-spy-def-for-type
                    (cider-spy-section-type
                     section)))))
     (when jump-fn
       (funcall jump-fn)))))

(defvar cider-spy-request-counter 1000
  "Continuation serial number counter.")

(defun cider-spy--dev-registered (hub-connection-buffer hub-registered-alias)
  "Developer has become registered on the hub, this is their alias"
  (with-current-buffer hub-connection-buffer
    (setq cider-spy-hub-registered-alias hub-registered-alias)))

(defun cider-spy-connection-buffer-emit (buffer value)
  "Emit into BUFFER the provided VALUE."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (max-char))
      (unless (bolp) (insert "\n"))
      (insert (format "%s" value)))))

;;;###autoload
(defun cider-spy-connect-to-hub ()
  "Connect to the CIDER-SPY-HUB.
   Associate the *CIDER-SPY-HUB* buffer with the default CIDER connection buffer."
  (interactive)
  (lexical-let ((hub-connection-buffer (get-buffer-create (generate-new-buffer-name "*cider spy hub*"))))
    (with-current-buffer (cider-default-connection)
      (setq cider-spy-hub-connection-buffer hub-connection-buffer)
      (let ((nrepl-request-counter (cl-incf cider-spy-request-counter)))
        (nrepl-send-request
         (append (list "op" "cider-spy-hub-connect"
                       "session" nrepl-session)
                 (when cider-spy-hub-alias
                   (list "hub-alias" cider-spy-hub-alias)))
         (lambda (response)
           (nrepl-dbind-response response (value err from recipient msg hub-registered-alias)
             (cond (msg
                    ;; Received a message from another developer in the hub
                    (cider-spy-msg-receive recipient from msg))
                   (hub-registered-alias
                    (cider-spy--dev-registered hub-connection-buffer hub-registered-alias))
                   (value
                    (cider-spy-connection-buffer-emit hub-connection-buffer (concat value "\n")))
                   (err
                    (cider-spy-connection-buffer-emit hub-connection-buffer (concat "OOPS\n" err "\n"))))))
         (current-buffer))))))

(defun cider-spy-attach-nrepl-response-handler ()
  "Attach an nREPL response handler.
When a response comes from nREPL relevant to the CIDER-SPY summary operation,
the current buffer will be updated accordingly."
  (let ((buffer (current-buffer)))
    (nrepl-send-request (list "op" "cider-spy-summary"
                              "session" (cider-current-session))
                        (nrepl-make-response-handler
                         buffer
                         (lambda (buffer str)
                           (cider-spy-refresh-buffer buffer (concat str "\n")))
                         '()
                         (lambda (buffer _str)
                           (cider-spy-refresh-buffer buffer "Oops"))
                         '())
                        cider-spy-summary-buffer-nrepl-connection)))

(defun cider-spy-refresh-summary ()
  "Refresh the cider spy summary buffer."
  (interactive)
  (cider-spy-attach-nrepl-response-handler))

;;;###autoload
(defun cider-spy-summary ()
  "Create *cider-spy* buffer and attach listener.
   We assign a cider-spy-summary buffer to the nrepl-connection-buffer."
  (interactive)
  (with-current-buffer (cider-default-connection)
    (unless (and cider-spy-summary-buffer (buffer-name cider-spy-summary-buffer))
      (let ((summary-buffer (get-buffer-create (generate-new-buffer-name "*cider spy*"))))
        (with-current-buffer summary-buffer
          (setq cider-spy-summary-buffer-nrepl-connection (cider-default-connection))
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
         "session" (cider-current-session))
   nil
   cider-spy-summary-buffer-nrepl-connection))

(defun cider-spy-alias ()
  "Reset CIDER-SPY tracking used for *cider-spy* buffer."
  (interactive)

  (let ((alias (read-string "Set Alias: ")))
    (nrepl-send-request
     (list "op" "cider-spy-hub-alias"
           "session" (cider-current-session)
           "alias" alias)
     nil)))

(defun cider-spy-hub-disconnect ()
  "Disconnect from CIDER-SPY-HUB."
  (interactive)
  (nrepl-send-request
   (list "op" "cider-spy-hub-disconnect"
         "session" (cider-current-session))
   nil))

(defun cider-spy-kill-buffers ()
  "Kill all CIDER-SPY buffers"
  (interactive)
  (let ((killed-names))
    (dolist (buffer (buffer-list))
      (let ((name (buffer-name buffer)))
        (when (string-match "^\\*cider spy" name)
          (kill-buffer name)
          (setq killed-names (cons name killed-names)))))
    (message "Killed buffers %s" (mapconcat 'identity killed-names ", "))))

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

(add-hook 'nrepl-connected-hook 'cider-spy-connect-to-hub)

;; cider-spy msg:

(defvar cider-spy-msg-popup-buffer-name-template "*hub %s*"
  "Buffer name for message popup.")

(make-variable-buffer-local
 (defvar cider-spy-msg-recipient nil
   "ID Recipient for msg."))

(make-variable-buffer-local
 (defvar cider-spy-msg-alias nil
   "Alias of current developer used for messaging."))

(make-variable-buffer-local
 (defvar cider-spy-msg-prompt-start nil
   "Marker for the start of prompt."))

(make-variable-buffer-local
 (defvar cider-spy-msg-input-start nil
   "Marker for the start of input."))

(defun cider-spy-msg-send (from recipient msg)
  (interactive)
  (nrepl-send-request
   (list "op" "cider-spy-hub-send-msg"
         "session" (cider-current-session)
         "from" from
         "recipient" recipient
         "message" msg)
   nil)
  (message "Sent message from %s to %s." from recipient))

(defun cider-spy-msg-reset-markers ()
  "Reset all CIDER-SPY-MSG markers."
  (dolist (markname '(cider-spy-msg-prompt-start
                      cider-spy-msg-input-start))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))

(defun cider-spy-msg--insert-msg (from msg)
  "Insert the msg into msg buffer.
   We use overlays to keep track of where the prompt and msg are,
   and for setting font locks."
  (goto-char cider-spy-msg-prompt-start)
  (unless (bolp) (insert-before-markers "\n"))
  (let ((prompt-start (point)))
    (insert-before-markers from)
    (insert-before-markers " >> ")
    (let ((msg-start (point)))
      (insert-before-markers msg)
      (let ((overlay (make-overlay prompt-start msg-start)))
        (overlay-put overlay 'face 'font-lock-keyword-face))
      (let ((overlay (make-overlay msg-start (point))))
        (overlay-put overlay 'face 'font-lock-keyword-face))))
  (when (not (eolp))
    (insert "\n"))
  (goto-char (max-char)))

(defun cider-spy-msg--insert-prompt ()
  "Insert a prompt into msg buffer"
  (goto-char (max-char))
  (set-marker cider-spy-msg-prompt-start (point))
  (unless (bolp) (insert "\n"))
  (insert (format "%s >> " cider-spy-msg-alias))
  (set-marker cider-spy-msg-input-start (point))
  (let ((overlay (make-overlay cider-spy-msg-prompt-start
                               cider-spy-msg-input-start)))
    (overlay-put overlay 'face 'font-lock-keyword-face)))

(defun cider-spy-msg--pointer-to-bookmark (pointer-str)
  "Convert a pointer to a bookmark."
  (when (string-prefix-p "Pointer:" pointer-str)
    (-map (lambda (bm-component)
            (let ((cmpt (split-string bm-component " ")))
              (cons (car cmpt) (cadr cmpt))))
          (split-string
           (cadr (split-string pointer-str ": "))
           ", "))))

(defun cider-spy-msg--match-project-to-relative-path (relative-file)
  (car (-filter 'file-exists-p
                (-map (lambda (conn)
                        (concat
                         (file-relative-name
                          (with-current-buffer (get-buffer conn)
                            nrepl-project-dir)
                          ) "/" relative-file))
                      nrepl-connection-list))))

(defun cider-spy-msg-jump-to-bookmark ()
  "Jump to bookmark in chat buffer."
  (interactive)
  (let* ((overlay (cl-first (overlays-at (point))))
         (overlay-string (buffer-substring-no-properties
                            (overlay-start overlay)
                            (overlay-end overlay)))
         (bm (cider-spy-msg--pointer-to-bookmark overlay-string))
         (filename (cider-spy-msg--match-project-to-relative-path
                    (cdr (assoc "filename" bm)))))
    (bookmark-jump
     (list "Cider-Spy"
           (cons 'filename filename)
           (cons 'position (string-to-number (cdr (assoc "position" bm))))))))

(defun cider-spy-msg-return ()
  "Hit return to send a message to user."
  (interactive)
  (if (cl-first (overlays-at (point)))
      (cider-spy-msg-jump-to-bookmark)
    (progn
      (goto-char (point-max))
      (let ((msg (buffer-substring cider-spy-msg-input-start (point))))
        (cider-spy-msg--insert-prompt)
        (cider-spy-msg-send cider-spy-msg-alias cider-spy-msg-recipient msg)))))

(defun cider-spy-msg--get-popup (alias dev)
  (let ((buffer-name (format cider-spy-msg-popup-buffer-name-template dev)))
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        ;; Initialise message buffer
        (cider-spy-popup-mode)
        (setq cider-spy-msg-alias alias)
        (setq cider-spy-msg-recipient dev)
        (cider-spy-msg-reset-markers)
        (cider-spy-msg--insert-prompt)))
    (get-buffer buffer-name)))

(defun cider-spy-msg-receive (alias sender msg)
  "Receive a message from another developer in the HUB."
  (message "Received message from %s" alias)
  (with-current-buffer (cider-spy-msg--get-popup alias sender)
    (cider-spy-msg--insert-msg from msg)
    (pop-to-buffer (current-buffer))))

(defun cider-spy-msg-edit (alias recipient)
  "Start a new message to another developer in the HUB."
  (interactive)
  (with-current-buffer (cider-spy-msg--get-popup alias recipient)
    (pop-to-buffer (current-buffer))))

(defun cider-spy-msg--pointer-from-bm (bm)
  "Extract a file path relative to a project directory from a bookmark.
   For example ~/proja/src/foo.clj -> src/foo.clj"
  (let* ((filename (cdr (assoc 'filename bm)))
         (bm-file (file-relative-name filename
                                      (locate-dominating-file filename "project.clj"))))
    (format "Pointer: filename %s, position %s"
            bm-file (cdr (assoc 'position bm)))))

(defun cider-spy-msg-send-bookmark ()
  "Send a bookmark to another developer."
  (interactive)
  (let* ((bm (cdr (bookmark-get-bookmark
                   (completing-read "Choose bookmark: " bookmark-alist nil t))))
         (pointer (cider-spy-msg--pointer-from-bm bm)))
    (goto-char (point-max))
    (insert pointer)
    (cider-spy-msg-return)))

(defvar cider-spy-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'cider-spy-msg-return)
    (define-key map (kbd "C-c C-b") 'cider-spy-msg-send-bookmark)
    map))

(define-derived-mode cider-spy-popup-mode text-mode "Cider Spy Popup")

(provide 'cider-spy)

;;; cider-spy.el ends here
