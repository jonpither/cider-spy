;;; cider-spy-msg.el --- Send messages through CIDER SPY.

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

;; TODO a later effort is to make a decent interactive command that uses completing read to get the right alias. It'd have to use the first id.

(defvar cider-spy-msg-edit-buffer-name "*cider spy msg*"
  "Buffer name for composing messages.")

(defvar cider-spy-edit-prev-window-configuration nil)

(defun cider-spy-edit-message (id alias)
  (interactive)
  (let ((buf (get-buffer-create cider-spy-msg-edit-buffer-name)))
    (setq cider-spy-edit-prev-window-configuration
          (current-window-configuration))
    (pop-to-buffer buf)
    (erase-buffer)
    (insert (format "## Send message to %s\n\n" alias))
    (cider-spy-edit-mode)
    (font-lock-fontify-buffer)
    (message "Type C-c C-c to send (C-c C-k to cancel).")))

(defun cider-spy-send-foo ()
  (interactive)
  (message "Sent message") ;; todo embed alias in this msg
  (kill-buffer (get-buffer cider-spy-msg-edit-buffer-name))
  (when cider-spy-edit-prev-window-configuration
    (set-window-configuration cider-spy-edit-prev-window-configuration)
    (setq cider-spy-edit-prev-window-configuration nil)))

(defvar cider-spy-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'cider-spy-send-foo)
;;    (define-key map (kbd "C-c C-k") 'cider-spy-send-foo)
    map
    ))

(define-derived-mode cider-spy-edit-mode text-mode "Cider Spy Edit")

(font-lock-add-keywords 'cider-spy-edit-mode
                        '(("## Send.*\n" . font-lock-comment-face)))

(provide 'cider-spy-msg)

;;; cider-spy.el ends here
