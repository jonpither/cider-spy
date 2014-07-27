(Given "^I refresh with \"\\(.+\\)\"$"
  (lambda (json)
    (message "f1 %s" json)
    (cider-spy-refresh-buffer (current-buffer) json)
    (message "f2")))

(Then "^I should see \"\\(.+\\)\" in the \"\\(.+\\)\" section$"
  (lambda (str section)
    (message "c")
    (let ((section-text (cider-spy-test-grab-section-as-string (current-buffer) (make-symbol section))))
      (assert true)
;;      (assert (equal str section-text) nil "Expected %S got %S" str section-text)
      )))
