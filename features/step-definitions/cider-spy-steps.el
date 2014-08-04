(Given "^I refresh with \\(.+\\)$"
  (lambda (json)
    (cider-spy-refresh-buffer (get-buffer-create "buf") json)))

(Then "^I should see \"\\(.+\\)\" in the \"\\(.+\\)\" section$"
  (lambda (str section)
    (let ((section-text (replace-regexp-in-string
                         "\n" "" (cider-spy-test-grab-section-as-string
                                  (get-buffer-create "buf") (make-symbol section)))))
      (assert (equal str section-text) nil "Expected %S got %S" str section-text))))
