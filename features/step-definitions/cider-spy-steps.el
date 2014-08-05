(Given "^I refresh with \\(.+\\)$"
  (lambda (json)
    (cider-spy-refresh-buffer (get-buffer-create "buf") json)))

(Then "^I should see \"\\(.+\\)\" in the \"\\(.+\\)\" section$"
  (lambda (str section)
    (let ((section-text (replace-regexp-in-string
                         "\n" "" (cider-spy-test-grab-section-as-string
                                  (get-buffer-create "buf") (make-symbol section)))))
      (assert (equal str section-text) nil "Expected %S got %S" str section-text))))

(Then "^I go to beginning of summary buffer$"
  (lambda ()
    (with-current-buffer (get-buffer-create "buf")
      (goto-char 1))))

(And "^I go to the next section$"
  (lambda ()
    (with-current-buffer (get-buffer-create "buf")
      (cider-spy-next-section (current-buffer)))))

(Then "^I should be on a \"\\(.+\\)\" section$"
  (lambda (section)
    (with-current-buffer (get-buffer-create "buf")
      (assert (string-equal section (cider-spy-section-type (cider-spy-find-section-at-point)))))))
