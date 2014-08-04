(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq cider-spy-root-path project-directory))

(add-to-list 'load-path cider-spy-root-path)

(require 'json)
(require 'cider-spy)

(defun cider-spy-test-grab-section (buffer k)
  (with-current-buffer buffer
    (car (-filter (lambda (section)
                    (string-equal k (cider-spy-section-type section)))
                  (cider-spy-descendent-sections cider-spy-root-section)))))

(defun cider-spy-test-grab-section-as-string (buffer k)
  (with-current-buffer buffer
    (let ((section (cider-spy-test-grab-section buffer k)))
      (buffer-substring-no-properties (cider-spy-section-beginning section)
                                      (- (cider-spy-section-end section) 1)))))

(Setup
 ;; Stuff
 )

(Before
 ;; Stuff
 )

(After
 ;; Stuff
 )

(Teardown
 ;; Done.
 )
