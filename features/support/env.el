(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq cider-spy-root-path project-directory))

(add-to-list 'load-path cider-spy-root-path)

(require 'espuds)
(require 'ert)

;;(require 'cider-spy)

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
