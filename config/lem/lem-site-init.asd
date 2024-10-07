;;;; lem-site-init.asd

(asdf/parse-defsystem:defsystem "lem-site-init"
  :depends-on ()
  :components ((:module "source"
                :components ((:file "paredit")
                             (:file "appearance")
                             (:file "utilities")
                             (:file "misc")
                             (:file "completions")
                             (:file "keybindings")
                             (:file "file-prompt")
                             (:file "time-stamp")))))
