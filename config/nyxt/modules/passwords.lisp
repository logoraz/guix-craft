;;;; Nyxt Passwords Modules
;;; Setup:
;;; ln -f ~/repos/guix-craft/config/nyxt/config.lisp \
;;;       ~/.config/nyxt/modules/passwords.lisp
;; References
;; 1. https://nyxt.atlas.engineer/documentation#keepassxc-support

(defmethod initialize-instance :after ((interface password:keepassxc-interface)
                                       &key &allow-other-keys)
  "It's obviously not recommended to set master password here,
as your config is likely unencrypted and can reveal your password to someone
peeking at the screen."
  (setf (password:password-file interface) "~/files/moses/passwords.kdbx"
        (password:key-file interface) "~/files/moses/keyfile"))

(define-configuration nyxt/mode/password:password-mode
  ((nyxt/mode/password:password-interface
    (make-instance 'password:keepassxc-interface))))

(define-configuration buffer
  ((default-modes
    (append (list 'nyxt/mode/password:password-mode) %slot-value%))))
