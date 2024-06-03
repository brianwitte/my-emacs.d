;;; -*- lexical-binding: t -*-
;;;
;;; file_keymaps.el --- Keymaps to open specific files/dirs


;; Helper function to create multiple navigation functions
(defun create-nav-functions (file-map)
  "Create functions to open files specified in FILE-MAP.
FILE-MAP is a list of (NAME . PATH) pairs."
  (dolist (entry file-map)
    (let ((name (car entry))
          (path (cdr entry)))
      (eval `(defun ,(intern (concat "nav-to-" name)) ()
               ,(concat "Open the " name " file.")
               (interactive)
               (find-file ,path))))))

;; Define file paths (lexically scoped to this file)
(let ((paths-for-keymaps
       '(("learn-elisp" . "~/.config/emacs/learn-emacs-lisp.el")
         ("init" . "~/.config/emacs/init.el")
         ("zshrc" . "~/.zshrc")
         ("gtd" . "~/org/gtd.org")
         ("file-bindings" . "~/.emacs/config/lisp/file-bindings.el")
         ("scripts-dir" . "~/scripts")
         ("xdg-home-dir" . "~/.config"))))
  (create-nav-functions paths-for-keymaps))

;; Define core visual keybindings
(defun my-file-keymaps ()
  (my-leader-def
    :keymaps 'normal
    "a l" 'nav-to-learn-elisp
    "a i" 'nav-to-init
    "a g" 'nav-to-gtd
    "a k" 'nav-to-file-keymaps
    "a z" 'nav-to-zshrc
    "d e" 'nav-to-elisp-dir
    "d s" 'nav-to-scripts-dir
    "d x" 'nav-to-xdg-home-dir
    ))

(my-file-keymaps)

(provide 'file-keymaps)
