;;; -*- lexical-binding: t -*-
;;
;;; fnl.el --- Description

(use-package fennel-mode
  :straight t
  :config
  (defun my-fennel-mode-keybindings ()
    (my-local-leader-def
      :states 'normal
      :keymaps '(fennel-mode-map)
      " '"  #'fennel-repl
      " m"  #'fennel-macroexpand
      " e f" #'fennel-eval-toplevel-form
      " e e" #'fennel-eval-last-sexp
      " e r" #'fennel-eval-region
      " e n" #'fennel-eval-form-and-next
      ;; Help
      " h d" #'fennel-show-documentation
      " h v" #'fennel-show-variable-documentation
      ;; Namespace
      " n r" #'fennel-reload
      ;; REPL
      " r r" #'fennel-reload
      ;; Test
      ;;" t t" #'my-fennel-run-tests)
      ))

  (defun my-fennel-evil-keybindings ()
    (evil-define-key 'normal fennel-mode-map
      "gd" 'fennel-find-definition))

  (add-hook 'fennel-mode-hook 'my-fennel-mode-keybindings)
  (add-hook 'fennel-mode-hook 'my-fennel-evil-keybindings))

(provide 'emx-fnl)
