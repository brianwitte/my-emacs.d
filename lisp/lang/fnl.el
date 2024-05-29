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



(use-package tree-sitter-langs
  :straight t)

(use-package tree-sitter
  :straight t
  :after tree-sitter-langs
  :custom-face
  (tree-sitter-hl-face:property         ((t (:slant normal))))
  (tree-sitter-hl-face:method.call      ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:function.call    ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:function.builtin ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:operator         ((t (:inherit default))))
  (tree-sitter-hl-face:type.builtin     ((t (:inherit font-lock-type-face))))
  (tree-sitter-hl-face:number           ((t (:inherit highlight-numbers-number))))
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(setq lsp-clients-lua-language-server-install-dir "/home/bkz/lang-servers/lua-language-server/")
(use-package lsp-mode
  :straight t)


(use-package lua-mode
  :straight t
  :init
  ;; lua-indent-level defaults to 3. let's keep it? Madness reigns.
  (setq lua-indent-level 3)
  :config

  (add-hook 'lua-mode-local-vars-hook #'lsp 'append)
  (add-hook 'lua-mode-local-vars-hook #'tree-sitter 'append))


(use-package lsp-ui
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-sideline-global ((t (:italic t))))
  (lsp-ui-peek-highlight  ((t (:foreground unspecified :background unspecified :inherit isearch))))
  :config
  (with-eval-after-load 'evil
    (add-hook 'buffer-list-update-hook
              #'(lambda ()
                  (when (bound-and-true-p lsp-ui-mode)
                    (evil-define-key '(motion normal) 'local (kbd "K")
                      #'(lambda () (interactive) (lsp-ui-doc-glance) (my/pulse-line)))))))
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-enhanced-markdown nil)
  (setq lsp-ui-doc-delay 0.01)
  (when (display-graphic-p)
    (setq lsp-ui-doc-use-childframe t)
    (setq lsp-ui-doc-text-scale-level -1.0)
    (setq lsp-ui-doc-max-width 80)
    (setq lsp-ui-doc-max-height 25)
    (setq lsp-ui-doc-position 'at-point))
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
  (setq lsp-ui-sideline-diagnostic-max-line-length 80)
  (setq lsp-ui-sideline-diagnostic-max-lines 2)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-delay 0.05))



(provide 'emx-fnl)
