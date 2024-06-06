;;; -*- lexical-binding: t -*-
;;

;;; editor.el --- Description


;; lsp-client-packages
;;
;; Type: (repeat symbol)
;;
;; (lsp-ansible
;;  lsp-awk
;;  lsp-bash
;;  lsp-clangd
;;  lsp-clojure
;;  lsp-lua
;;  lsp-nginx
;;  lsp-ocaml
;;  lsp-pyright
;;  lsp-rust
;;  lsp-solargraph
;;  lsp-ruby-lsp
;;  lsp-ruby-syntax-tree
;;  lsp-solidity
;;  lsp-sqls
;;  lsp-svelte
;;  lsp-steep
;;  lsp-tilt
;;  lsp-zig
;;  lsp-jq)


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

(defun my/pulse-line ()
  "Flash highlight the current line with region face"
  (interactive)
  (pulse-momentary-highlight-one-line (point) 'region))

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((java-mode       ; eclipse-jdtls
          ) . lsp-deferred)
  :preface
  (defun my/lsp-execute-code-action ()
    "Execute code action with pulse-line animation."
    (interactive)
    (my/pulse-line)
    (call-interactively 'lsp-execute-code-action))
  :custom-face
  (lsp-headerline-breadcrumb-symbols-face                ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-path-face                   ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-project-prefix-face         ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-unknown-project-prefix-face ((t (:inherit variable-pitch))))
  :commands lsp
  :config
  (add-hook 'java-mode-hook #'(lambda () (when (eq major-mode 'java-mode) (lsp-deferred))))
  (global-unset-key (kbd "<f2>"))
  (define-key lsp-mode-map (kbd "<f2>") #'lsp-rename)
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-links nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-lens-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-keep-workspace-alive nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.25)
  (setq lsp-auto-execute-action nil)
  )


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

(use-package reformatter
  :straight t)

(provide 'emx-editor)
