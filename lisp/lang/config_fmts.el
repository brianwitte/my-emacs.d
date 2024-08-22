;;; -*- lexical-binding: t -*-
;;

;;; config_fmts.el --- Description


(use-package jq-mode
  :straight t
  :mode "\\.jq\\'"
  :commands jq-mode
  :init
  ;; Automatically load jq-mode for .jq files
  (add-to-list 'auto-mode-alist '("\\.jq\\'" . jq-mode))
  :config
  ;; Optional: If using interactively with JSON mode
  (with-eval-after-load "json-mode"
    (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively))
  ;; Optional: If using yq for YAML files
  (setq jq-interactive-command "jq"
        jq-interactive-default-options "")
  )

(use-package ob-jq
  :after org
  :config
  ;; Enable jq for Org Babel
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((jq . t))))


(provide 'emx-config-fmts)
