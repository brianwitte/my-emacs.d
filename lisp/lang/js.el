;;; -*- lexical-binding: t -*-

(use-package typescript-mode
  :straight t)

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook (typescript-ts-mode . lsp-deferred)
  :init
  (setq lsp-prefer-capf t)  ; Use `completion-at-point-functions', recommended for better performance
  :config
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection (lambda () '("vtsls" "--stdio")))
                     :major-modes '(typescript-mode js-mode js2-mode js3-mode)
                     :server-id 'vtsls)))

(use-package typescript-ts-mode
  :straight t
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(defun vtsls-execute-command (command &optional arguments)
  "Execute a vtsls specific command."
  (when (lsp-workspace)
    (lsp-execute-command
      (intern (format "typescript.%s" command))  ; Convert command string to symbol
      arguments)))

(defun vtsls-restart-ts-server ()
  "Restart the TypeScript server."
  (interactive)
  (vtsls-execute-command "restartTsServer"))

(defun vtsls-open-ts-log ()
  "Open TypeScript server log."
  (interactive)
  (vtsls-execute-command "openTsServerLog"))

(defun vtsls-organize-imports ()
  "Organize imports in the current buffer."
  (interactive)
  (vtsls-execute-command "organizeImports" (list (lsp--text-document-identifier))))

(add-hook 'typescript-mode-hook
          (lambda ()
            (define-key typescript-mode-map (kbd "C-c l r") 'vtsls-restart-ts-server)
            (define-key typescript-mode-map (kbd "C-c l o") 'vtsls-open-ts-log)
            (define-key typescript-mode-map (kbd "C-c l i") 'vtsls-organize-imports)))

(provide 'emx-js)
