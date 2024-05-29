;;; -*- lexical-binding: t -*-
;;
;;; ocaml.el --- Description

;; Major mode for OCaml programming
(use-package tuareg
  :straight t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

;; Major mode for editing Dune project files
(use-package dune
  :straight t)

;; Merlin provides advanced IDE features
;;(use-package merlin
;;  :straight t
;;  :config
;;  (add-hook 'tuareg-mode-hook #'merlin-mode)
;;  (add-hook 'merlin-mode-hook #'corfu-mode)
;;  ;; we're using flycheck instead
;;  (setq merlin-error-after-save nil))
;;
;;(use-package merlin-eldoc
;;  :straight t
;;  :hook ((tuareg-mode) . merlin-eldoc-setup))


(defun my/ocaml-lsp-fix-buffer ()
  "Formats buffer and organizes imports."
  (interactive)
  (lsp-organize-imports)
  (lsp-format-buffer))

(use-package lsp-mode
  :straight t
  :after flycheck
  :commands lsp
  :bind (("C-c l n" . flycheck-next-error)
         ("C-c l d" . lsp-find-definition)
         ("C-c l r" . lsp-find-references)
         ("C-c l h" . lsp-describe-thing-at-point)
         ("C-c l i" . lsp-find-implementation)
         ("C-c l R" . lsp-rename)
         ("C-c l o" . my-lsp-fix-buffer))
  :hook ((tuareg-mode . lsp)
         (caml-mode . lsp)
         (reason-mode . lsp)
         (before-save . lsp-organize-imports))
  :custom
  (lsp-lens-enable t)
  (lsp-log-io nil)
  (lsp-headerline-breadcrumb-enable nil)
  :config
  (lsp-enable-which-key-integration t)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     '("opam" "exec" "--" "ocamllsp"))
    :major-modes '(caml-mode tuareg-mode reason-mode)
    :server-id 'ocamllsp)))

;; This uses Merlin internally
(use-package flycheck-ocaml
  :straight t
  :config
  (flycheck-ocaml-setup))


(provide 'emx-ocaml)
