;;; -*- lexical-binding: t -*-

(use-package typescript-mode
  :straight t)

;; if you use typescript-mode
(use-package tide
  :straight t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; if you use treesitter based typescript-ts-mode (emacs 29+)
(use-package tide
  :straight t
  :after (company flycheck)
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
