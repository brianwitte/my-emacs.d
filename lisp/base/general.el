;;; -*- lexical-binding: t -*-
;;;
;;; base/general.el --- Description

(use-package general
  :straight t
  :config
  (general-create-definer my-leader-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(defun my-core-normal-keybindings ()
  (my-leader-def
    :keymaps 'normal
    ;; File operations
    "ff" 'find-file
    "fs" 'save-buffer
    ;; Buffers
    "bs" 'fzf-switch-buffer
    "bb" 'switch-to-buffer
    "bd" 'kill-buffer
    "bi" 'ibuffer
    ;; Commands
    ": " 'execute-extended-command
    ;; Projectile keybindings
    "p p" 'projectile-switch-project
    "SPC" 'fzf-projectile
    ;; More projectile
    "p r" 'projectile-recentf
    "p b" 'projectile-switch-to-buffer
    "p s" 'projectile-save-project-buffers
    "p k" 'projectile-kill-buffers
    "p d" 'projectile-find-dir
    "p d" 'projectile-dired
    "p R" 'projectile-regenerate-tags
    ;; Code Actions
    "l a"  'lsp-execute-code-action
    ;; Refactoring
    "l r r" 'lsp-rename
    ;; Help/Documentation
    "l h d" 'lsp-describe-thing-at-point
    ;; Goto
    "l g t" 'lsp-goto-type-definition
    "l g i" 'lsp-goto-implementation
    ;; Workspace/Project
    "l r R" 'lsp-restart-workspace
    ;; Formatting
    "l f b" 'lsp-format-buffer
    ;; Highlight Symbol
    "l h l" 'lsp-symbol-highlight
    ;; Workspace Folders
    "l w a" 'lsp-workspace-folders-add
    "l w r" 'lsp-workspace-folders-remove
    "l w s" 'lsp-workspace-folders-switch
    ))

(defun my-core-visual-keybindings ()
  (my-leader-def
    :keymaps 'visual
    "fr" 'fill-region
    "; " 'evilnc-comment-or-uncomment-lines))

(defun my-core-general-keybindings ()
  (my-leader-def
    :keymaps '(normal visual)
    ": " 'execute-extended-command))

(defun my-window-management-keybindings ()
  (my-leader-def
    :keymaps 'normal
    ;; Window splits
    "ws" 'split-window-below
    "wv" 'split-window-right
    ;; Window navigation
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    ;; Window operations
    "wq" 'evil-quit
    "wd" 'delete-window
    "wD" 'kill-buffer-and-window
    ;; Maximize window
    "wmm" 'delete-other-windows))

(defun my-evil-normal-keybindings ()
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map "," nil)
    (evil-define-key 'normal 'global
      "gd" 'xref-find-definitions
      "gD" 'xref-find-definitions-other-window)))

(defun my-local-leader-keybindings ()
  (general-create-definer my-local-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix ","))

(defun my-forcing-keybindings ()
  (my-leader-def
    :keymaps '(insert visual)
    "ee" 'evil-force-normal-state))


(my-core-normal-keybindings)
(my-core-visual-keybindings)
(my-core-general-keybindings)
(my-window-management-keybindings)
(my-evil-normal-keybindings)
(my-local-leader-keybindings)
(my-forcing-keybindings)

(provide 'emx-general)
