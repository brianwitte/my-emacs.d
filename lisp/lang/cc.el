;;; -*- lexical-binding: t -*-

;;; cc.el --- Citre Configuration for C Modes

(use-package citre
  :straight t
  :init
  (require 'citre-config)  ; Required for lazy loading

  :config
  ;; Enable Citre integrations
  (setq-default citre-enable-capf-integration t)

  ;; Customize Citre's completion settings
  (setq citre-completion-case-sensitive nil
        citre-capf-substr-completion t
        citre-capf-optimize-for-popup t)

  ;; Local leader keybindings specific to citre-mode in c-mode
  (add-hook 'c-mode-hook (lambda ()
                           (citre-mode 1)  ; Enable Citre mode only in c-mode
                           (my-local-leader-def
                             :keymaps 'c-mode-map
                             "g" 'citre-jump
                             "G" 'citre-jump-back
                             "p" 'citre-ace-peek
                             "u" 'citre-update-this-tags-file)))

  ;; Citre external tools configuration
  (setq citre-ctags-program "/usr/bin/ctags-universal"
        citre-project-root-function #'projectile-project-root
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t
        citre-auto-enable-citre-mode-modes nil)  ; No automatic enabling

  ;; Disabling projectile's tags settings to avoid conflicts
  (setq projectile-tags-backend nil
        projectile-tags-command nil
        projectile-tags-file-name "tags"))

;; Code formatting with reformatter for consistent coding style
(require 'reformatter)

(reformatter-define kstyle 
  :program "uncrustify"
  :args `("-c" ,(expand-file-name "~/src/uncrustify/etc/linux.cfg") "-l" "C")
  :lighter " kstyle")

(reformatter-define allman
  :program "uncrustify"
  :args `("-c" ,(expand-file-name "~/src/uncrustify/etc/allman.cfg") "-l" "C")
  :lighter " allman")

(provide 'emx-cc)
;;; cc.el ends here
