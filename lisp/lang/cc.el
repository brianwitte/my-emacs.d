;;; -*- lexical-binding: t -*-
;;

;;; cc.el --- Description

(use-package citre
  :straight t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.  Alternatively, you can define them
  ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :config
  ;; Enable integrations
  (setq-default citre-enable-capf-integration t)
  ;;(setq-default citre-enable-xref-integration t)
  ;;(setq-default citre-enable-imenu-integration t)

  ;; Customize completion behavior
  (setq citre-completion-case-sensitive nil) ;; Adjust according to your needs
  (setq citre-capf-substr-completion t)
  (setq citre-capf-optimize-for-popup t)

  ;; Set up completion styles
;;  (setq completion-styles '(substring basic))
;;  (add-to-list 'completion-category-overrides
;;               '(citre (styles basic)))



;;
;;  ;; Add the hook for org-mode unfolding
;;  (add-hook 'citre-after-jump-hook
;;            (defun unfold-if-in-org-mode ()
;;              (when (derived-mode-p 'org-mode)
;;                (org-fold-show-context 'isearch))))
  (setq
   ;; Set these if readtags/ctags is not in your PATH.
   ;;citre-readtags-program "/path/to/readtags"
   citre-ctags-program "/usr/bin/ctags-universal"
   ;; Set these if gtags/global is not in your PATH (and you want to use the
   ;; global backend)
   ;;citre-gtags-program "/path/to/gtags"
   ;;citre-global-program "/path/to/global"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   ;;citre-default-create-tags-file-location 'global-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ;; By default, when you open any file, and a tags file can be found for it,
   ;; `citre-mode' is automatically enabled.  If you only want this to work for
   ;; certain modes (like `prog-mode'), set it like this.
   citre-auto-enable-citre-mode-modes '(c-mode prog-mode)))

;; Ensure that Citre uses Corfu for popup completion
;;(add-hook 'completion-at-point-functions #'my-completion-at-point)

;; Optionally, you can bind the completion-at-point function to a key
;;(global-set-key (kbd "M-TAB") #'my-completion-at-point)

(setq projectile-tags-backend nil)
(setq projectile-tags-command nil)
(setq projectile-tags-file-name "tags")


(provide 'emx-cc)
