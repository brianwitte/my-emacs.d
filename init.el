;;; -*- lexical-binding: t -*-
;;; init.el --- Clean and pragmatic Emacs configuration

;; =======================
;; Package Management
;; =======================

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; =======================
;; Core Settings
;; =======================

;; Performance optimizations
(setq gc-cons-threshold 50000000
      load-prefer-newer t
      large-file-warning-threshold 100000000
      confirm-kill-processes nil)

;; UI cleanup
(setq inhibit-startup-screen t
      initial-scratch-message
      (concat ";;      _________\n"
              ";;     / ======= \\\n"
              ";;    / __________\\\n"
              ";;   | ___________ |\n"
              ";;   | |e        | |\n"
              ";;   | |         | |\n"
              ";;   | |_________| |______________________________\n"
              ";;   \\_____________/   ==== _  _  __   __  __     )\n"
              ";;   /\\\\\\\\\\\\\\\\\\\\\\\\\\\\   |__  |\\/| |__| |   [__    \\/\n"
              ";;  / ::::::::::::: \\  |    |||| |  | |__  __]  /\n"
              ";; (_________________) ====                    /\n"
              ";;                                         =D-'\n"
              ";; Emacs Version: " emacs-version "\n"
              ";;\n"
              "(message \"Welcome to Emacs!\")\n\n"))

;; Better defaults
(setq-default
 indent-tabs-mode nil
 tab-width 4
 fill-column 80
 truncate-lines t)

;; File handling
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Built-in enhancements
(recentf-mode 1)
(setq recentf-max-saved-items 1000)
(show-paren-mode 1)
(electric-pair-mode 1)
(savehist-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Minibuffer settings
(setq enable-recursive-minibuffers t
      read-extended-command-predicate #'command-completion-default-include-p
      minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

;; Hide warnings buffer
(add-to-list 'display-buffer-alist
             '("^\\*Warnings\\*" . (display-buffer-no-window)))

;; =======================
;; System Integration
;; =======================

(use-package exec-path-from-shell
  :config
  (when (or (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))

(use-package sudo-edit
  :commands sudo-edit)

(use-package xclip
  :config
  (xclip-mode 1))

;; =======================
;; Evil Mode & Keybindings
;; =======================

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  
  ;; Visual line motions
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  
  ;; Navigation bindings
  (evil-define-key 'normal 'global (kbd "gd") 'xref-find-definitions)
  (evil-define-key 'normal 'global (kbd "gD") 'xref-find-definitions-other-window)
  
  ;; Initial states
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package general
  :after evil
  :config
  ;; Define leader key functions
  (general-create-definer my-leader-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer my-local-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix ",")

  ;; Global leader bindings
  (my-leader-def
    ;; Core commands
    ":" 'execute-extended-command
    "SPC" 'projectile-find-file
    "." 'embark-act
    "," 'consult-buffer
    
    ;; Files
    "ff" 'find-file
    "fs" 'save-buffer
    "fr" 'recentf-open-files
    
    ;; Buffers
    "bb" 'switch-to-buffer
    "bd" 'kill-buffer
    "bi" 'ibuffer
    "bn" 'next-buffer
    "bp" 'previous-buffer
    
    ;; Windows
    "ws" 'split-window-below
    "wv" 'split-window-right
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "wd" 'delete-window
    "wD" 'kill-buffer-and-window
    "wmm" 'delete-other-windows
    "wr" 'my/resize-window
    
    ;; Projects
    "pp" 'projectile-switch-project
    "pb" 'projectile-switch-to-buffer
    "pc" 'projectile-compile-project
    "pk" 'projectile-kill-buffers
    "pd" 'projectile-dired
    "pr" 'projectile-replace
    
    ;; Search & Navigation
    "ss" 'consult-line
    "rf" 'consult-find
    "rm" 'consult-mode-command
    "ro" 'consult-outline
    "ri" 'consult-imenu
    "rk" 'consult-flymake
    "rs" 'consult-locate
    "rg" 'ripgrep
    
    ;; Tools
    "op" 'treemacs
    "dl" 'downcase-current-line))

;; =======================
;; Completion & Navigation
;; =======================

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic-remote orderless))))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal orderless-prefixes orderless-initialism orderless-regexp))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher))
  :init
  ;; Dispatcher functions
  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))
  
  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))
  
  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1)))))

(use-package vertico
  :demand t
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed vertico-flat vertico-grid
                               vertico-mouse vertico-quick vertico-buffer
                               vertico-repeat vertico-reverse vertico-directory
                               vertico-multiform vertico-unobtrusive))
  :general
  (:keymaps '(normal insert visual motion) "M-." 'vertico-repeat)
  (:keymaps 'vertico-map
            "<tab>" 'vertico-insert
            "<escape>" 'minibuffer-keyboard-quit
            "?" 'minibuffer-completion-help
            "C-M-n" 'vertico-next-group
            "C-M-p" 'vertico-previous-group
            "<backspace>" 'vertico-directory-delete-char
            "C-w" 'vertico-directory-delete-word
            "C-<backspace>" 'vertico-directory-delete-word
            "RET" 'vertico-directory-enter
            "C-i" 'vertico-quick-insert
            "C-o" 'vertico-quick-exit
            "M-o" 'kb/vertico-quick-embark
            "M-G" 'vertico-multiform-grid
            "M-F" 'vertico-multiform-flat
            "M-R" 'vertico-multiform-reverse
            "M-U" 'vertico-multiform-unobtrusive
            "C-l" 'kb/vertico-multiform-flat-toggle)
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)
         (minibuffer-setup . vertico-repeat-save))
  :custom
  (vertico-count 13)
  (vertico-resize nil)
  (vertico-cycle nil)
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  (vertico-multiform-categories
   '((file reverse)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
     (t reverse)))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)))
  :init
  ;; Helper functions
  (defun kb/vertico-multiform-flat-toggle ()
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
  
  (defun kb/vertico-quick-embark (&optional arg)
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))
  
  ;; Tramp completion workaround
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  
  (add-to-list 'completion-styles-alist
               '(basic-remote kb/basic-remote-try-completion
                 kb/basic-remote-all-completions nil))
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  
  ;; Current candidate prefix
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package embark
  :demand t
  :general
  ("C-." 'embark-act)
  ("C-;" 'embark-dwim)
  (:keymaps 'vertico-map "C-." 'embark-act)
  (:keymaps 'embark-heading-map "l" 'org-id-store-link)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  :init
  (global-corfu-mode))

(use-package corfu-terminal
  :after corfu
  :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :init
  (unless (display-graphic-p) (corfu-terminal-mode +1)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package avy
  :demand t
  :general
  (general-def '(normal motion)
    "s" 'evil-avy-goto-char-timer
    "f" 'evil-avy-goto-char-in-line
    "gl" 'evil-avy-goto-line
    ";" 'avy-resume)
  :init
  ;; Avy actions
  (defun my/avy-action-insert-newline (pt)
    (save-excursion (goto-char pt) (newline))
    (select-window (cdr (ring-ref avy-ring 0))))
  
  (defun my/avy-action-kill-whole-line (pt)
    (save-excursion (goto-char pt) (kill-whole-line))
    (select-window (cdr (ring-ref avy-ring 0))))
  
  (defun my/avy-action-embark (pt)
    (unwind-protect
        (save-excursion (goto-char pt) (embark-act))
      (select-window (cdr (ring-ref avy-ring 0))))
    t)
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'my/avy-action-embark
        (alist-get ?i avy-dispatch-alist) 'my/avy-action-insert-newline
        (alist-get ?K avy-dispatch-alist) 'my/avy-action-kill-whole-line))

(use-package consult)

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default
        projectile-enable-caching t))

(use-package treemacs)
(use-package ripgrep)

;; =======================
;; Development Tools
;; =======================

(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 2
        company-selection-wrap-around t))

(use-package flycheck
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package smartparens
  :hook ((emacs-lisp-mode clojure-mode clojurescript-mode clojurec-mode
          lisp-mode scheme-mode fennel-mode) . smartparens-strict-mode)
  :config
  (require 'smartparens-config))

;; =======================
;; Language Configurations
;; =======================

;; Common S-expression keybindings for all Lisp modes
(defun setup-lisp-sexp-keybindings ()
  "Setup common S-expression keybindings for all Lisp modes."
  (my-local-leader-def
    :keymaps 'local
    "k=" 'sp-reindent
    "kW" 'sp-unwrap-sexp
    "kb" 'sp-forward-barf-sexp
    "kB" 'sp-backward-barf-sexp
    "kd" 'sp-kill-sexp
    "kr" 'sp-raise-sexp
    "ks" 'sp-forward-slurp-sexp
    "kS" 'sp-backward-slurp-sexp
    "kt" 'sp-transpose-sexp
    "kw" 'sp-wrap-sexp
    "ky" 'sp-copy-sexp))

;; Emacs Lisp
(defun setup-emacs-lisp-keybindings ()
  "Setup Emacs Lisp specific keybindings."
  (my-local-leader-def
    :keymaps 'local
    "eb" 'eval-buffer
    "ed" 'eval-defun
    "ee" 'eval-last-sexp
    "er" 'eval-region
    "gf" 'find-function
    "gv" 'find-variable
    "hd" 'describe-function
    "hv" 'describe-variable))

(add-hook 'emacs-lisp-mode-hook 'setup-lisp-sexp-keybindings)
(add-hook 'emacs-lisp-mode-hook 'setup-emacs-lisp-keybindings)

;; =======================
;; Clojure LSP Setup
;; =======================

(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable t)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-completion-provider :none) ; Use corfu instead
  (lsp-eldoc-hook nil)
  :config
  ;; Add clojure-lsp to exec path if needed
  (add-to-list 'exec-path "~/.local/bin")
  
  ;; LSP keybindings (add to your local leader)
  (defun setup-clojure-lsp-keybindings ()
    "Setup Clojure LSP specific keybindings."
    (my-local-leader-def
      :keymaps 'local
      ;; Navigation
      "gd" 'lsp-find-definition
      "gr" 'lsp-find-references
      "gi" 'lsp-find-implementation
      "gt" 'lsp-find-type-definition
      
      ;; Documentation
      "hh" 'lsp-describe-thing-at-point
      "hs" 'lsp-signature-activate
      
      ;; Refactoring
      "rr" 'lsp-rename
      "rf" 'lsp-format-buffer
      "ro" 'lsp-organize-imports
      "ra" 'lsp-execute-code-action
      
      ;; Workspace
      "wr" 'lsp-workspace-restart
      "wq" 'lsp-workspace-shutdown
      
      ;; Diagnostics
      "el" 'lsp-ui-flycheck-list
      "en" 'flycheck-next-error
      "ep" 'flycheck-previous-error))
  
  (add-hook 'clojure-mode-hook 'setup-clojure-lsp-keybindings)
  (add-hook 'clojurescript-mode-hook 'setup-clojure-lsp-keybindings)
  (add-hook 'clojurec-mode-hook 'setup-clojure-lsp-keybindings))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-peek-always-show t)
  :general
  (:keymaps 'lsp-ui-mode-map
            "M-." 'lsp-ui-peek-find-definitions
            "M-?" 'lsp-ui-peek-find-references))

;; Optional: Enhanced completion with lsp
(use-package consult-lsp
  :after (lsp-mode consult)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;; Clojure
(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode)))

(use-package cider
  :hook ((clojure-mode clojurescript-mode clojurec-mode) . cider-mode)
  :config
  (defun setup-clojure-keybindings ()
    "Setup Clojure specific keybindings."
    (my-local-leader-def
      :keymaps 'local
      "'" 'cider-jack-in-clj
      "\"" 'cider-jack-in-cljs
      "eb" 'cider-eval-buffer
      "ed" 'cider-eval-defun-at-point
      "ee" 'cider-eval-last-sexp
      "er" 'cider-eval-region
      "gd" 'cider-find-var
      "hd" 'cider-doc
      "rb" 'cider-switch-to-repl-buffer
      "rr" 'cider-restart
      "rq" 'cider-quit
      "tt" 'cider-test-run-test
      "tn" 'cider-test-run-ns-tests))
  
  (dolist (mode '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook))
    (add-hook mode 'setup-lisp-sexp-keybindings)
    (add-hook mode 'setup-clojure-keybindings)))

;; Common Lisp
(use-package sly
  :config
  (defun setup-common-lisp-keybindings ()
    "Setup Common Lisp specific keybindings."
    (my-local-leader-def
      :keymaps 'local
      "'" 'sly
      "eb" 'sly-eval-buffer
      "ed" 'sly-eval-defun
      "ee" 'sly-eval-last-expression
      "er" 'sly-eval-region
      "gd" 'sly-edit-definition
      "hd" 'sly-describe-symbol
      "rb" 'sly-switch-to-output-buffer
      "rr" 'sly-restart-inferior-lisp
      "rq" 'sly-quit-lisp))
  
  (add-hook 'lisp-mode-hook 'setup-lisp-sexp-keybindings)
  (add-hook 'lisp-mode-hook 'setup-common-lisp-keybindings))

;; Scheme
(use-package geiser
  :config
  (defun setup-scheme-keybindings ()
    "Setup Scheme specific keybindings."
    (my-local-leader-def
      :keymaps 'local
      "'" 'run-geiser
      "eb" 'geiser-eval-buffer
      "ed" 'geiser-eval-definition
      "ee" 'geiser-eval-last-sexp
      "er" 'geiser-eval-region
      "gd" 'geiser-edit-symbol-at-point
      "hd" 'geiser-doc-symbol-at-point
      "rb" 'geiser-mode-switch-to-repl))
  
  (add-hook 'scheme-mode-hook 'setup-lisp-sexp-keybindings)
  (add-hook 'scheme-mode-hook 'setup-scheme-keybindings))

;; Fennel
(use-package fennel-mode
  :config
  (defun setup-fennel-keybindings ()
    "Setup Fennel specific keybindings."
    (my-local-leader-def
      :keymaps 'local
      "'" 'fennel-repl
      "ee" 'fennel-eval-last-sexp
      "er" 'fennel-eval-region
      "ef" 'fennel-eval-toplevel-form
      "gd" 'fennel-find-definition
      "hd" 'fennel-show-documentation
      "rr" 'fennel-reload))
  
  (add-hook 'fennel-mode-hook 'setup-lisp-sexp-keybindings)
  (add-hook 'fennel-mode-hook 'setup-fennel-keybindings))

;; Lua
(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (setq lua-indent-level 2)
  
  (defun setup-lua-keybindings ()
    "Setup Lua specific keybindings."
    (my-local-leader-def
      :keymaps 'local
      "'" 'lua-start-process
      "eb" 'lua-send-buffer
      "ed" 'lua-send-defun
      "ee" 'lua-send-current-line
      "er" 'lua-send-region
      "es" 'lua-send-string
      "rb" 'lua-show-process-buffer
      "rr" 'lua-restart-process
      "rq" 'lua-kill-process
      "hd" 'lua-search-documentation))
  
  (add-hook 'lua-mode-hook 'setup-lua-keybindings)
  (add-hook 'lua-mode-hook 'electric-pair-local-mode))

(use-package company-lua
  :after (company lua-mode)
  :config
  (add-hook 'lua-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-lua))))

;; =======================
;; Theme
;; =======================

(use-package leuven-theme
  :config
  (load-theme 'leuven-dark t))

;; =======================
;; Utility Functions
;; =======================

(defun downcase-current-line ()
  "Downcase the entire current line."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (downcase-region start end)))

(defun my/resize-window (&optional arg)
  "Resize window interactively."
  (interactive "p")
  (if (one-window-p) (error "Cannot resize sole window"))
  (or arg (setq arg 1))
  (let (c)
    (catch 'done
      (while t
        (message
         "h=heighten, s=shrink, w=widen, n=narrow (by %d); 1-9=unit, q=quit"
         arg)
        (setq c (read-char))
        (condition-case ()
            (cond
             ((= c ?h) (enlarge-window arg))
             ((= c ?s) (shrink-window arg))
             ((= c ?w) (enlarge-window-horizontally arg))
             ((= c ?n) (shrink-window-horizontally arg))
             ((= c ?\^G) (keyboard-quit))
             ((= c ?q) (throw 'done t))
             ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
             (t (beep)))
          (error (beep)))))
    (message "Done.")))

;; =======================
;; Final Setup
;; =======================

;; Dired integration with Evil
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "SPC") nil))

(message "Configuration loaded successfully!")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
