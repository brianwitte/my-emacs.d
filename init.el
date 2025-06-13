;;; -*- lexical-binding: t -*-

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

;; Integrate straight with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; =======================
;; Custom Scratch Message
;; =======================

(setq initial-scratch-message
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

;; =======================
;; Basic Settings
;; =======================

;; Hide startup screen
(setq inhibit-startup-screen t)

;; Better defaults
(setq-default
 indent-tabs-mode nil
 tab-width 4
 fill-column 80)

;; Backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Recentf
(recentf-mode 1)
(setq recentf-max-saved-items 1000)

;; Show matching parens
(show-paren-mode 1)

;; Electric pair mode
(electric-pair-mode 1)

;; Line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; =======================
;; Theme
;; =======================

(use-package leuven-theme
  :config
  (load-theme 'leuven-dark t))

;; =======================
;; System Integration
;; =======================

(use-package xclip
  :config
  (xclip-mode 1))

;; =======================
;; Evil Mode
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
  
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  
  ;; Set initial state for some modes
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

;; =======================
;; General Keybindings
;; =======================

(use-package general
  :after evil
  :config
  (general-create-definer my-leader-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer my-local-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix ",")

  ;; File operations
  (my-leader-def
    "ff" 'find-file
    "fs" 'save-buffer
    "fr" 'recentf-open-files)

  ;; Buffer operations
  (my-leader-def
    "bb" 'switch-to-buffer
    "bd" 'kill-buffer
    "bi" 'ibuffer
    "bn" 'next-buffer
    "bp" 'previous-buffer)

  ;; Window operations
  (my-leader-def
    "ws" 'split-window-below
    "wv" 'split-window-right
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "wd" 'delete-window
    "wD" 'kill-buffer-and-window
    "wmm" 'delete-other-windows)

  ;; Commands
  (my-leader-def
    ":" 'execute-extended-command
    "SPC" 'projectile-find-file)

  ;; Projectile
  (my-leader-def
    "pp" 'projectile-switch-project
    "pb" 'projectile-switch-to-buffer
    "pc" 'projectile-compile-project
    "pk" 'projectile-kill-buffers
    "pd" 'projectile-dired
    "pr" 'projectile-replace))

;; =======================
;; Projectile
;; =======================

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default
        projectile-enable-caching t))

;; =======================
;; Smartparens for Lisps
;; =======================

(use-package smartparens
  :hook ((emacs-lisp-mode 
          clojure-mode 
          clojurescript-mode 
          clojurec-mode
          lisp-mode 
          scheme-mode 
          fennel-mode) . smartparens-strict-mode)
  :config
  (require 'smartparens-config))

;; =======================
;; Lisp Configuration
;; =======================

;; Common lisp keybindings function
(defun setup-lisp-sexp-keybindings ()
  "Setup common S-expression keybindings for all Lisp modes."
  (my-local-leader-def
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
  
  (add-hook 'clojure-mode-hook 'setup-lisp-sexp-keybindings)
  (add-hook 'clojure-mode-hook 'setup-clojure-keybindings)
  (add-hook 'clojurescript-mode-hook 'setup-lisp-sexp-keybindings)
  (add-hook 'clojurescript-mode-hook 'setup-clojure-keybindings)
  (add-hook 'clojurec-mode-hook 'setup-lisp-sexp-keybindings)
  (add-hook 'clojurec-mode-hook 'setup-clojure-keybindings))

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

;; =======================
;; Lua Configuration
;; =======================

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

;; Enhanced Lua support with company completion
(use-package company-lua
  :after (company lua-mode)
  :config
  (add-hook 'lua-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-lua))))

;; =======================
;; Completion
;; =======================

(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 2
        company-selection-wrap-around t))

;; =======================
;; Syntax Checking
;; =======================

(use-package flycheck
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

;; =======================
;; Helpful Utilities
;; =======================

;; Text manipulation functions
(defun downcase-current-line ()
  "Downcase the entire current line."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (downcase-region start end)))

;; Window resize function
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

;; Add resize binding
(my-leader-def "wr" 'my/resize-window)

;; Add text manipulation binding
(my-leader-def "dl" 'downcase-current-line)

;; =======================
;; Final Configuration
;; =======================

;; Hide warnings buffer
(add-to-list 'display-buffer-alist
             '("^\\*Warnings\\*" . (display-buffer-no-window)))

;; Custom navigation
(with-eval-after-load 'evil
  (define-key evil-normal-state-map "gd" 'xref-find-definitions)
  (define-key evil-normal-state-map "gD" 'xref-find-definitions-other-window))

(message "Configuration loaded successfully!")
