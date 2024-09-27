;;; -*- lexical-binding: t -*-

;; =======================
;; Setup Configuration
;; =======================
;;

(defvar my-emacs-local-dir "~/.emacs-local.d/"
  "The root directory for all Emacs local, stateful files.")

(unless (file-exists-p my-emacs-local-dir)
  (make-directory my-emacs-local-dir t))

(setq initial-major-mode 'emacs-lisp-mode)

(setq custom-file (expand-file-name "custom.el" my-emacs-local-dir))

;; Load custom file. Doesn't throw an error if the file doesn't exist.
(when (file-exists-p custom-file)
  (load custom-file))

;; use-package check for redundancy
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;; ascii art by Brian MacDonald
;; https://www.asciiart.eu/computers/computers
(setq scratch-ascii-art
      (concat ";;      _________\n"
              ";;     / ======= \\\n"
              ";;    / __________\\\n"
              ";;   | ___________ |\n"
              ";;   | |e        | |\n"
              ";;   | |         | |\n"
              ";;   | |_________| |______________________________\n"
              ";;   \\_____________/   ==== _  _  __   __  __     )\n"
              ";;   /\\\\\\\\\\\\\\\\\\\\\\\\\\\\   |__  |\\/| |__| |   [__    \/\n"
              ";;  / ::::::::::::: \\  |    |||| |  | |__  __]  /\n"
              ";; (_________________) ====                    /\n"
              ";;                                         =D-'\n"))

(defun gpl-v3-text ()
  (concat
   ";; This document is free software: you can redistribute it and/or modify\n"
   ";; it under the terms of the GNU General Public License as published by\n"
   ";; the Free Software Foundation, either version 3 of the License, or\n"
   ";; (at your option) any later version.\n"
   ";;\n"
   ";; This program is distributed in the hope that it will be useful,\n"
   ";; but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
   ";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
   ";; GNU General Public License for more details.\n"
   ";;\n"
   ";; You should have received a copy of the GNU General Public License\n"
   ";; along with this program.  If not, see <https://www.gnu.org/licenses/>.\n"
   ";;\n"
   ))

(setq initial-scratch-message
      (concat
       scratch-ascii-art
       ";; Emacs Version: " emacs-version "\n"
       ";;\n"
       (gpl-v3-text)
       "\n"
       "\n"
       "(message \"eval me\")\n"
       "\n"
       ))

(setq native-comp-eln-load-path (list (expand-file-name "eln-cache/" my-emacs-local-dir)))

(let ((eln-cache-dir (expand-file-name "eln-cache/" my-emacs-local-dir)))
  (unless (file-exists-p eln-cache-dir)
    (make-directory eln-cache-dir)))

;; transient is dep of many useful packages
(setq transient-history-file (concat my-emacs-local-dir "transient/history.el"))

(setq straight-base-dir my-emacs-local-dir)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" my-emacs-local-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)      ; integrate with use-package
(setq straight-use-package-by-default t) ; use straight.el for every package by default



;; =======================
;; Theme
;; =======================

(use-package leuven-theme
  :straight t
  :config
  (load-theme 'leuven-dark :no-confirm))

;; =======================
;; Compatibility Layer
;; =======================

(use-package xclip
  :straight t
  :config
  (setq xclip-mode 1))

;; =======================
;; Evil Mode Configuration
;; =======================
;;;
;;; base/evil.el --- Description


(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)  ; required if I start using which-key
  :config
  (evil-mode 1))

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :straight t
  :after evil)

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

;; =======================
;; General Keybindings
;; =======================
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
    "bb" 'switch-to-buffer
    "bd" 'kill-buffer
    "bi" 'ibuffer
    ;; Commands
    ": " 'execute-extended-command
    ;; Projectile keybindings
    "p p" 'projectile-switch-project
    "SPC" 'projectile-find-file
    ;; More projectile
    "p r" 'projectile-recentf
    "p b" 'projectile-switch-to-buffer
    "p c" 'projectile-compile-project
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
    ;; Window resize
    "wr" 'my/resize-window
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

;; =======================
;; Projectile Configuration
;; =======================
;;;
;;; core.el --- Description

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'default)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recentf)
  )

;; =======================
;; S-Expression Handling
;; =======================
;;
;;; sexp.el -- s-expression editing

(use-package smartparens
  :straight t)

(defun my-sexp-keybindings ()
  (my-leader-def
    :keymaps 'normal
    "k="  'sp-reindent
    "k-"  'sp-reindent
    "kW"  'sp-unwrap-sexp
    "kb"  'sp-forward-barf-sexp
    "kB"  'sp-backward-barf-sexp
    "kc"  'sp-convolute-sexp
    "kdx" 'sp-kill-sexp
    "kr"  'sp-raise-sexp
    "ks"  'sp-forward-slurp-sexp
    "kS"  'sp-backward-slurp-sexp
    "kt"  'sp-transpose-sexp
    "kw"  'sp-wrap-sexp
    "ky"  'sp-copy-sexp))

(add-hook 'emacs-lisp-mode-hook 'my-sexp-keybindings)
(add-hook 'clojure-mode-hook 'my-sexp-keybindings)
(add-hook 'fennel-mode-hook 'my-sexp-keybindings)

;; =======================
;; OS Compatibility
;; =======================
;;;
;;; os.el --- Description

(use-package exec-path-from-shell
  :straight t
  :config
  (when (or (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))

(use-package sudo-edit
  :straight t
  :commands (sudo-edit))

;; =======================
;; Performance Tuning
;; =======================
;;;

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)


;; =======================
;; Navigation Enhancements
;; =======================
;;;
;;; base/nav.el --- Description

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package corfu-terminal
  :after corfu
  :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :init
  (unless (display-graphic-p) (corfu-terminal-mode +1)))


;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  (setq-default truncate-lines t)


  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package vertico
  :demand t                             ; Otherwise won't get loaded immediately
  :straight (vertico :files (:defaults "extensions/*") ; Special recipe to load extensions conveniently
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :general
  (:keymaps '(normal insert visual motion)
            "M-." #'vertico-repeat
            )
  (:keymaps 'vertico-map
            "<tab>" #'vertico-insert ; Set manually otherwise setting `vertico-quick-insert' overrides this
            "<escape>" #'minibuffer-keyboard-quit
            "?" #'minibuffer-completion-help
            "C-M-n" #'vertico-next-group
            "C-M-p" #'vertico-previous-group
            ;; Multiform toggles
            "<backspace>" #'vertico-directory-delete-char
            "C-w" #'vertico-directory-delete-word
            "C-<backspace>" #'vertico-directory-delete-word
            "RET" #'vertico-directory-enter
            "C-i" #'vertico-quick-insert
            "C-o" #'vertico-quick-exit
            "M-o" #'kb/vertico-quick-embark
            "M-G" #'vertico-multiform-grid
            "M-F" #'vertico-multiform-flat
            "M-R" #'vertico-multiform-reverse
            "M-U" #'vertico-multiform-unobtrusive
            "C-l" #'kb/vertico-multiform-flat-toggle
            )
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
         )
  :custom
  (vertico-count 13)
  (vertico-resize nil)
  (vertico-cycle nil)
  ;; Extensions
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
     (t reverse)
     ))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)
     ))
  :init
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))

  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  :config
  (vertico-mode)
  ;; Extensions
  (vertico-multiform-mode)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

(use-package embark
  :demand t
  :general
  (my/leader-keys
     "." 'embark-act) ;; easily accessible 'embark-act' binding.
  ("C-." 'embark-act) ;; overlaps with evil-repeat 
  ("C-;" 'embark-dwim) ;; overlaps with IEdit
  (:keymaps 'vertico-map
            "C-." 'embark-act) ;; embark on completion candidates
  (:keymaps 'embark-heading-map
            "l" 'org-id-store-link)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

 (use-package avy
    :demand t
    :init
(defun my/avy-action-insert-newline (pt)
      (save-excursion
        (goto-char pt)
        (newline))
      (select-window
       (cdr
        (ring-ref avy-ring 0))))
    (defun my/avy-action-kill-whole-line (pt)
      (save-excursion
        (goto-char pt)
        (kill-whole-line))
      (select-window
       (cdr
        (ring-ref avy-ring 0))))
    (defun my/avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t) ;; adds an avy action for embark
    :general
    (general-def '(normal motion)
      "s" 'evil-avy-goto-char-timer
      "f" 'evil-avy-goto-char-in-line
      "gl" 'evil-avy-goto-line ;; this rules
      ";" 'avy-resume)
    :config
    (setf (alist-get ?. avy-dispatch-alist) 'my/avy-action-embark ;; embark integration
          (alist-get ?i avy-dispatch-alist) 'my/avy-action-insert-newline
          (alist-get ?K avy-dispatch-alist) 'my/avy-action-kill-whole-line)) ;; kill lines with avy

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
                   orderless
                   ))
     ))

  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher
     ))
  :init
  (defun orderless--strict-*-initialism (component &optional anchored)
    "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.

If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
    (orderless--separated-by
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
      (cl-loop for char across component collect `(seq word-start ,char))
      (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
      (when (eq anchored 'both)
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

  (defun orderless-strict-initialism (component)
    "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters.  Only non-letters can be in between the
words that start with the initials."
    (orderless--strict-*-initialism component))

  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))

  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  )


(use-package treemacs
  :straight t
  :config
  (my-leader-def
    :keymaps 'normal
    "op" 'treemacs))

(use-package ripgrep
  :straight t
  :config
  (my-leader-def
    :keymaps 'normal
    "rg" 'ripgrep
    "rg" 'projectile-ripgrep))

(use-package consult
  :straight t
  :config
  (my-leader-def
    :keymaps 'normal
    "rf" 'consult-find
    "ss" 'consult-line
    "," 'consult-buffer
    "rm" 'consult-mode-command
    "ro" 'consult-outline
    "ri" 'consult-imenu
    "rk" 'consult-flymake
    "rs" 'consult-locate))

;; dired stuff
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "SPC") nil))

;; =======================
;; UI Enhancements
;; =======================
;;;
;;; ui.el --- Description

(if (eq system-type 'darwin)  ; macOS
    (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'default nil :height 120))

(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-paren-match '(bold underline))
  :config
  (my-leader-def
    :keymaps 'normal
    "mt" 'modus-themes-select))

(use-package emacs
  :hook
  ((prog-mode text-mode) . display-line-numbers-mode))

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; no scrollbar
(scroll-bar-mode -1)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; maximize the initial frame automatically
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; https://www.emacswiki.org/emacs/HiroseYuuji
(defun my/resize-window (&optional arg)
  "*Resize window interactively."
  (interactive "p")
  (if (one-window-p) (error "Cannot resize sole window"))
  (or arg (setq arg 1))
  (let (c)
    (catch 'done
      (while t
	(message
	 "h=heighten, s=shrink, w=widen, n=narrow (by %d);  1-9=unit, q=quit"
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
;; Org Mode Configuration
;; =======================
;;;
;;; org.el --- Description

(defun setup-org-mode-keys ()
  (my-local-leader-def
    :keymaps 'org-mode-map
    "o a" 'org-agenda))

(setup-org-mode-keys)

;; =======================
;; Help System Enhancements
;; =======================
;;;
;;; base/help.el --- Description

;; The best resource within Emacs for learning about basic Emacs Lisp (Elisp)
;; syntax, including how to perform loops, is the Emacs Lisp Reference
;; Manual. You can access it by running `M-x info` and then selecting "Emacs
;; Lisp".
;;
;; For loops specifically, navigate to the section that talks about iteration,
;; which will provide you with details on using `while`, `dolist`, `dotimes`,
;; and other loop constructs. You'll get in-depth explanations, examples, and
;; even some best practices.
;;
;; To go directly to the Emacs Lisp Reference Manual from within Emacs, you can
;; also execute `M-x info-emacs-lisp-reference`, provided that the `info` pages
;; for Emacs Lisp are installed on your system.
;;
;; For quicker, context-based help, you can use the following:
;;
;; - `M-x describe-function`: Describe a specific function.
;; - `M-x describe-variable`: Describe a variable.  `C-h k` (or
;; - `M-x describe-key`: Describe what a specific keybinding does,
;;    and shows the Elisp function that it calls.
;;
;; These are great for learning what a specific function or variable does, but
;; they are less suitable for understanding larger programming constructs like
;; loops.
;;
;; The combination of the Emacs Lisp Reference Manual for in-depth learning and
;; `describe-function`, `describe-variable`, and `describe-key` for quick
;; lookups covers most of what you'll need to become proficient in Elisp.


(defun my-custom-help-setup ()
  "Open *info* in new window below current focused window."
  (interactive)
  (split-window-below)
  (other-window 1)
  (info-emacs-manual)  ;; Initialize the apropos command prompt
  )


(my-leader-def
  :keymaps 'normal
  "hh" 'my-custom-help-setup)

(my-local-leader-def
  :keymaps 'Info-mode-map
  "n" 'Info-next
  "p" 'Info-prev
  "u" 'Info-up
  "g" 'Info-goto-node
  "S" 'Info-search
  "s" 'swiper
  "i" 'Info-index
  "l" 'Info-history-back
  "f" 'Info-follow-nearest-node
  "m" 'Info-menu
  "q" 'delete-window)

(add-hook 'Info-mode-hook
          (lambda () (define-key Info-mode-map (kbd "SPC") nil)))

;; =======================
;; Autoloads
;; =======================
;;;
;;; base/autoload.el --- Description

(defun wrap-in-quotes ()
  "Wrap each line in the current region in double quotes."
  (interactive)
  (save-excursion
    (let ((start (region-beginning))
          (end (region-end)))
      (goto-char start)
      (while (< (point) end)
        (beginning-of-line)
        (insert "\"")
        (end-of-line)
        (insert "\"")
        (forward-line)
        (setq end (+ end 2))))))

;; =======================
;; Build Systems
;; =======================
;;

;; =======================
;; CI/CD Configuration
;; =======================
;;
;;; ci.el --- Description

;; =======================
;; Virtualization
;; =======================
;;
;;; virt.el --- Description

;; =======================
;; Container Management
;; =======================
;;

;; =======================
;; Linux Kernel Development
;; =======================
;;
;;; kernel.el --- Description

;; =======================
;; Patch Management
;; =======================
;;

;; =======================
;; Utilities
;; =======================
;;

(use-package vterm
  :straight t
  )

(use-package multi-vterm
  :straight t
  :config
  (my-leader-def
    :keymaps 'normal
    "ot" 'multi-vterm)
  )

(use-package emamux
  :straight t
  )

(use-package restclient
  :straight t)

;; =======================
;; Database Management
;; =======================
;;
;;; db.el --- Description

;; =======================
;; Debugging Tools
;; =======================
;;

;;; debug.el --- Description

(setq gdb-many-windows t
      gdb-use-separate-io-buffer t)

(advice-add 'gdb-setup-windows :after
            (lambda () (set-window-dedicated-p (selected-window) t)))

(defconst gud-window-register 123456)

(defun gud-quit ()
  (interactive)
  (gud-basic-call "quit"))

(add-hook 'gud-mode-hook
          (lambda ()
            (gud-tooltip-mode)
            (window-configuration-to-register gud-window-register)
            (local-set-key (kbd "C-q") 'gud-quit)))

(advice-add 'gud-sentinel :after
            (lambda (proc msg)
              (when (memq (process-status proc) '(signal exit))
                (jump-to-register gud-window-register)
                (bury-buffer))))


;; =======================
;; Git Integration
;; =======================
;;
;;; git.el --- Description

(use-package magit
  :straight t
  :config
  ;; Your magit configuration
  (my-leader-def
    :keymaps 'normal
    "gg" 'magit-status
    "gb" 'magit-blame
    "gc"  'magit-clone
    "gl"  'magit-log-all
    "gff" 'magit-find-file
    "gd"  'magit-diff
    "gD"  'magit-diff-buffer-file
    "gP"  'magit-push
    "gF"  'magit-pull
    "gR"  'magit-rebase
    "gr"  'magit-revert
    "gC"  'magit-commit
    "gS"  'magit-stage-file
    "gU"  'magit-unstage-file
    "gs"  'magit-stage-modified
    "gu"  'magit-unstage-all
    "go"  'magit-checkout
    "gF"  'magit-fetch
    "gm"  'magit-merge
    "gt"  'magit-tag
    "gW"  'magit-worktree
))

;;;###autoload
(defun +my-magit/commit-log-for-directory (dir)
  "Open a Magit log buffer filtered for changes in DIR."
  (interactive "sDirectory: ")
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (if default-directory
        (magit-log-head nil (list dir))
      (message "Not inside a Git repository."))))

;;;###autoload
(defun +my-magit/find-removed-files-in-dir (dir)
  "Find when files were removed in DIR using Git."
  (interactive "sDirectory: ")
  (let ((git-root (locate-dominating-file default-directory ".git"))
        (output-buf (get-buffer-create "*Removed Files*")))
    (if git-root
        (progn
          (shell-command
           (format "git log --diff-filter=D --summary -- '%s'" dir)
           output-buf)
          (pop-to-buffer output-buf))
      (message "Not inside a Git repository."))))

;; =======================
;; Web Browsing
;; =======================
;;
;;; browse.el --- Description

;; =======================
;; Email Configuration
;; =======================

(cond
 ((eq system-type 'darwin)  ; macOS
  (add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu4e"))
 ((eq system-type 'gnu/linux)  ; Linux
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")))

;; example configuration for mu4e

;; make sure mu4e is in your load-path
(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-sent-folder        "/Sent")
(setq mu4e-drafts-folder      "/Drafts")
(setq mu4e-trash-folder       "/Trash")
(setq mu4e-refile-folder      "/Archive")

(setq smtpmail-smtp-user      "brianwitte@mailfence.com")
(setq mu4e-compose-signature  "Thanks,\nBrian Witte")

(defun determine-email-flags ()
  "Determine if the email is a reply and return appropriate flags."
  (if (save-excursion
        (goto-char (point-min))
        (re-search-forward "^Subject: Re:" nil t))
      "RS"
    "S"))

(defun rename-and-move-file (file-path sent-folder draft-folder)
  "Rename and move the email file between draft and sent folders."
  (let ((draft-file (file-name-nondirectory file-path))
        (flags (determine-email-flags))
        sent-file)
    (setq sent-file
          (replace-regexp-in-string ",DS" (concat "," flags) draft-file))
    (rename-file
     (concat draft-folder draft-file)
     (concat sent-folder sent-file))
    sent-file))

(defun send-email-file (sent-folder sent-file)
  "Send the email file using msmtp."
  (let ((send-command
         (format "cat %s | msmtp -t -a default --debug"
                 (shell-quote-argument (concat sent-folder sent-file)))))
    (compile send-command)
    (= 0 compilation-exit-status)))

(defun append-to-msmtp-log (file-path)
  "Append a log entry for the given file-path."
  (let ((current-time-iso8601
         (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
    (append-to-file
     (format "%s Sending: %s\n" current-time-iso8601 file-path)
     nil
     "~/.msmtp.log")))

(defun send-current-file-msmtp ()
  "Send the current buffer's file via msmtp."
  (interactive)
  (let ((file-path (buffer-file-name))
        (sent-folder ".mail/mailfence.com/Sent/cur/")
        (draft-folder ".mail/mailfence.com/Drafts/cur/")
        sent-file)
    (if (not file-path)
        (message "Buffer is not visiting a file")
      (setq sent-file
            (rename-and-move-file file-path sent-folder draft-folder))
      (append-to-msmtp-log (concat sent-folder sent-file))
      (if (send-email-file sent-folder sent-file)
          (delete-file
           (concat draft-folder (file-name-nondirectory file-path)))
        (rename-file
         (concat sent-folder sent-file)
         (concat draft-folder
                 (file-name-nondirectory file-path)))))))

(defun open-msmtp-log-and-drafts ()
  "Open msmtp log file in a buffer and the drafts folder in dired mode."
  (interactive)
  ;; Delete other windows to start with a clean slate
  (delete-other-windows)

  ;; Open msmtp log in the current window
  (find-file "~/.msmtp.log")  ; Replace with your msmtp log path

  ;; Split window horizontally and open drafts in dired in the new window
  (split-window-right)
  (other-window 1)
  (dired "~/.mail/mailfence.com/Drafts/cur/")  ; Replace with your Drafts path

  ;; Move focus back to the msmtp log
  (other-window -1))


(defun fill-email-region-hard-break (start end)
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((line-start (point))
            (max-col fill-column))
        (if (looking-at "^> ")
            (progn
              (forward-char 2)
              (setq max-col (- max-col 2))))
        (move-to-column max-col)
        (if (or (>= (point) (line-end-position)) (eobp))
            (forward-line 1)
          (backward-word)
          (forward-word)
          (if (< (point) line-start)
              (goto-char line-start))
          (insert "\n> ")))))
  (widen))


;; =======================
;; Bot Configuration
;; =======================
;;
;; -- bots.el

;; =======================
;; File Keybindings
;; =======================
;;;
;;; file_keymaps.el --- Keymaps to open specific files/dirs


;; Helper function to create multiple navigation functions
(defun create-nav-functions (file-map)
  "Create functions to open files specified in FILE-MAP.
FILE-MAP is a list of (NAME . PATH) pairs."
  (dolist (entry file-map)
    (let ((name (car entry))
          (path (cdr entry)))
      (eval `(defun ,(intern (concat "nav-to-" name)) ()
               ,(concat "Open the " name " file.")
               (interactive)
               (find-file ,path))))))

;; Define file paths (lexically scoped to this file)
(let ((paths-for-keymaps
       '(("learn-elisp" . "~/.config/emacs/learn-emacs-lisp.el")
         ("init" . "~/.config/emacs/init.el")
         ("zshrc" . "~/.zshrc")
         ("gtd" . "~/org/gtd.org")
         ("file-bindings" . "~/.emacs/config/lisp/file-bindings.el")
         ("scripts-dir" . "~/scripts")
         ("xdg-home-dir" . "~/.config"))))
  (create-nav-functions paths-for-keymaps))

;; Define core visual keybindings
(defun my-file-keymaps ()
  (my-leader-def
    :keymaps 'normal
    "a l" 'nav-to-learn-elisp
    "a i" 'nav-to-init
    "a g" 'nav-to-gtd
    "a k" 'nav-to-file-keymaps
    "a z" 'nav-to-zshrc
    "d e" 'nav-to-elisp-dir
    "d s" 'nav-to-scripts-dir
    "d x" 'nav-to-xdg-home-dir
    ))

(my-file-keymaps)

;; =======================
;; Main Personal Config
;; =======================
;;;

(setq user-full-name "Brian Witte"
      user-mail-address "brianwitte@mailfence.com")

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; Wrap lines at 80 characters
;; (setq-default fill-column 80)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; auto-save-list dir
(let ((auto-save-list-dir (expand-file-name "auto-save-list/" my-emacs-local-dir)))
  (unless (file-exists-p auto-save-list-dir)
    (make-directory auto-save-list-dir)))

(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" my-emacs-local-dir))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Start proced in a similar manner to dired
(global-set-key (kbd "C-x p") #'proced)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "s-q") #'fill-paragraph)
(global-set-key (kbd "s-x") #'execute-extended-command)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; enable some commands that are disabled by default
(put 'erase-buffer 'disabled nil)

;; make it possible to navigate to the C source of Emacs functions
(setq find-function-C-source-directory "~/src/emacs-29.1")

;; auto-create missing folders
(defun er-auto-create-missing-dirs ()
  "Make missing parent directories automatically."
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'er-auto-create-missing-dirs)


;;; built-in packages
(use-package paren
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

(use-package calendar
  :config
  ;; weeks start on Monday
  (setq calendar-week-start-day 1))

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" my-emacs-local-dir))
  ;; activate it for all buffers
  (setq-default save-place t))


(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" my-emacs-local-dir))
  (savehist-mode +1))

;; (use-package desktop
;;   :config
;;   (desktop-save-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" my-emacs-local-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
;;  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; log and provide
(message "Evaluated %s" (or (buffer-file-name) "unknown file"))

;; =======================
;; Text Editing Configuration
;; =======================
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

(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

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
          tsx-ts-mode
          typescript-ts-mode
          js-ts-mode) . lsp-deferred)
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

;; =======================
;; Configuration Formats
;; =======================
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
        jq-interactive-default-options ""))

;; =======================
;; LSP Configuration
;; =======================
;;
;;; lsp.el --- Description

(use-package lsp-mode
  :straight t)

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-sideline-global ((t (:italic t))))
  (lsp-ui-peek-highlight
   ((t (:foreground unspecified :background unspecified :inherit isearch))))
  :config
  (with-eval-after-load 'evil
    (add-hook
     'buffer-list-update-hook
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

;; =======================
;; Tree Sitter Configuration
;; =======================
;;
;;; tree_sitter.el --- Description

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

;; =======================
;; Elisp Configuration
;; =======================
;;;
;;; elisp.el --- Description


(defun eval-last-sexp-and-show-temporarily ()
  "Evaluate the sexp before the point and momentarily display the result."
  (interactive)
  ;; Evaluate the preceding sexp and get the result.
  (let*
    (
      (result (eval-last-sexp nil))
      (result-string (format "\n;=> %S" result))
      ;; The position to display the message at.
      (display-pos (1+ (point))))
    ;; Use `momentary-string-display` to show the result temporarily.
    ;; By default, it will disappear with the next keystroke.
    (momentary-string-display result-string display-pos)))

(defun setup-elisp-mode-keys ()
  (my-local-leader-def
    :keymaps 'emacs-lisp-mode-map
    "m"
    'macrostep-expand
    "eb"
    'eval-buffer
    "ed"
    'eval-defun
    "ee"
    'eval-last-sexp-and-show-temporarily
    "er"
    'eval-region
    "el"
    'load-library
    "gf"
    'find-function
    "gv"
    'find-variable
    "gl"
    'find-library))

(add-to-list 'exec-path (expand-file-name "fmt-bin" user-emacs-directory))

(require 'reformatter)
(reformatter-define elatto
  :program (expand-file-name "elatto" (locate-user-emacs-file "fmt-bin"))
  :args `("--stdin"
          "--stdout"
          "--fmt-defs-dir"
          ,(expand-file-name "fmt-bin" user-emacs-directory)
          "--fmt-defs"
          "elatto.overrides.json"
          "--fmt-style"
          "fixed")
  :lighter " Elatto")


(add-hook 'emacs-lisp-mode-hook 'setup-elisp-mode-keys)


;; =======================
;; Shell Script Configuration
;; =======================

;; Make sure 'shfmt' is in your PATH and you have the following options:
;; - -i 4: Use 4 spaces for indentation.
;; - -ci: Indent switch cases.
;; - -bn: Place binary operators like && and | at the start of a new line.
;; - -sr: Place a space before redirect operators.
;; - -kp: Keep column alignment padding.
;; - -fn: Place function opening braces on a new line.

(require 'reformatter)
(reformatter-define shfmt
  :program "shfmt"
  :args '("-i" "4" "-ci" "-bn" "-sr" "-kp")
  :lighter " shfmt")


;; =======================
;; C/C++ Configuration
;; =======================

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

(defun remove-log-debug-statements ()
  "Remove all lines containing log_debug statements in C files."
  (interactive)
  (when (derived-mode-p 'c-mode 'c++-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "log_debug(" nil t)
        (let ((start (match-beginning 0)))
          (if (search-forward ";" nil t)
              (delete-region start (point))))))))


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

;;; cc.el ends here
;; =======================
;; Clojure Configuration
;; =======================

;;; clj.el --- Clojure Development Configuration

(use-package cider
  :straight t
  :config
  (defun my-clojure-mode-cider-keybindings ()
    (my-local-leader-def
      :states 'normal
      :keymaps '(clojure-mode-map clojurescript-mode-map clojurec-mode-map)
      " '"  #'cider-jack-in-clj
      " \"" #'cider-jack-in-cljs
      " c"  #'cider-connect-clj
      " C"  #'cider-connect-cljs
      " m"  #'cider-macroexpand-1
      " M"  #'cider-macroexpand-all ;; Debug
      " d d" #'cider-debug-defun-at-point
      ;; Eval
      " e b" #'cider-eval-buffer
      " e d" #'cider-eval-defun-at-point
      " e D" #'cider-insert-defun-in-repl
      " e e" #'cider-eval-last-sexp
      " e E" #'cider-insert-last-sexp-in-repl
      " e r" #'cider-eval-region
      " e R" #'cider-insert-region-in-repl
      " e u" #'cider-undef
      ;; Goto
      " g b" #'cider-pop-back
      " g g" #'cider-find-var
      " g n" #'cider-find-ns
      ;; Help
      " h a" #'cider-apropos
      " h c" #'cider-clojuredocs
      " h d" #'cider-doc
      " h j" #'cider-javadoc
      " h w" #'cider-clojuredocs-web
      ;; Inspect
      " i e" #'cider-enlighten-mode
      " i i" #'cider-inspect
      " i r" #'cider-inspect-last-result
      ;; Namespace
      " n n" #'cider-browse-ns
      " n N" #'cider-browse-ns-all
      " n r" #'cider-ns-refresh
      ;; Print
      " p p" #'cider-pprint-eval-last-sexp
      " p P" #'cider-pprint-eval-last-sexp-to-comment
      " p d" #'cider-pprint-eval-defun-at-point
      " p D" #'cider-pprint-eval-defun-to-comment
      " p r" #'cider-pprint-eval-last-sexp-to-repl
      ;; REPL
      " r n" #'cider-repl-set-ns
      " r q" #'cider-quit
      " r r" #'cider-ns-refresh
      " r R" #'cider-restart
      " r b" #'cider-switch-to-repl-buffer
      " r B" #'+clojure/cider-switch-to-repl-buffer-and-switch-ns
      " r c" #'cider-find-and-clear-repl-output
      " r f" #'cider-load-file
      " r l" #'cider-load-buffer
      " r L" #'cider-load-buffer-and-switch-to-repl-buffer
      ;; Test
      " t a" #'cider-test-rerun-test
      " t l" #'cider-test-run-loaded-tests
      " t n" #'cider-test-run-ns-tests
      " t p" #'cider-test-run-project-tests
      " t r" #'cider-test-rerun-failed-tests
      " t s" #'cider-test-run-ns-tests-with-filters
      " t t" #'cider-test-run-test))

  (add-hook 'clojure-mode-hook 'my-clojure-mode-cider-keybindings)
  (add-hook 'clojurescript-mode-hook 'my-clojure-mode-cider-keybindings)
  (add-hook 'clojurec-mode-hook 'my-clojure-mode-cider-keybindings))


(require 'reformatter)
(reformatter-define zprint
  :program "zprint-clj"
  :lighter " ZprintCLJ")


;;; emx-clj.el ends here
;; =======================
;; Fennel Configuration
;; =======================
;;
;;; fnl.el --- Description

(use-package fennel-mode
  :straight t
  :config
  (defun my-fennel-mode-keybindings ()
    (my-local-leader-def
      :states 'normal
      :keymaps '(fennel-mode-map)
      " '"  #'fennel-repl
      " m"  #'fennel-macroexpand
      " e f" #'fennel-eval-toplevel-form
      " e e" #'fennel-eval-last-sexp
      " e r" #'fennel-eval-region
      " e n" #'fennel-eval-form-and-next
      ;; Help
      " h d" #'fennel-show-documentation
      " h v" #'fennel-show-variable-documentation
      ;; Namespace
      " n r" #'fennel-reload
      ;; REPL
      " r r" #'fennel-reload
      ;; Test
      ;;" t t" #'my-fennel-run-tests)
      ))

  (defun my-fennel-evil-keybindings ()
    (evil-define-key 'normal fennel-mode-map
      "gd" 'fennel-find-definition))

  (add-hook 'fennel-mode-hook 'my-fennel-mode-keybindings)
  (add-hook 'fennel-mode-hook 'my-fennel-evil-keybindings))

;; =======================
;; JavaScript Configuration
;; =======================

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

;; #######################

;; =======================
;; Lua Configuration
;; =======================

(setq lsp-clients-lua-language-server-install-dir
      "/home/bkz/lang-servers/lua-language-server/")

(use-package lua-mode
  :straight t
  :init
  ;; lua-indent-level defaults to 3. let's keep it? idk
  (setq lua-indent-level 4)
  :config

  (add-hook 'lua-mode-local-vars-hook #'lsp 'append)
  (add-hook 'lua-mode-local-vars-hook #'tree-sitter 'append))

;; #######################

;; =======================
;; OCaml Configuration
;; =======================

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

;; #######################

;; =======================
;; Poke Configuration
;; =======================

;;; poke.el --- Poke Development Configuration

(use-package poke-mode
  :straight t)

;; #######################

;; =======================
;; Ruby Configuration
;; =======================
;;

;;; ruby.el --- Description

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook ((ruby-mode . lsp-deferred))
  :init
  (setq lsp-enable-snippet nil  ;; Disable snippets
        lsp-enable-symbol-highlighting nil  ;; Disable symbol highlighting
        lsp-enable-text-document-color nil  ;; Disable text document color
        lsp-enable-on-type-formatting nil  ;; Disable auto formatting
        lsp-enable-indentation nil  ;; Disable indentation
        lsp-diagnostics-provider :none)
  )  ;; Disable diagnostics


(use-package inf-ruby
  :straight t
  :config
  (defun inf-ruby-start-pry ()
    "Run an inferior Ruby process with Pry in a buffer.
If there is a Ruby process running in an existing buffer with Pry, switch
to that buffer. Otherwise create a new buffer with Pry."
    (interactive)
    (let* ((impl "pry")
           (command (cdr (assoc impl inf-ruby-implementations))))
      (run-ruby command impl))))

(defun my-ruby-lsp-executable ()
  (let ((ruby-exec (expand-file-name "3.1.3/bin/ruby-lsp" (getenv "HOME"))))
    (if (file-executable-p ruby-exec)
        ruby-exec
      (error "ruby-lsp not found in ~/.rubies/3.1.3"))))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable t
        lsp-ui-imenu-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-update-mode 'point))

(use-package rspec-mode
  :straight t
  :defer t
  :init
  ;; Autoload RSpec minor mode in ruby buffers
  (add-hook 'ruby-mode-hook 'rspec-mode)
  :config
  ;; Use rspec instead of rake
  (setq rspec-use-rake-when-possible nil)
  ;; Configurations, keybindings, etc.
  )

(require 'reformatter)
(reformatter-define rubyfmt
  :program "rubyfmt"
  :lighter " Rubyfmt")

(defun my-inf-ruby-console ()
  "Open inf-ruby-console and return its buffer."
  (inf-ruby-console-auto)
  (get-buffer "*inf-ruby*"))

(defun my-ruby-scratch-buffer ()
  "Open or get a Ruby scratch buffer."
  (or (get-buffer "*ruby-scratch*")
      (generate-new-buffer "*ruby-scratch*")))

(defun my-rails-console ()
  "Open Rails console."
  (interactive)
  (projectile-rails-console)
  (get-buffer "*Rails*"))

;; load local chruby package
(load (expand-file-name
       ".config/emacs/lisp/packages/chruby"
       gnus-home-directory))

;; then require it
(require 'chruby)

;; =======================
;; Zig Configuration
;; =======================
;;

(use-package zig-mode
  :straight t
  :config
  (defun my-zig-mode-keybindings ()
    (my-local-leader-def
      :states 'normal
      :keymaps '(zig-mode-map)
      " e c" #'zig-compile
      " e r" #'zig-run
      " f b" #'zig-format-buffer
      " t t" #'zig-test-buffer)
    )
  (add-hook 'zig-mode-hook 'my-zig-mode-keybindings))

;;; test.el --- Description
;; =======================
;; Test Framework
;; =======================
;;
;;; test.el --- Description

;; =======================
;; Miscellaneous
;; =======================
;;

;;; misc.el --- Description

(defun jump-to-config-section ()
  "Jump to a section in the single.el file based on fuzzy matching."
  (interactive)
  (let* ((sections '("Setup Configuration"
                     "Theme"
                     "Compatibility Layer"
                     "Evil Mode Configuration"
                     "General Keybindings"
                     "Projectile Configuration"
                     "S-Expression Handling"
                     "OS Compatibility"
                     "Performance Tuning"
                     "Navigation Enhancements"
                     "UI Enhancements"
                     "Org Mode Configuration"
                     "Help System Enhancements"
                     "Autoloads"
                     "Build Systems"
                     "CI/CD Configuration"
                     "Virtualization"
                     "Container Management"
                     "Linux Kernel Development"
                     "Patch Management"
                     "Utilities"
                     "Database Management"
                     "Debugging Tools"
                     "Git Integration"
                     "Web Browsing"
                     "Email Configuration"
                     "Bot Configuration"
                     "File Keybindings"
                     "Main Personal Config"
                     "Text Editing Configuration"
                     "Configuration Formats"
                     "LSP Configuration"
                     "Tree Sitter Configuration"
                     "Elisp Configuration"
                     "C/C++ Configuration"
                     "Clojure Configuration"
                     "Fennel Configuration"
                     "JavaScript Configuration"
                     "Lua Configuration"
                     "OCaml Configuration"
                     "Poke Configuration"
                     "Ruby Configuration"
                     "Zig Configuration"
                     "Miscellaneous"
                     "Test Framework"
                     "Core Configuration"))
         (chosen-section (completing-read "Jump to section: " sections))
         (search-pattern (concat ";; =======================\n;; " chosen-section "\n;; =======================")))
    (goto-char (point-min))
    (if (search-forward search-pattern nil t)
        (progn
          ;; Move to the start of the section
          (beginning-of-line)
          ;; Recenter so that the section is two lines down from the top
          (recenter 2)
          (message "Jumped to %s" chosen-section))
      (message "Section not found: %s" chosen-section))))

(defun emacs-config-keybindings ()
  (my-leader-def
    :states 'normal
    "e j" #'jump-to-config-section))

(emacs-config-keybindings)


;; RANDOM STUFF BELOW

(defun insert-ascii-main-comment ()
  "Insert an ASCII art comment block for 'main' with explanation."
  (interactive)
  (insert "##################################################\n")
  (insert "#                  _\n")
  (insert "#                 (_)\n")
  (insert "#  _ __ ___   __ _ _ _ __\n")
  (insert "# | '_ ` _ \\ / _` | | '_ \\\n")
  (insert "# | | | | | | (_| | | | | |\n")
  (insert "# |_| |_| |_|\\__,_|_|_| |_|\n")
  (insert "#\n")
  (insert "##################################################\n")
  (insert "# CHANGEME - explain what this \"main\" does\n")
  (insert "##################################################\n"))



;; Now you can bind this function to a key combination if desired


(require 'url)  ;; Ensure url functionality is available

(defun download-file (url destination)
  "Download a file from the given URL and save it to the DESTINATION path."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "\n\n") ;; Skip the HTTP headers
    (write-region (point) (point-max) destination)))

(defun get-files-sh (directory)
  "Download the 'files.sh' script to the DIRECTORY of your choosing."
  (interactive "DSelect destination directory: ")
  (let ((url "https://gist.githubusercontent.com/brianwitte/cb133e044bbd8f45a178a8b6c6b95112/raw/6229c86bb5ed063ad55c511981d4fc32ca605fb5/files.sh")
        (destination (concat (file-name-as-directory directory) "files.sh")))
    (download-file url destination)
    (message "Downloaded files.sh to %s" destination)))

(defun get-license-rb (directory)
  "Download the 'license.rb' script to the DIRECTORY of your choosing."
  (interactive "DSelect destination directory: ")
  (let ((url "https://gist.githubusercontent.com/brianwitte/96e22d09347e34e689166501d48ab0f4/raw/be80b96a0ddc8f427bf8600b0a314b53167c5be7/license.rb")
        (destination (concat (file-name-as-directory directory) "license.rb")))
    (download-file url destination)
    (message "Downloaded license.rb to %s" destination)))

