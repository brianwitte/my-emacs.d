;;; -*- lexical-binding: t -*-

;; =======================
;; Setup Configuration
;; =======================

(defvar my-emacs-local-dir "~/.emacs-local.d/"
  "The root directory for all Emacs local, stateful files.")

(unless (file-exists-p my-emacs-local-dir)
  (make-directory my-emacs-local-dir t))

;; Directory setup function to reduce repetition
(defun ensure-directory (dir)
  "Create directory DIR if it doesn't exist."
  (let ((path (expand-file-name dir my-emacs-local-dir)))
    (unless (file-exists-p path)
      (make-directory path t))
    path))

;; Initial setup
(setq initial-major-mode 'emacs-lisp-mode
      custom-file (expand-file-name "custom.el" my-emacs-local-dir))

;; Load custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

;; use-package setup
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;; ASCII art and license text
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
       "\n"))

;; Setup directories and caches
(ensure-directory "eln-cache/")
(setq native-comp-eln-load-path (list (expand-file-name "eln-cache/" my-emacs-local-dir))
      transient-history-file (concat my-emacs-local-dir "transient/history.el")
      straight-base-dir my-emacs-local-dir)

;; Straight.el setup
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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; =======================
;; Theme
;; =======================

(use-package leuven-theme
  :config
  (load-theme 'leuven-dark :no-confirm))

;; =======================
;; Compatibility Layer
;; =======================

(use-package xclip
  :config
  (setq xclip-mode 1))

;; =======================
;; Evil Mode Configuration
;; =======================

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :after evil)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; =======================
;; General Keybindings
;; =======================

;; Hide warnings buffer
(add-to-list 'display-buffer-alist
             '("^\\*Warnings\\*" . (display-buffer-no-window)))

;; Text manipulation functions
(defun downcase-current-line ()
  "Downcase the entire current line."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (downcase-region start end)))

;; Window management function
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

;; Common keybinding functions using general.el
(use-package general
  :config
  (general-create-definer my-leader-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Define keybinding maps to be applied
  (setq my-normal-keys-map
        '(;; File operations
          "ff" find-file
          "fs" save-buffer
          ;; Buffers
          "bb" switch-to-buffer
          "bd" kill-buffer
          "bi" ibuffer
          ;; Commands
          ": " execute-extended-command
          ;; Downcase
          "d l" downcase-current-line
          ;; Projectile keybindings
          "p p" projectile-switch-project
          "SPC" projectile-find-file
          "p r" projectile-recentf
          "p b" projectile-switch-to-buffer
          "p c" projectile-compile-project
          "p s" projectile-save-project-buffers
          "p k" projectile-kill-buffers
          "p d" projectile-dired
          "p R" projectile-regenerate-tags
          ;; Gud Debugger
          "d ." projectile-run-gdb
          "d b" gud-break
          "d n" gud-next
          "d s" gud-step
          "d f" gud-finish
          "d c" gud-cont
          "d r" gud-run
          "d u" gud-until
          "d p" gud-print
          "d d" gud-remove
          "d t" gud-tbreak
          "d w" gud-watch
          "d l" gud-refresh
          "d <" gud-up
          "d >" gud-down
          "d q" gud-quit
          "d i" gud-stepi
          "d h" gud-goto-here
          "d v" gud-print
          "d m" gud-display-memory
          ;; LSP
          "l l"  lsp
          "l \"" lsp-workspace-restart
          "l q"  lsp-workspace-shutdown
          ;; LSP Code Actions
          "l a"  lsp-execute-code-action
          "l A"  lsp-organize-imports
          ;; LSP Diagnostics
          "l d d" lsp-ui-flycheck-list
          "l d n" flycheck-next-error
          "l d p" flycheck-previous-error
          "l d f" flycheck-buffer
          ;; LSP Format
          "l f b" lsp-format-buffer
          "l f r" lsp-format-region
          ;; LSP Goto
          "l g d" lsp-find-definition
          "l g r" lsp-find-references
          "l g i" lsp-find-implementation
          "l g t" lsp-find-type-definition
          "l g o" lsp-describe-session
          ;; LSP Help
          "l h d" lsp-describe-thing-at-point
          "l h s" lsp-signature-help
          "l h t" lsp-treemacs-symbols
          ;; LSP Rename
          "l r r" lsp-rename
          ;; LSP Imenu and Outline
          "l i i" lsp-ui-imenu
          "l i o" lsp-treemacs-outline
          ;; LSP Peek
          "l p d" lsp-ui-peek-find-definitions
          "l p r" lsp-ui-peek-find-references
          "l p i" lsp-ui-peek-find-implementation
          ;; LSP Workspace
          "l w a" lsp-workspace-folders-add
          "l w r" lsp-workspace-folders-remove
          "l w l" lsp-workspace-folders-list
          "l w s" lsp-workspace-folders-switch
          ;; LSP Miscellaneous
          "l m s" lsp-restart-workspace
          "l m l" lsp-toggle-trace-io
          "l m t" lsp-clients-debug-info))

  (setq my-visual-keys-map
        '("fr" fill-region
          "; " evilnc-comment-or-uncomment-lines))

  (setq my-general-keys-map
        '(": " execute-extended-command))

  (setq my-window-keys-map
        '(;; Window resize
          "wr" my/resize-window
          ;; Window splits
          "ws" split-window-below
          "wv" split-window-right
          ;; Window navigation
          "wh" evil-window-left
          "wj" evil-window-down
          "wk" evil-window-up
          "wl" evil-window-right
          ;; Window operations
          "wq" evil-quit
          "wd" delete-window
          "wD" kill-buffer-and-window
          ;; Maximize window
          "wmm" delete-other-windows))

  ;; Apply keybindings
  (defun apply-key-map (map-list keymap)
    "Apply key bindings from MAP-LIST to KEYMAP."
    (let ((idx 0))
      (while (< idx (length map-list))
        (let ((key (nth idx map-list))
              (func (nth (1+ idx) map-list)))
          (my-leader-def
            :keymaps keymap
            key func))
        (setq idx (+ idx 2)))))

  ;; Apply the keyboard mappings
  (apply-key-map my-normal-keys-map 'normal)
  (apply-key-map my-visual-keys-map 'visual)
  (apply-key-map my-general-keys-map '(normal visual))
  (apply-key-map my-window-keys-map 'normal)

  ;; Special case for evil normal mode
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map "," nil)
    (evil-define-key 'normal 'global
      "gd" 'xref-find-definitions
      "gD" 'xref-find-definitions-other-window))

  ;; Local leader setup
  (general-create-definer my-local-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix ",")

  ;; Forcing keybindings
  (my-leader-def
    :keymaps '(insert visual)
    "ee" 'evil-force-normal-state))

;; =======================
;; Projectile Configuration
;; =======================

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-sort-order 'recentf))

;; =======================
;; S-Expression Handling
;; =======================

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

;; Add to lisp-like mode hooks
(dolist (hook '(emacs-lisp-mode-hook clojure-mode-hook fennel-mode-hook))
  (add-hook hook 'my-sexp-keybindings))

;; =======================
;; OS Compatibility
;; =======================

(use-package exec-path-from-shell
  :config
  (when (or (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))

(use-package sudo-edit
  :commands (sudo-edit))

;; =======================
;; Performance Tuning
;; =======================

;; Always load newest byte code
(setq load-prefer-newer t
      ;; Reduce garbage collection frequency
      gc-cons-threshold 50000000
      ;; Warning threshold for large files
      large-file-warning-threshold 100000000
      ;; Don't confirm killing processes
      confirm-kill-processes nil)

;; =======================
;; Navigation Enhancements
;; =======================

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package corfu-terminal
  :after corfu
  :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :init
  (unless (display-graphic-p) (corfu-terminal-mode +1)))

;; Emacs default settings
(use-package emacs
  :init
  (setq completion-cycle-threshold 3
        tab-always-indent 'complete)
  (setq-default truncate-lines t))

;; Marginalia for minibuffer annotations
(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

;; Icons in completion
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Vertical completion UI
(use-package vertico
  :demand t
  :straight (vertico :files (:defaults "extensions/*")
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
                                vertico-unobtrusive))
  :general
  (:keymaps '(normal insert visual motion)
            "M-." #'vertico-repeat)
  (:keymaps 'vertico-map
            "<tab>" #'vertico-insert
            "<escape>" #'minibuffer-keyboard-quit
            "?" #'minibuffer-completion-help
            "C-M-n" #'vertico-next-group
            "C-M-p" #'vertico-previous-group
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
            "C-l" #'kb/vertico-multiform-flat-toggle)
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

  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))

  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))

  (add-to-list 'completion-styles-alist
               '(basic-remote
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  :config
  (vertico-mode)
  (vertico-multiform-mode)

  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

;; Embark for contextual actions
(use-package embark
  :demand t
  :general
  (my/leader-keys
    "." 'embark-act)
  ("C-." 'embark-act)
  ("C-;" 'embark-dwim)
  (:keymaps 'vertico-map
            "C-." 'embark-act)
  (:keymaps 'embark-heading-map
            "l" 'org-id-store-link)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Avy for jump navigation
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
    t)

  :general
  (general-def '(normal motion)
    "s" 'evil-avy-goto-char-timer
    "f" 'evil-avy-goto-char-in-line
    "gl" 'evil-avy-goto-line
    ";" 'avy-resume)

  :config
  (setf (alist-get ?. avy-dispatch-alist) 'my/avy-action-embark
        (alist-get ?i avy-dispatch-alist) 'my/avy-action-insert-newline
        (alist-get ?K avy-dispatch-alist) 'my/avy-action-kill-whole-line))

;; Orderless completion style
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic-remote orderless))))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher))
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
    "Literal style dispatcher using the equals sign as a suffix."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    "Leading initialism dispatcher using the comma suffix."
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))

  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    "Flex dispatcher using the tilde suffix."
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1)))))

;; Additional navigation tools
(use-package treemacs
  :config
  (my-leader-def
    :keymaps 'normal
    "op" 'treemacs))

(use-package ripgrep
  :config
  (my-leader-def
    :keymaps 'normal
    "rg" 'ripgrep
    "rp" 'projectile-ripgrep))

(use-package consult
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

;; Fix dired keybindings
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "SPC") nil))

;; =======================
;; UI Enhancements
;; =======================

;; Font size based on OS
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'default nil :height 120))


;; Basic UI settings
(use-package emacs
  :hook
  ((prog-mode text-mode) . display-line-numbers-mode))

;; Disable unnecessary UI elements
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore
      inhibit-startup-screen t)

;; Scrolling settings
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; Mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Miscellaneous UI settings
(fset 'yes-or-no-p 'y-or-n-p)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; =======================
;; Org Mode Configuration
;; =======================

(defun setup-org-mode-keys ()
  (my-local-leader-def
    :keymaps 'org-mode-map
    "o a" 'org-agenda))

(setup-org-mode-keys)

;; =======================
;; Autoloads
;; =======================

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

(use-package meson-mode
  :mode ("meson.build\\'" . meson-mode)
  :config
  (setq meson-mode-indent-level 2))

;; Compilation buffer enhancements
(require 'ansi-color)

(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(defun regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))

(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")

(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '(;; Icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; Non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; No-op sequences
         "\012\033\\[2K\033\\[1F"
         )))

(defun filter-non-sgr-control-sequences-in-region (begin end)
  "Filter out non-SGR control sequences in region from BEGIN to END."
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))

(defun filter-non-sgr-control-sequences-in-output (ignored)
  "Filter non-SGR control sequences in the current comint or compilation output."
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))

(defun clean-and-colorize-compilation-buffer ()
  "Clean and colorize the compilation buffer."
  (endless/colorize-compilation)
  (filter-non-sgr-control-sequences-in-region
   compilation-filter-start (point)))

;; Add hooks to handle compilation output
(add-hook 'compilation-filter-hook
          #'clean-and-colorize-compilation-buffer)
(add-hook 'comint-output-filter-functions
          #'filter-non-sgr-control-sequences-in-output)

(use-package compile
  :straight (:type built-in)
  :hook ((compilation-mode . (lambda ()
                               (setq truncate-lines nil)))
         (compilation-filter . clean-and-colorize-compilation-buffer))
  :config
  (defun compilation-wrap ()
    "Toggle line wrapping or truncation in the current compilation buffer."
    (interactive)
    (if (derived-mode-p 'compilation-mode)
        (progn
          (setq truncate-lines (not truncate-lines))
          (if truncate-lines
              (message "Truncation enabled for compilation buffer.")
            (message "Line wrapping enabled for compilation buffer.")))
      (message "This function works only in compilation-mode buffers.")))

  (setq-default compilation-mode-line-errors nil)
  (setq compilation-window-height 20
        compilation-scroll-output t)
  (define-key compilation-mode-map (kbd "C-c w") 'compilation-wrap)
  (add-hook 'compilation-start-hook
            (lambda (proc)
              (with-current-buffer (process-buffer proc)
                (setq truncate-lines t)))))

;; =======================
;; Utilities
;; =======================

(use-package vterm
  :straight t)

(use-package multi-vterm
  :straight t
  :config
  (my-leader-def
    :keymaps 'normal
    "ot" 'multi-vterm))

(use-package emamux
  :straight t)

(use-package restclient
  :straight t)

;; =======================
;; Debugging Tools
;; =======================

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

(use-package magit
  :straight t
  :config
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
    "gW"  'magit-worktree))

;; Helper git functions
(defun +my-magit/commit-log-for-directory (dir)
  "Open a Magit log buffer filtered for changes in DIR."
  (interactive "sDirectory: ")
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (if default-directory
        (magit-log-head nil (list dir))
      (message "Not inside a Git repository."))))

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

(defun gitui ()
  "Run gitui in a new buffer."
  (interactive)
  (let ((buffer-name "*gitui*"))
    (if (get-buffer buffer-name)
        (kill-buffer buffer-name))
    (if (fboundp 'vterm)
        (progn
          (vterm buffer-name)
          (vterm-send-string "gitui")
          (vterm-send-return))
      (progn
        (ansi-term "/bin/bash" buffer-name)
        (term-send-raw-string "gitui\n")))))

;; =======================
;; File Keybindings
;; =======================

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

;; Define file paths and create functions
(let ((paths-for-keymaps
       '(("learn-elisp" . "~/.config/emacs/learn-emacs-lisp.el")
         ("init" . "~/.config/emacs/init.el")
         ("zshrc" . "~/.zshrc")
         ("gtd" . "~/org/gtd.org")
         ("file-bindings" . "~/.emacs/config/lisp/file-bindings.el")
         ("scripts-dir" . "~/scripts")
         ("xdg-home-dir" . "~/.config"))))
  (create-nav-functions paths-for-keymaps))

;; Define file access keybindings
(defun my-file-keymaps ()
  (my-leader-def
    :keymaps 'normal
    "a l" 'nav-to-learn-elisp
    "a i" 'nav-to-init
    "a g" 'nav-to-gtd
    "a k" 'nav-to-file-keymaps
    "a z" 'nav-to-zshrc))

(my-file-keymaps)

;; =======================
;; Main Personal Config
;; =======================

(setq user-full-name "Brian Witte"
      user-mail-address "brianwitte@mailfence.com")

;; Basic editing preferences
(setq-default indent-tabs-mode nil
              tab-width 8)
(setq require-final-newline t)
(delete-selection-mode t)

;; Backup settings
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Auto-save list configuration
(let ((auto-save-list-dir (expand-file-name "auto-save-list/" my-emacs-local-dir)))
  (unless (file-exists-p auto-save-list-dir)
    (make-directory auto-save-list-dir)))

(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" my-emacs-local-dir))

;; Auto revert and encoding
(global-auto-revert-mode t)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Hippie expand configuration
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Global key bindings
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x p") #'proced)
(global-set-key (kbd "C-x \\") #'align-regexp)
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "s-q") #'fill-paragraph)
(global-set-key (kbd "s-x") #'execute-extended-command)

;; Tab behavior and permissions
(setq tab-always-indent 'complete)
(put 'erase-buffer 'disabled nil)

;; C source directory for debugging Emacs functions
(setq find-function-C-source-directory "~/src/emacs-29.1")

;; Auto-create missing directories when saving files
(defun er-auto-create-missing-dirs ()
  "Make missing parent directories automatically."
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'er-auto-create-missing-dirs)

;; Built-in package configurations
(use-package paren
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

(use-package calendar
  :config
  (setq calendar-week-start-day 1))

(use-package hl-line
  :config
  (global-hl-line-mode +1))

;; State persistence
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" my-emacs-local-dir)
        save-place-mode t)
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" my-emacs-local-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" my-emacs-local-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; =======================
;; Text Editing Configuration
;; =======================

(defun my/pulse-line ()
  "Flash highlight the current line with region face"
  (interactive)
  (pulse-momentary-highlight-one-line (point) 'region))

;; Reformatter for code formatting
(use-package reformatter
  :straight t)

;; =======================
;; Configuration Formats
;; =======================

(use-package jq-mode
  :straight t
  :mode "\\.jq\\'"
  :commands jq-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.jq\\'" . jq-mode))
  :config
  (with-eval-after-load "json-mode"
    (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively))
  (setq jq-interactive-command "jq"
        jq-interactive-default-options ""))

;; =======================
;; Elisp Configuration
;; =======================

(defun eval-last-sexp-and-show-temporarily ()
  "Evaluate the sexp before the point and momentarily display the result."
  (interactive)
  (let* ((result (eval-last-sexp nil))
         (result-string (format "\n;=> %S" result))
         (display-pos (1+ (point))))
    (momentary-string-display result-string display-pos)))

(defun setup-elisp-mode-keys ()
  (my-local-leader-def
    :keymaps 'emacs-lisp-mode-map
    "m"  'macrostep-expand
    "eb" 'eval-buffer
    "ed" 'eval-defun
    "ee" 'eval-last-sexp-and-show-temporarily
    "er" 'eval-region
    "el" 'load-library
    "gf" 'find-function
    "gv" 'find-variable
    "gl" 'find-library))

(add-hook 'emacs-lisp-mode-hook 'setup-elisp-mode-keys)

;; =======================
;; Programming Language Support
;; =======================

;; Common code formatting using reformatter
(defmacro define-code-formatters ()
  "Define formatters for multiple languages using reformatter."
  '(progn
     ;; Shell formatting
     (reformatter-define shfmt
       :program "shfmt"
       :args '("-i" "4" "-ci" "-bn" "-fn" "-sr" "-kp")
       :lighter " shfmt")

     ;; C/C++ formatting
     (reformatter-define kstyle
       :program "uncrustify"
       :args `("-c" ,(expand-file-name "~/src/uncrustify/etc/linux.cfg") "-l" "C")
       :lighter " kstyle")

     (reformatter-define allman
       :program "uncrustify"
       :args `("-c" ,(expand-file-name "~/src/uncrustify/etc/allman.cfg") "-l" "C")
       :lighter " allman")

     ;; Clojure formatting
     (reformatter-define zprint
       :program "zprint-clj"
       :lighter " ZprintCLJ")

     ;; Common Lisp formatting
     (reformatter-define cl-format
       :program "cl-pretty-format"
       :lighter " CLFmt")

     ;; Ruby formatting
     (reformatter-define rubyfmt
       :program "rubyfmt"
       :lighter " Rubyfmt")))

(define-code-formatters)

;; =======================
;; Reusable Language Configuration Maps
;; =======================

;; Define reusable configuration maps for language modes
;; This enables consistent keybindings across different languages

;; Common evaluation commands
(defvar my-eval-cmd-map
  '(
    "e b" eval-buffer      ;; Eval buffer
    "e d" eval-defun       ;; Eval defun/function
    "e e" eval-last-sexp   ;; Eval expression
    "e r" eval-region      ;; Eval region
    ))

;; Common REPL commands
(defvar my-repl-cmd-map
  '(
    "r b" switch-to-repl   ;; Switch to REPL buffer
    "r r" reload-code      ;; Reload/refresh code
    "r c" clear-repl       ;; Clear REPL output
    "r q" quit-repl        ;; Quit REPL
    ))

;; Common navigation commands
(defvar my-goto-cmd-map
  '(
    "g g" goto-definition  ;; Go to definition
    "g b" jump-back        ;; Jump back from definition
    ))

;; Common documentation/help commands
(defvar my-doc-cmd-map
  '(
    "h d" show-doc         ;; Show documentation
    "h a" apropos          ;; Search for symbols
    ))

;; Common debugging commands
(defvar my-debug-cmd-map
  '(
    "d d" debug-defun      ;; Debug current function
    ))

;; Common test commands
(defvar my-test-cmd-map
  '(
    "t t" run-test         ;; Run test at point
    "t a" run-all-tests    ;; Run all tests
    "t r" run-failed-tests ;; Run failed tests
    ))

;; Common format commands
(defvar my-format-cmd-map
  '(
    "f b" format-buffer    ;; Format buffer
    "f r" format-region    ;; Format region
    ))

;; Helper function to apply config maps to specific modes
(defun apply-mode-maps (mode-map cmd-map cmd-funcs)
  "Apply command map CMD-MAP to MODE-MAP, mapping to functions in CMD-FUNCS."
  (let ((idx 0))
    (while (< idx (length cmd-map))
      (let* ((key (nth idx cmd-map))
             (cmd-key (nth (1+ idx) cmd-map))
             (func (plist-get cmd-funcs (intern cmd-key))))
        (when func
          (my-local-leader-def
            :states 'normal
            :keymaps mode-map
            key func)))
      (setq idx (+ idx 2)))))

;; Helper for setting up mode-specific key bindings with a plist
(defun setup-mode-keybindings (mode-map bindings)
  "Define local leader keybindings for a specific MODE-MAP.
BINDINGS is a plist of (key . function) pairs."
  (let ((idx 0))
    (while (< idx (length bindings))
      (let ((key (nth idx bindings))
            (func (nth (1+ idx) bindings)))
        (my-local-leader-def
          :states 'normal
          :keymaps mode-map
          key func))
      (setq idx (+ idx 2)))))

;; =======================
;; Language-Specific Configurations
;; =======================

;; Clojure
(use-package cider
  :straight t
  :config
  (defun my-clojure-mode-cider-keybindings ()
    ;; Define the function mappings for clojure
    (let ((clojure-funcs
           '(eval-buffer cider-eval-buffer
             eval-defun cider-eval-defun-at-point
             eval-last-sexp cider-eval-last-sexp
             eval-region cider-eval-region
             switch-to-repl cider-switch-to-repl-buffer
             reload-code cider-ns-refresh
             clear-repl cider-find-and-clear-repl-output
             quit-repl cider-quit
             goto-definition cider-find-var
             jump-back cider-pop-back
             show-doc cider-doc
             apropos cider-apropos
             debug-defun cider-debug-defun-at-point
             run-test cider-test-run-test
             run-all-tests cider-test-run-ns-tests
             run-failed-tests cider-test-rerun-failed-tests
             format-buffer cider-format-buffer)))

      ;; Apply common keybinding patterns
      (dolist (map-pair '((my-eval-cmd-map . eval)
                          (my-repl-cmd-map . repl)
                          (my-goto-cmd-map . goto)
                          (my-doc-cmd-map . doc)
                          (my-debug-cmd-map . debug)
                          (my-test-cmd-map . test)
                          (my-format-cmd-map . format)))
        (apply-mode-maps '(clojure-mode-map clojurescript-mode-map clojurec-mode-map)
                        (symbol-value (car map-pair))
                        clojure-funcs))

      ;; Add Clojure-specific bindings
      (setup-mode-keybindings
       '(clojure-mode-map clojurescript-mode-map clojurec-mode-map)
       (list
        " '"  #'cider-jack-in-clj
        " \"" #'cider-jack-in-cljs
        " c"  #'cider-connect-clj
        " C"  #'cider-connect-cljs
        " m"  #'cider-macroexpand-1
        " M"  #'cider-macroexpand-all
        " e D" #'cider-insert-defun-in-repl
        " e E" #'cider-insert-last-sexp-in-repl
        " e R" #'cider-insert-region-in-repl
        " e u" #'cider-undef
        " g n" #'cider-find-ns
        " h c" #'cider-clojuredocs
        " h j" #'cider-javadoc
        " h w" #'cider-clojuredocs-web
        " i e" #'cider-enlighten-mode
        " i i" #'cider-inspect
        " i r" #'cider-inspect-last-result
        " n n" #'cider-browse-ns
        " n N" #'cider-browse-ns-all
        " n r" #'cider-ns-refresh
        " p p" #'cider-pprint-eval-last-sexp
        " p P" #'cider-pprint-eval-last-sexp-to-comment
        " p d" #'cider-pprint-eval-defun-at-point
        " p D" #'cider-pprint-eval-defun-to-comment
        " p r" #'cider-pprint-eval-last-sexp-to-repl
        " r n" #'cider-repl-set-ns
        " r R" #'cider-restart
        " r B" #'+clojure/cider-switch-to-repl-buffer-and-switch-ns
        " r f" #'cider-load-file
        " r l" #'cider-load-buffer
        " r L" #'cider-load-buffer-and-switch-to-repl-buffer
        " t l" #'cider-test-run-loaded-tests
        " t n" #'cider-test-run-ns-tests
        " t p" #'cider-test-run-project-tests
        " t s" #'cider-test-run-ns-tests-with-filters))))

  ;; Add hooks for all Clojure-related modes
  (dolist (mode '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook))
    (add-hook mode 'my-clojure-mode-cider-keybindings)))

;; Common Lisp (SLY)
(use-package sly
  :straight t
  :config
  (defun my-common-lisp-mode-sly-keybindings ()
    ;; Define the function mappings for Common Lisp
    (let ((cl-funcs
           '(eval-buffer sly-eval-buffer
             eval-defun sly-eval-defun
             eval-last-sexp sly-eval-last-expression
             eval-region sly-eval-region
             switch-to-repl sly-switch-to-output-buffer
             reload-code sly-reload-system
             clear-repl sly-clear-repl
             quit-repl sly-quit
             goto-definition sly-edit-definition
             jump-back sly-pop-find-definition-stack
             show-doc sly-describe-symbol
             apropos sly-apropos
             debug-defun sly-db-inspect-condition)))

      ;; Apply common keybinding patterns
      (dolist (map-pair '((my-eval-cmd-map . eval)
                          (my-repl-cmd-map . repl)
                          (my-goto-cmd-map . goto)
                          (my-doc-cmd-map . doc)
                          (my-debug-cmd-map . debug)))
        (apply-mode-maps 'lisp-mode-map
                        (symbol-value (car map-pair))
                        cl-funcs))

      ;; Add Common Lisp-specific bindings
      (setup-mode-keybindings
       'lisp-mode-map
       (list
        " '"  #'sly
        " \"" #'sly-connect
        " m"  #'sly-macroexpand-1
        " M"  #'sly-macroexpand-all
        " e D" #'sly-eval-defun-to-string
        " e E" #'sly-eval-last-expression-to-string
        " g n" #'sly-browse-symbol
        " h j" #'sly-jump-to-documentation
        " i i" #'sly-inspect
        " i r" #'sly-inspect-last-expression
        " n n" #'sly-list-all-packages
        " p p" #'sly-eval-print-last-expression
        " p d" #'sly-eval-defun-and-show
        " r r" #'sly-restart-inferior-lisp
        " r l" #'sly-load-file))))

  (add-hook 'lisp-mode-hook 'my-common-lisp-mode-sly-keybindings))

;; Fennel
(use-package fennel-mode
  :straight t
  :config
  (defun my-fennel-mode-keybindings ()
    ;; Define the function mappings for Fennel
    (let ((fennel-funcs
           '(eval-last-sexp fennel-eval-last-sexp
             eval-region fennel-eval-region
             eval-toplevel fennel-eval-toplevel-form
             reload-code fennel-reload
             show-doc fennel-show-documentation)))

      ;; Apply common evaluation mappings
      (apply-mode-maps 'fennel-mode-map
                      my-eval-cmd-map
                      fennel-funcs)

      ;; Add Fennel-specific bindings
      (setup-mode-keybindings
       'fennel-mode-map
       (list
        " '"  #'fennel-repl
        " m"  #'fennel-macroexpand
        " e f" #'fennel-eval-toplevel-form
        " e n" #'fennel-eval-form-and-next
        " h v" #'fennel-show-variable-documentation
        " r r" #'fennel-reload
        " n r" #'fennel-reload))))

  (defun my-fennel-evil-keybindings ()
    (evil-define-key 'normal fennel-mode-map
      "gd" 'fennel-find-definition))

  (add-hook 'fennel-mode-hook 'my-fennel-mode-keybindings)
  (add-hook 'fennel-mode-hook 'my-fennel-evil-keybindings))

;; Lua
(setq lsp-clients-lua-language-server-install-dir
      "/home/bkz/lang-servers/lua-language-server/")

(use-package lua-mode
  :straight t
  :init
  (setq lua-indent-level 4)
  :config
  (add-hook 'lua-mode-local-vars-hook #'lsp 'append)
  (add-hook 'lua-mode-local-vars-hook #'tree-sitter 'append))

;; Go
(use-package go-mode
  :straight t
  :hook ((go-mode . lsp-deferred)
         (go-mode . lsp-go-install-save-hooks))
  :config
  (defun lsp-go-install-save-hooks ()
    "Set up before-save hooks for Go to format buffer and organize imports."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (defun my-go-mode-keybindings ()
    (let ((go-funcs
           '(format-buffer lsp-format-buffer
             goto-definition lsp-find-definition)))

      ;; Apply common mappings
      (dolist (map-pair '((my-format-cmd-map . format)
                          (my-goto-cmd-map . goto)))
        (apply-mode-maps 'go-mode-map
                        (symbol-value (car map-pair))
                        go-funcs))))

  (add-hook 'go-mode-hook 'my-go-mode-keybindings))

(use-package go-eldoc
  :straight t
  :hook (go-mode . go-eldoc-setup)
  :config
  (setq go-eldoc-gocode "gopls"))

;; Poke
(use-package poke-mode
  :straight t)

;; Ruby
(use-package inf-ruby
  :straight t
  :config
  (defun inf-ruby-start-pry ()
    "Run an inferior Ruby process with Pry in a buffer."
    (interactive)
    (let* ((impl "pry")
           (command (cdr (assoc impl inf-ruby-implementations))))
      (run-ruby command impl)))

  (defun my-ruby-mode-keybindings ()
    (let ((ruby-funcs
           '(switch-to-repl inf-ruby-switch-to-repl
             eval-buffer ruby-send-buffer
             eval-region ruby-send-region)))

      ;; Apply common mappings
      (dolist (map-pair '((my-repl-cmd-map . repl)
                          (my-eval-cmd-map . eval)))
        (apply-mode-maps 'ruby-mode-map
                        (symbol-value (car map-pair))
                        ruby-funcs))))

  (add-hook 'ruby-mode-hook 'my-ruby-mode-keybindings))

(use-package rspec-mode
  :straight t
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'rspec-mode)
  :config
  (setq rspec-use-rake-when-possible nil)

  (defun my-rspec-mode-keybindings ()
    (let ((rspec-funcs
           '(run-test rspec-verify
             run-all-tests rspec-verify-all
             run-failed-tests rspec-rerun)))

      ;; Apply test mappings
      (apply-mode-maps 'ruby-mode-map
                      my-test-cmd-map
                      rspec-funcs)))

  (add-hook 'ruby-mode-hook 'my-rspec-mode-keybindings))

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

;; Zig
(use-package zig-mode
  :straight t
  :config
  (defun my-zig-mode-keybindings ()
    (let ((zig-funcs
           '(format-buffer zig-format-buffer
             run-test zig-test-buffer)))

      ;; Apply common mappings
      (dolist (map-pair '((my-format-cmd-map . format)
                          (my-test-cmd-map . test)))
        (apply-mode-maps 'zig-mode-map
                        (symbol-value (car map-pair))
                        zig-funcs))

      ;; Add Zig-specific bindings
      (setup-mode-keybindings
       'zig-mode-map
       (list
        " e c" #'zig-compile
        " e r" #'zig-run))))

  (add-hook 'zig-mode-hook 'my-zig-mode-keybindings))

;; Flycheck for syntax checking
(use-package flycheck
  :straight t
  :hook ((java-mode . flycheck-mode)
         (c-mode . flycheck-mode)
         (ruby-mode . flycheck-mode))
  :config
  (setq-default flycheck-checkers '(ruby-standard c-gcc go-staticcheck))
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; Final message
(message "Evaluated %s" (or (buffer-file-name) "unknown file"))
