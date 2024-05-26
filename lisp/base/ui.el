;;; -*- lexical-binding: t -*-
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

(provide 'emx-ui)
