;;; -*- lexical-binding: t -*-
;;;
;;; base/nav.el --- Description

(use-package vertico
  :straight t
  :init
  (vertico-mode 1)  ; enable Vertico in all buffers
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico--prompt)
  (setq vertico-cycle t)  ; enable cycling of candidates

  (defun my/vertico-backward-updir ()
    "Delete directory before point, accounting for consecutive slashes."
    (interactive)
    (if (looking-back "/" 1)
        (progn
          (call-interactively #'backward-delete-char)
          (when (search-backward "/" (minibuffer-prompt-end) t)
            (delete-region (1+ (point)) (point-max))))
      (if (search-backward "/" (minibuffer-prompt-end) t)
          (delete-region (1+ (point)) (point-max))
        (call-interactively #'backward-delete-char)))
    (end-of-line))

  (define-key vertico-map (kbd "<backspace>") #'my/vertico-backward-updir))

(define-key evil-insert-state-map (kbd "TAB") 'completion-at-point)

(use-package swiper
  :straight t
  :config
  (my-leader-def
    :keymaps 'normal
    "ss" 'swiper
    )
  )

(use-package ripgrep
  :straight t
  :config
  (my-leader-def
    :keymaps 'normal
    "rg" 'ripgrep-regexp
    )
  )

;; dired stuff
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "SPC") nil))

(provide 'emx-nav)
