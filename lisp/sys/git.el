;;; -*- lexical-binding: t -*-
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

(provide 'emx-git)
