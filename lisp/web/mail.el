;;; -*- lexical-binding: t -*-

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


(provide 'emx-mail)
