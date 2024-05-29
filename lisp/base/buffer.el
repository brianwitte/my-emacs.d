;;; -*- lexical-binding: t -*-
;;;

(defun my/delete-current-file ()
  "Delete the file associated with the current buffer and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))
      (message "Buffer is not visiting a file"))))

(provide 'emx-buffer)
