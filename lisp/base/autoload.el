;;; -*- lexical-binding: t -*-
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

(provide 'emx-base-autoload)
