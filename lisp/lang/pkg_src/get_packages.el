(defvar elisp-package-urls
  '(("jsonnet-mode" . "https://raw.githubusercontent.com/tminor/jsonnet-mode/main/jsonnet-mode.el"))
  "A list of Emacs Lisp package names and their URLs.")

(defun download-elisp-package-to-current-dir (package-name)
  "Download an Emacs Lisp package given its PACKAGE-NAME to the directory of the current buffer's file."
  (interactive
   (list (completing-read "Package name: " (mapcar 'car elisp-package-urls))))
  (let* ((url (cdr (assoc package-name elisp-package-urls)))
         (buffer (when url (url-retrieve-synchronously url)))
         (filename (concat (file-name-directory (buffer-file-name)) package-name ".el")))
    (if (not url)
        (message "Package URL not found for %s" package-name)
      (if buffer
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "\n\n")
            (write-region (point) (point-max) filename)
            (kill-buffer)
            (message "Downloaded %s to %s" package-name filename))
        (error "Failed to download %s" package-name)))))
