;;; format-buffer.el --- Format buffer

;;; Code:

(provide 'format-buffer)

(defun format-buffer--indent-buffer ()
  "Indent whole buffer with `indent-region' command."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defvar format-buffer-fn 'format-buffer--indent-buffer)

(defun format-buffer()
  "Format whole buffer with `format-buffer-fn' function."
  (interactive)
  (call-interactively format-buffer-fn))

;;; format-buffer.el ends here
