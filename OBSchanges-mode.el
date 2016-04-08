(defun changelog-format-entry ()
  (interactive)
  (beginning-of-line)
  (delete-whitespace-rectangle (point) (line-end-position) nil)
  (if (looking-at "[-+*] ")
      (delete-char 2))
  (replace-string "``" "\"" nil (point) (line-end-position))
  (beginning-of-line)
  (insert "  * ")
  (move-end-of-line nil)
  (fill-region (line-beginning-position) (line-end-position))
  (forward-line 1))

(defun changelog-format-entry2 ()
  (interactive)
  (beginning-of-line)
  (delete-whitespace-rectangle (point) (line-end-position) nil)
  (if (looking-at "[-+*] ")
      (delete-char 2))
  (replace-string "``" "\"" nil (point) (line-end-position))
  (beginning-of-line)
  (insert "    + ")
  (move-end-of-line nil)
  (fill-region (line-beginning-position) (line-end-position))
  (forward-line 1))

(defun changelog-insert-version ()
  (interactive)
  (beginning-of-line)
  (delete-whitespace-rectangle (point) (line-end-position) nil)
  (insert "- changes from version ")
  (end-of-line)
  (insert ":")
  (forward-line 1))


;;;###autoload
(define-minor-mode OBSchanges-mode
  "Minor mode to make it easier to edit changes files for OBS. Especially to cut&paste from Changelog files and webpages."
  :lighter " OBSchanges"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<f5>") 'changelog-format-entry)
	    (define-key map (kbd "<f6>") 'changelog-format-entry2)
	    (define-key map (kbd "<f7>") 'changelog-insert-version)
	    map)
  (make-variable-buffer-local 'adaptive-fill-regexp)
  ; add + to adaptive fill-prefix
  (setq adaptive-fill-regexp "[ \t]*\\([-–!|#%;>*+·•‣⁃◦]+[ \t]*\\)*"))

(provide 'OBSchanges-mode)
