(defun changelog-format-entry (itemstring)
  "Format a changelog entry as a level 1 item starting with itemstring.
  Also: replace `` quotes with a normal quote, delete blank lines after entry,
        add single blank line at end of section"
  (interactive)
  (beginning-of-line)
  (while (looking-at "[[:blank:]]*\n")
      (kill-line))
  (delete-whitespace-rectangle (point) (line-end-position) nil)
  (if (looking-at "[-+*] ")
      (delete-char 2))
  (if (looking-at "[[:alnum:]]\\{40\\}[[:blank:]]*")
      (delete-char 41))
  (replace-string "``" "\"" nil (point) (line-end-position))
  (beginning-of-line)
  (insert itemstring)
  (move-end-of-line nil)
  (fill-region (line-beginning-position) (line-end-position))
  (forward-line 1)
  (if (looking-at "[[:blank:]]*\n")
      (delete-blank-lines))
  (if (looking-at "- changes from version ")
      (insert "\n"))
  (if (looking-at "-------")
      (insert "\n"))
  (recenter))

(defun changelog-format-level1 ()
  (interactive)
  (changelog-format-entry "  * ")
  (recenter))

(defun changelog-format-level2 ()
  (interactive)
  (changelog-format-entry "    + ")
  (recenter))

(defun changelog-insert-version ()
  (interactive)
  (save-excursion
    (forward-line -1)
    (move-end-of-line nil)
    (insert "\n")
    (forward-line -1)
    (if (looking-at "[[:blank:]]*\n")
	(delete-blank-lines)))
  (beginning-of-line)
  (delete-whitespace-rectangle (point) (line-end-position) nil)
  ; handle lines like v10.0.1 automatically
  (if (looking-at "v[[:digit:]]*")
      (delete-char 1))
  (insert "- changes from version ")
  (end-of-line)
  (insert ":")
  (forward-line 1)
  (if (looking-at "[[:blank:]]*\n")
      (delete-blank-lines)))


;;;###autoload
(define-minor-mode OBSchanges-mode
  "Minor mode to make it easier to edit changes files for OBS. Especially to cut&paste from Changelog files and webpages."
  :lighter " OBSchanges"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<f5>") 'changelog-format-level1)
	    (define-key map (kbd "<f6>") 'changelog-format-level2)
	    (define-key map (kbd "<f7>") 'changelog-insert-version)
	    map)
  (make-variable-buffer-local 'adaptive-fill-regexp)
  ; add + to adaptive fill-prefix
  (setq adaptive-fill-regexp "[ \t]*\\([-–!|#%;>*+·•‣⁃◦]+[ \t]*\\)*"))

(provide 'OBSchanges-mode)
