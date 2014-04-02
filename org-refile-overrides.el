(defun org-refile-new-child (parent-target child)
  "Use refile target PARENT-TARGET to add new CHILD below it."
  (unless parent-target
    (error "Cannot find parent for new node"))
  (let ((file (nth 1 parent-target))
        (pos (nth 3 parent-target))
        level)
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (save-excursion
        (save-restriction
          (widen)
          (if pos
              (goto-char pos)
            (goto-char (point-max))
            (if (not (bolp)) (newline)))
          (when (looking-at org-outline-regexp)
            (setq level (funcall outline-level))
            (org-end-of-subtree t t))
          (org-back-over-empty-lines)
          (newline)
          (insert "\n" (make-string
                        (if pos (org-get-valid-level level 1) 1) ?*)
                  " " child "\n")
          (org-todo "PROJECT")
          (beginning-of-line 0)
          (list (concat (car parent-target) "/" child) file "" (point)))))))
