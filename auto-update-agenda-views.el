(defun smishy--run-agenda-cmd (f)
  "Run commands \"f\" in Agenda buffer. You can get these
commands using \"smishy--get-org-agenda-view-commands\"."
  (save-window-excursion
    (let* ((line (org-current-line)))
      (if f (eval f))
      (org-goto-line line))))

(defun smishy--get-org-agenda-view-commands ()
  "Get commands by which the current state of Agenda buffer can
be restored using \"(eval commands)\"."
  (let* ((p (or (and (looking-at "\\'") (1- (point))) (point)))
         (series-redo-cmd (get-text-property p 'org-series-redo-cmd)))
    (if series-redo-cmd
      (get-text-property p 'org-series-redo-cmd)
      (get-text-property p 'org-redo-cmd))))

(defun smishy--auto-update-agenda ()
  (interactive)
  (dolist (buf (smishy--get-agenda-buffers))
    (with-current-buffer buf
      (smishy--run-agenda-cmd (smishy--get-org-agenda-view-commands)))))

(defun smishy--get-agenda-buffers ()
  (let (blist)
    (dolist (buf (buffer-list))
      (when (with-current-buffer buf (eq major-mode 'org-agenda-mode))
        (push buf blist)))
    blist))

(defun smishy--tab-out-of-agenda (&optional highlight)
  "Tab out of agenda view easily.
Used to tab out of the agenda view when the marker is not on a todo, which org-mode usually responds to by throwing an error."
  (interactive)
  (if (org-get-at-bol 'org-marker) ; true if org-marker
    (org-agenda-goto highlight) ; continue as normal
    (smishy--tab-out-of-agenda-list))) ; open list

(defun smishy--tab-out-of-agenda-list ()
  "Create popup list to tab out of agenda view.
Usually called by smishy--tab-out-of-agenda, this function either creates a list of org files that org-agenda knows about, or tabs out to it if only 1 is known."
  (let* ((file-path (cond ((equal 1 (length org-agenda-files))
                           (car org-agenda-files))
                          ((< 1 (length org-agenda-files))
                           (popup-menu* org-agenda-files))))
         (buffer (get-file-buffer file-path)))
    (if buffer
      (switch-to-buffer-other-window buffer)
      (find-file-other-window file-path))))


(defun smishy--bulk-update-agenda ()
  (run-with-idle-timer 0.1 t (lambda ()
                               (unless (eq major-mode 'org-agenda-mode)
                                 (smishy--auto-update-agenda)))))


(defun smishy--auto-update-agenda-views-start ()
  (smishy--bulk-update-agenda))
