;;;;;;;;;;;;;;;;;;;;;;;; Make sure to have dependencies ;;;;;;;;;;;;;;;;;;;;;;;;

(require 'screen)
(require 'org)
(require 'popup)

(load-library "configure")
(load-library "auto-update-agenda-views")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; public functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smishy-reload-top ()
  "Prepare new Next Action line.
This function will jump to line smishy--work-line, newline it and then put a * NEXT ACTION line in and get ready for input."
  (interactive)
  ;; (switch-to-buffer "biglist.org")
  (goto-line smishy--work-line)
  (setq mystr (buffer-substring (point-at-bol) (point-at-eol)))
  (cond ((string= mystr "* NEXT ACTION ") (move-end-of-line 1))
        ((string= mystr "* NEXT ACTION")
         (move-beginning-of-line 1)
         (kill-line)
         (insert "* NEXT ACTION "))
        ((string= mystr "")
         (move-beginning-of-line 1)
         (kill-line)
         (newline)
         (goto-line smishy--work-line)
         (insert "* NEXT ACTION "))
        ((string-match "^ +$" mystr) ;tests for 1 or more blank spaces only
         (move-beginning-of-line 1)
         (kill-line)
         (newline)
         (goto-line smishy--work-line)
         (insert "* NEXT ACTION "))
        (t
         (move-beginning-of-line 1)
         (newline)
         (goto-line smishy--work-line)
         (insert "* NEXT ACTION ")))
  (goto-line (+ 1 smishy--work-line))
  (if (string-match "\* NEXT ACTION ." mystr)
      (org-todo "TODO"))
  (setq mystr2 (buffer-substring (point-at-bol) (point-at-eol)))
  (goto-line (+ 2 smishy--work-line))
  ;; This bit processes the 2nd line under the work line and turns it into a
  ;; TODO which doesn't really seem to be a good thing
  ;; (if (and (string-match "\* TODO " mystr2)
  ;;          (string-match "\* NEXT ACTION ." mystr))
  ;;     (org-todo "TODO"))
  (goto-line smishy--work-line)
  (move-end-of-line 1)
  (save-buffer))

(defun smishy-save-n-go ()
  "Save new Next Action and clone frame.
Push the current task down, add a new DOING, then save the whole file. Finally, detach the screen, thus killing the xterm."
  (interactive)
  (smishy-reload-top)
  (save-buffer)
  (shell-command "screen -D smishy--taskflow"))

(defun smishy-toggle-done ()
  "Toggle TODO/DONE on current line.
Finish task on the current line and save it at the bottom as 'DONE'"
  (interactive)
  (setq mystr (buffer-substring (point-at-bol) (point-at-eol)))
  (cond ((string-match "^\** TODO " mystr) (org-todo "DONE"))
        ((string-match "^\** DONE " mystr) (org-todo "TODO"))))

(defun smishy-create-project ()
  "Create an Org-Mode project.
Creates an Org-Mode project, then allows you to insert a TODO"
  (interactive)
  (if (equal (line-number-at-pos) smishy--work-line)
    (goto-line (+ smishy--work-line 1)))
  (org-insert-todo-heading nil)
  (org-todo "PROJECT"))

(defun smishy-insert-heading-todo ()
  "Insert TODO heading"
  (interactive)
  (smishy--insert-heading "TODO"))

(defun smishy-insert-heading-project ()
  "Insert PROJECT heading"
  (interactive)
  (smishy--insert-heading "PROJECT"))

(defun smishy-insert-heading-ref ()
  "Insert REF heading"
  (interactive)
  (smishy--insert-heading "REF"))

(defun smishy-insert-heading-note ()
  "Insert NOTE heading"
  (interactive)
  (smishy--insert-heading "NOTE"))

(defun smishy-insert-new-heading-todo ()
  "Insert TODO new-heading"
  (interactive)
  (smishy--insert-new-heading "TODO"))

(defun smishy-insert-new-heading-project ()
  "Insert PROJECT new-heading"
  (interactive)
  (smishy--insert-new-heading "PROJECT"))

(defun smishy-insert-new-heading-ref ()
  "Insert REF new-heading"
  (interactive)
  (smishy--insert-new-heading "REF"))

(defun smishy-insert-new-heading-note ()
  "Insert NOTE new-heading"
  (interactive)
  (smishy--insert-new-heading "NOTE"))

(defun smishy-insert-subheading-todo ()
  "Insert TODO subheading"
  (interactive)
  (smishy--insert-subheading "TODO"))

(defun smishy-insert-subheading-project ()
  "Insert PROJECT subheading"
  (interactive)
  (smishy--insert-subheading "PROJECT"))

(defun smishy-insert-subheading-ref ()
  "Insert REF subheading"
  (interactive)
  (smishy--insert-subheading "REF"))

(defun smishy-insert-subheading-note ()
  "Insert NOTE subheading"
  (interactive)
  (smishy--insert-subheading "NOTE"))

(defun smishy--insert-heading (val)
  "Insert val heading
Creates a val heading in the current line"
  ;; (move-end-of-line nil)
  ;; (newline)
  (org-insert-todo-heading nil)
  (org-todo val))

(defun smishy--insert-new-heading (val)
  "Insert new val heading
Creates a new val heading in the current line"
  ;; (move-end-of-line nil)
  ;; (newline)
  (goto-line (+ 1 smishy--work-line))
  (org-insert-todo-heading nil)
  (org-todo val))

(defun smishy--insert-subheading (val)
  "Insert val subheading
Creates a val subheading in the current line"
  ;; (move-end-of-line nil)
  ;; (newline)
  (org-insert-todo-subheading nil)
  (org-todo val))


(defun smishy-start-taskflow (file-path)
  "Start the smishy task flow"
  (interactive)
  (setq inhibit-splash-screen t)
  (let ((buff (find-file-noselect file-path)))
    (pop-to-buffer buff)
    (add-to-list 'org-agenda-files (buffer-file-name buff)))
  (smishy--set-variables)
  (smishy--set-faces)
  (smishy--set-key-bindings)
  ;; (smishy--auto-update-agenda-views-start)
  (delete-other-windows)
  (smishy-reload-top)
  (org-mode)
  (org-agenda nil "h"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test stuff, Ignore! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smishy--lock-spaces-down ()
  "This function makes the entire document read only, except for a space on
the work line. It is problematic as I still cant apply functions properly..."
  (interactive)
  ;; find the point that indicates the end of the 1st read only region
  (goto-line smishy--work-line)
  (setq mystr (buffer-substring (point-at-bol) (point-at-eol)))
  (if (string-match "\* [A-Z]+ " mystr)
      (progn
        (setq readonly-region1-end (- (+ (point) (match-end 0)) 1)))
    (setq readonly-region1-end nil))
  ;; find the point that indicates the start of the 2nd read only region
  (goto-line (+ smishy--work-line 1))
  (setq readonly-region2-start (- (point) 1))
  ;; set read only regions
  (if readonly-region1-end
      (add-text-properties (point-min) readonly-region1-end '(read-only t)))
  (add-text-properties readonly-region2-start (point-max) '(read-only t)))

(defun smishy-reload-tasks ()
  "Function to reload .smishy--taskflow, used during devel"
  (interactive)
  (load-file "~/Code/smishy--taskflow/smishy--taskflow.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fancy colors, Ignore! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; #458b00
;; #66cd00
;; #76ee00
;; #7fff00
;; #008b00
;; #00cd00
;; #00ee00
;; #00ff00
;; #008b45
;; #00cd66
;; #00ee76
;; #00ff7f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Finished ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'smishy-taskflow)


;; (setq org-agenda-start-with-clockreport-mode nil)
