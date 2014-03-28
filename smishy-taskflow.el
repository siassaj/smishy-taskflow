;;;;;;;;;;;;;;;;;;;;;;;; Make sure to have dependencies ;;;;;;;;;;;;;;;;;;;;;;;;

(require 'screen)
(require 'org)
(require 'popup)

(load-library "configure")
(load-library "auto-update-agenda-views")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; public functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smishy-toggle-done ()
  "Toggle ACTION/DONE on current line.
Finish task on the current line and save it at the bottom as 'DONE'"
  (interactive)
  (setq mystr (buffer-substring (point-at-bol) (point-at-eol)))
  (cond ((string-match "^\** ACTION " mystr) (org-todo "DONE"))
        ((string-match "^\** DONE " mystr) (org-todo "ACTION"))))

(defun smishy-create-project ()
  "Create an Org-Mode project.
Creates an Org-Mode project, then allows you to insert a ACTION"
  (interactive)
  (if (equal (line-number-at-pos) smishy--work-line)
    (goto-line (+ smishy--work-line 1)))
  (org-insert-todo-heading nil)
  (org-todo "PROJECT"))

(defun smishy-start-taskflow (file-path)
  "Start the smishy task flow"
  (interactive)
  (setq inhibit-splash-screen t)
  (let ((buff (find-file-noselect file-path)))
    (pop-to-buffer buff)
    (add-to-list 'org-agenda-files (buffer-file-name buff)))
  (smishy--set-variables)
  (smishy--set-faces)
  (smishy--set-capture-templates)
  (smishy--set-key-bindings)
  ;; (smishy--auto-update-agenda-views-start)
  (delete-other-windows)
  (org-mode)
  (org-agenda nil "h"))

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
