(defun smishy--set-variables ()
  "Set important smishy--taskflow variables."
  (setq smishy--work-line 9) ;set what line you will be entering your task
  (setq org-agenda-start-with-clockreport-mode t)
  (setq org-agenda-custom-commands
        '(("h" "2 week agenda, DONE and TODOs"
           ((agenda "" ((org-agenda-start-on-weekday nil)))
            (todo "DONE")
            (todo "DELETED")
            (todo "TODO")
            (stuck ""))
           ((org-agenda-compact-blocks nil)))))
  (setq org-agenda-start-with-follow-mode t)
  (setq org-lowest-priority 69)
  (setq org-default-priority 68)
  (setq org-stuck-projects '("/PROJECT" ("TODO") nil ""))
  ;; (setq org-stuck-projects '("{^.*}/PROJECT" ("TODO" "DOING") nil ""))
  (setq org-startup-indented t)
  (setq org-startup-folded 'content))

(defun smishy--set-faces ()
  "Set smishy--taskflow faces."
  (setq org-todo-keywords '((type "NEXT ACTION" "DOING" "TODO" "PROJECT" "DEFERRED" "DELEGATED" "REF" "NOTE" "|" "DONE" "DELETED")))
  ;; Use hex values for terminal and gui color support
  (setq org-todo-keyword-faces
        '(("NEXT ACTION" . (:foreground "#87d7d7" :background "#005c5c"))
          ("DOING" . (:foreground "#5c5c00" :background "#d7ff5f"))
          ("TODO" . (:foreground "#eeeeee" :background "#9a32cd"))
          ("PROJECT" . (:foreground "#005c00" :background "#5fff5f"))
          ("DEFERRED" . (:foreground "#444444" :background "#d7875f"))
          ("DELEGATED" . (:foreground "#005c5c" :background "#87d7d7"))
          ("REF" . (:foreground "#333333" :background "#bebebe"))
          ("NOTE" . (:foreground "#870000" :background "#af005f"))
          ("DONE" . (:foreground "#5f0000" :background "#eeeeee"))
          ("DELETED" . (:foreground "#ff0000" :background "#870000"))))
  (custom-set-faces

   '(default ((t (:background "#242424"))))
   '(org-tag ((t (:foreground "#000000" :background "#5fff00"))))
   '(org-level-1 ((t (:foreground "#d7ffff"))))
   '(org-level-2 ((t (:foreground "#d7ffd7"))))
   '(org-level-3 ((t (:foreground "#d7ffaf"))))
   '(org-level-4 ((t (:foreground "#d7ff87"))))
   '(org-special-keyword ((t (:foreground "#ff00ff" :background "#00005f"))))))

(defun smishy--set-key-bindings ()
  "Set smishy--taskflow key-bindings.
Sets keys for org-mode-map and org-agenda-mode-map.
   ',.p y|f gcrl
   aoeu i|d htns
    qjk x|b mwvz "
  (eval-after-load "org"
    '(progn
      (define-key org-mode-map (kbd "C-c C-c") 'smishy-insert-todo)
      (define-key org-mode-map (kbd "C-c C-d") 'org-deadline)
      (define-key org-mode-map (kbd "C-c C-h") 'org-schedule)
      (define-key org-mode-map (kbd "C-c C-t") 'org-set-tags)
      (define-key org-mode-map (kbd "C-c C-l") 'org-store-link)

      (define-key org-mode-map (kbd "C-c e") 'smishy-reload-tasks)
      (define-key org-mode-map (kbd "C-c p") 'smishy-create-project)
      (define-key org-mode-map (kbd "C-c c") 'smishy-reload-top)
      (define-key org-mode-map (kbd "C-c r") 'org-clock-goto)
      (define-key org-mode-map (kbd "C-c a") 'org-agenda)
      (define-key org-mode-map (kbd "C-c o") 'org-clock-out)
      (define-key org-mode-map (kbd "C-c i") 'org-clock-in)
      (define-key org-mode-map (kbd "C-c d") 'smishy-toggle-done)
      (define-key org-mode-map (kbd "C-c h") (lambda () (interactive) (org-agenda nil "h")))
      (define-key org-mode-map (kbd "C-c t") (lambda () (interactive) (org-todo-list "TODO")))
      (define-key org-mode-map (kbd "C-c n") (lambda () (interactive) (org-agenda-list 56)))
      (define-key org-mode-map (kbd "C-c s") 'smishy-save-n-go)
      (define-key org-mode-map (kbd "C-c b") 'org-iswitchb)

      ;; The following keybinds are for when emacs is in an xterm, shift + direction
      ;; keys return ESC [ 1 ; 2 bla  for some reason (reason is found in ECMA-48) ;;
      (define-key org-mode-map (kbd "ESC [ 1 ; 2 D") (kbd "<S-left>"))
      (define-key org-mode-map (kbd "ESC [ 1 ; 2 C") (kbd "<S-right>"))
      (define-key org-mode-map (kbd "ESC [ 1 ; 2 A") (kbd "<S-up>"))
      (define-key org-mode-map (kbd "ESC [ 1 ; 2 B") (kbd "<S-down>"))))

  (eval-after-load "org-agenda"
    '(progn
      (define-key org-agenda-mode-map (kbd "TAB") 'smishy--tab-out-of-agenda))))
