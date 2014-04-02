(defun smishy--set-variables ()
  "Set important smishy--taskflow variables."
  (setq org-mobile-directory "~/Webapps/mobileorg/files")
  ;; (setq org-mobile-capture-file "~/org/captured.org")
  (setq org-refile-targets '((org-agenda-files :todo . "PROJECT")
                             (org-agenda-files :todo . "REF")))
  (setq org-reverse-note-order t)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes t)
  (setq org-mobile-use-encryption nil)
  (setq org-mobile-encryption-password "swishybuttsaregreat")
  (setq smishy--work-line 9) ;set what line you will be entering your task
  (setq org-clock-report-include-clocking-task t)
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)
  (setq org-todo-keywords '((type "ACTION(a!)" "PROJECT(p)" "SOMEDAY(s@/!)" "WAITING(w@/!)" "|" "DONE(d)" "DELETED(D)")
                            (type "REF(r)" "NOTE(n)" "|" )))
  (setq org-tag-alist '(( "general" . 103) ( "home" . 104) ( "call" . 99) ( "mail" . 109) ( "errand" . 101) ( "event" . 69) ( "readreview" . 114)))
  (setq org-agenda-custom-commands
        '(("R" "Weekly Review"
           ((stuck "PROJECT" ((org-stuck-projects '("/PROJECT" ("ACTION" "WAITING") nil ""))))
            (todo "ACTION")
            (todo "WAITING")
            (agenda ""
                    ((org-agenda-span 'fortnight)
                     (org-agenda-clockreport-mode nil)))
            (todo "SOMEDAY")))
          ("g" tags-todo "general"
           ((org-agenda-overriding-header "General")))
          ("c" tags-todo "call"
           ((org-agenda-overriding-header "Calls")))
          ("m" tags-todo "mail"
           ((org-agenda-overriding-header "Mailable")))
          ("e" tags-todo "errand"
           ((org-agenda-overriding-header "Errands")))
          ("E" tags-todo "event"
           ((org-agenda-overriding-header "Events")))
          ("r" tags-todo "readreview"
           ((org-agenda-overriding-header "Read/Review Pile")))
          ("H" tags-todo "home"
           ((org-agenda-overriding-header "Home things")))
          ("h" "2 week agenda, DONE and ACTIONs"
           ((todo "DONE|DELETED")
            (todo "ACTION")
            (agenda "" ((org-agenda-span 1)
                        (org-agenda-clockreport-mode t)))
            (stuck "PROJECT" ((org-stuck-projects '("/PROJECT" ("ACTION" "WAITING") nil "")))))
           ((org-agenda-compact-blocks nil)))))
  (setq org-agenda-start-with-follow-mode t)
  (setq org-agenda-include-diary t)
  (setq org-lowest-priority 69)
  (setq org-default-priority 68)
  (setq org-startup-indented t)
  (setq org-startup-folded 'content)
  (setq org-completion-use-ido t)
  (setq ido-everywhere t)
  (setq ido-max-directory-size 100000)
  (ido-mode 'both))

(defun smishy--set-capture-templates (capture-file)
  (setq org-capture-templates
        `(("g" "General" entry (file ,capture-file)
           "* ACTION %? :general:\n%u")
          ("h" "Home" entry (file ,capture-file)
           "* ACTION %? :home:\n%u")
          ("c" "Call" entry (file ,capture-file)
           "* ACTION %? :call:\n%u")
          ("m" "(e)Mail" entry (file ,capture-file)
           "* ACTION %? :mail:\n%u")
          ("e" "Errand" entry (file ,capture-file)
           "* ACTION %? :errand:\n%u")
          ("E" "Event" entry (file ,capture-file)
           "* ACTION %? :event:\n%u")
          ("r" "Read/Review" entry (file ,capture-file)
           "* ACTION %? :readreview:\n%u")
          ("w" "Waiting" entry (file ,capture-file)
           "* WAITING %?\n%u"))))

(defun smishy--set-faces ()
  "Set smishy--taskflow faces."

  ;; Use hex values for terminal and gui color support
  (setq org-todo-keyword-faces
        '(("ACTION" . (:foreground "#eeeeee" :background "#9a32cd"))
          ("PROJECT" . (:foreground "#005c00" :background "#5fff5f"))
          ("SOMEDAY" . (:foreground "#444444" :background "#d7875f"))
          ("WAITING" . (:foreground "#005c5c" :background "#87d7d7"))
          ("REF" . (:foreground "#333333" :background "#bebebe"))
          ("NOTE" . (:foreground "#870000" :background "#af005f"))
          ("DONE" . (:foreground "#5f0000" :background "#eeeeee"))
          ("DELETED" . (:foreground "#ff0000" :background "#870000"))))
  (custom-set-faces

   '(default ((t (:background "#242424"))))
   '(org-tag ((t (:foreground "#555555"))))
   '(org-level-1 ((t (:foreground "#d7ffff"))))
   '(org-level-2 ((t (:foreground "#d7ffd7"))))
   '(org-level-3 ((t (:foreground "#d7ffaf"))))
   '(org-level-4 ((t (:foreground "#d7ff87"))))
   '(org-agenda-date-today ((t (:background "#444444"))))
   '(org-special-keyword ((t (:foreground "#ff00ff" :background "#00005f"))))))


(defun smishy--set-key-bindings ()
  "Set smishy--taskflow key-bindings.
Sets keys for org-mode-map and org-agenda-mode-map.
   ',.p y|f gcrl
   aoeu i|d htns
    qjk x|b mwvz "
  (eval-after-load "org"
    '(progn
       (define-key global-map (kbd "C-c c") 'org-capture)

       (define-key org-mode-map (kbd "C-t i") 'org-clock-in)
       (define-key org-mode-map (kbd "C-t o") 'org-clock-out)
       (define-key org-mode-map (kbd "C-t c") 'org-clock-goto)

       (define-key org-mode-map (kbd "C-c d") 'smishy-toggle-done)

       (define-key org-mode-map (kbd "C-c h") (lambda () (interactive) (org-agenda nil "h")))
       (define-key org-mode-map (kbd "C-c t") (lambda () (interactive) (org-todo-list "ACTION")))
       (define-key org-mode-map (kbd "C-c n") (lambda () (interactive) (org-agenda-list 56)))

      (define-key org-mode-map (kbd "C-c a") 'org-agenda)

      ;; The following keybinds are for when emacs is in an xterm, shift + direction
      ;; keys return ESC [ 1 ; 2 bla  for some reason (reason is found in ECMA-48) ;;
      (define-key org-mode-map (kbd "ESC [ 1 ; 2 D") (kbd "<S-left>"))
      (define-key org-mode-map (kbd "ESC [ 1 ; 2 C") (kbd "<S-right>"))
      (define-key org-mode-map (kbd "ESC [ 1 ; 2 A") (kbd "<S-up>"))
      (define-key org-mode-map (kbd "ESC [ 1 ; 2 B") (kbd "<S-down>"))))

  (eval-after-load "org-agenda"
    '(progn
      (define-key org-agenda-mode-map (kbd "TAB") 'smishy--tab-out-of-agenda))))

(defun smishy--set-max-priority()
  "Set the current ACTION to have the maximum priority"
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char marker)
          (org-back-to-heading t)
          (org-priority 65))))))
