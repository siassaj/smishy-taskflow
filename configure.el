(defun smishy--set-variables ()
  "Set important smishy--taskflow variables."
  (setq org-mobile-directory "~/Webapps/mobileorg/files")
  ;; (setq org-mobile-capture-file "~/org/captured.org")
  (setq org-refile-targets '((org-agenda-files :todo . "PROJECT")
                             (org-agenda-files :todo . "REF")))
  ;; (setq org-reverse-note-order t)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes t)
  (setq org-mobile-use-encryption nil)
  (setq org-mobile-encryption-password "swishybuttsaregreat")
  (setq smishy--work-line 9) ;set what line you will be entering your task
  (setq org-clock-report-include-clocking-task t)
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-enforce-todo-dependencies t)
  (setq org-todo-keywords '((type "ACTION(a!)" "SOMEDAY(s@/!)" "WAITING(w@/!)" "|" "DONE(d)" "DELETED(D)")
                            (type "REF(r)" "NOTE(n)" "|" )
                            (type "PROJECT(p)" "|" "CLOSED(c@/!)")))
  (setq org-drawers (append org-drawers '("DETAILS")))
  (setq org-tag-alist
        `(("general" . ,(character "g"))
          ("home" . ,(character "h"))
          ("meeting" . ,(character "M"))
          ("office" . ,(character "o"))
          ("projman" . ,(character "a"))
          ("web" . ,(character "W"))
          ("call" . ,(character "c"))
	  ("coffee" . ,(character "j"))
	  ("break" . ,(character "b"))
          ("mail" . ,(character "m"))
          ("errand" . ,(character "e"))
          ("event" . ,(character "E"))
          ("review" . ,(character "r"))
          ("flagged" . ,(character "f"))))

  (setq org-agenda-custom-commands
        '(("R" "Weekly Review, lists scheduled/deadlined tasks"
           ((stuck "PROJECT" ((org-stuck-projects '("/PROJECT" ("ACTION" "WAITING" "SOMEDAY") nil ""))))
            (todo "DONE|DELETED|CLOSED")
            (todo "ACTION")
            (todo "WAITING")
            (agenda ""
                    ((org-agenda-span 'fortnight)
                     (org-agenda-clockreport-mode nil)))
            (todo "SOMEDAY"))
           ((org-agenda-todo-ignore-scheduled nil)
            (org-agenda-todo-ignore-deadlines nil)
            (org-agenda-todo-ignore-with-date nil)))
          ("g" tags-todo "general"
           ((org-agenda-overriding-header "General")))
          ("M" tags-todo "meeting"
           ((org-agenda-overriding-header "Meeting")))
          ("W" tags-todo "web"
           ((org-agenda-overriding-header "Web")))
          ("c" tags-todo "call"
           ((org-agenda-overriding-header "Calls")))
          ("a" tags-todo "projman"
           ((org-agenda-overriding-header "ProjMan")))
          ("j" tags-todo "coffee"
	   ((org-agenda-overriding-header "Coffee")))
          ("b" tags-todo "break"
	   ((org-agenda-overriding-header "Break")))
          ("m" tags-todo "mail"
           ((org-agenda-overriding-header "Mailable")))
          ("e" tags-todo "errand"
           ((org-agenda-overriding-header "Errands")))
          ("E" tags-todo "event"
           ((org-agenda-overriding-header "Events")))
          ("r" tags-todo "review"
           ((org-agenda-overriding-header "Read/Review Pile")))
          ("H" tags-todo "home"
           ((org-agenda-overriding-header "Home things")))
          ("o" tags-todo "office"
           ((org-agenda-overriding-header "Office things")))

          ("h" "2 week agenda, DONE and ACTIONs"
           ((todo "DONE|DELETED|CLOSED" ((org-agenda-todo-ignore-scheduled nil)
                                         (org-agenda-todo-ignore-deadlines nil)
                                         (org-agenda-todo-ignore-with-date nil)))
            (todo "ACTION")
            (agenda "" ((org-agenda-span 1)
                        (org-agenda-clockreport-mode t)))
            (todo "WAITING")
            (stuck "PROJECT" ((org-stuck-projects '("/PROJECT" ("ACTION" "WAITING" "SOMEDAY") nil "")))))
           ((org-agenda-compact-blocks nil)))))

  (setq org-agenda-start-with-0ollow-mode t)
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
          ("M" "Meeting" entry (file ,capture-file)
           "* ACTION %? :meeting:\n%u")
          ("h" "Home" entry (file ,capture-file)
           "* ACTION %? :home:\n%u")
          ("W" "Web" entry (file ,capture-file)
           "* ACTION %? :web:\n%u")
          ("o" "Office" entry (file ,capture-file)
           "* ACTION %? :office:\n%u")
          ("a" "ProjMan" entry (file ,capture-file)
           "* ACTION %? :projman:\n%u")
	  ("j" "Coffee" entry (file ,capture-file)
           "* ACTION %? :coffee:\n%u")
	  ("b" "Break" entry (file ,capture-file)
           "* ACTION %? :break:\n%u")
          ("c" "Call" entry (file ,capture-file)
           "* ACTION %? :call:\n%u")
          ("m" "(e)Mail" entry (file ,capture-file)
           "* ACTION %? :mail:\n%u")
          ("e" "Errand" entry (file ,capture-file)
           "* ACTION %? :errand:\n%u")
          ("E" "Event" entry (file ,capture-file)
           "* ACTION %? :event:\n%u")
          ("r" "Read/Review" entry (file ,capture-file)
           "* ACTION %? :review:\n%u")
          ("w" "Waiting" entry (file ,capture-file)
           "* WAITING %?\n%u")
          ("p" "Project" entry (file ,capture-file)
           "* PROJECT %?\nOutcome: %^{Outcome}\n%u"))))

(defun smishy--set-faces ()
  "Set smishy--taskflow faces."

  ;; Use hex values for terminal and gui color support
  (setq org-tag-faces
        '(("web" :foreground "#0000cd" :weight "bold")
          ("call" :foreground "#aa0000" :weight "bold")
          ("projman" :foreground "#00aacd" :weight "bold")
          ("errand" :foreground "#008b8b" :weight "bold")
          ("review" :foreground "#cd00cd" :weight "bold")
          ("mail" :foreground "#00afcc" :weight "bold")
          ("home" :foreground "#ff875f" :weight "bold")
          ("general" :foreground "#5f5faf" :weight "bold")
          ("flagged" :foreground "#000000" :background "#ff33ff" :weight "bold")))


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
   '(org-tag ((t (:foreground "#5f5faf"))))
   '(default ((t (:background "#242424"))))
   '(org-tag ((t (:foreground "#555555"))))
   '(org-level-1 ((t (:foreground "#d7ffff"))))
   '(org-level-2 ((t (:foreground "#d7ffd7"))))
   '(org-level-3 ((t (:foreground "#d7ffaf"))))
   '(org-level-4 ((t (:foreground "#d7ff87"))))
   '(org-agenda-date-today ((t
                             (:weight bold :slant italic :underline "#cd00cd"
                                      :background "#444444" :inherit (org-agenda-date)))))
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
  )
;; (eval-after-load "org-agenda"
;;   '(progn
;;     (define-key org-agenda-mode-map (kbd "TAB") 'smishy--tab-out-of-agenda))))

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
