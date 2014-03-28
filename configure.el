(defun smishy--set-variables ()
  "Set important smishy--taskflow variables."
  (setq smishy--work-line 9) ;set what line you will be entering your task
  (setq org-agenda-start-with-clockreport-mode t)
  (setq org-tag-alist '("general(g)" "home(h)" "call(c)" "mail(m)" "errand(e)" "event(E)" "review(r)"))
  (setq org-agenda-custom-commands
        '(("g" tags-todo "general")
          ("c" tags-todo "call")
          ("m" tags-todo "mail")
          ("e" tags-todo "errand")
          ("E" tags-todo "event")
          ("r" tags-todo "review")
          ("H" tags-todo "home")
          ("h" "2 week agenda, DONE and ACTIONs"
           ((agenda "" ((org-agenda-start-on-weekday nil)))
            (todo "DONE")
            (todo "DELETED")
            (todo "ACTION")
            (stuck "PROJECT"))
           ((org-agenda-compact-blocks nil)))))
  (setq org-agenda-start-with-follow-mode t)
  (setq org-lowest-priority 69)
  (setq org-default-priority 68)
  (setq org-stuck-projects '("/PROJECT" ("ACTION") nil ""))
  (setq org-startup-indented t)
  (setq org-startup-folded 'content))

(defun smishy--set-capture-templates ()
  (setq org-capture-templates
        '(("g" "General" entry (file "~/org/biglist.org")
           "* ACTION %? :general:\n%u")
          ("h" "Home" entry (file "~/org/biglist.org")
           "* ACTION %? :home:\n%u")
          ("c" "Call" entry (file "~/org/biglist.org")
           "* ACTION %? :call:\n%u")
          ("m" "(e)Mail" entry (file "~/org/biglist.org")
           "* ACTION %? :mail:\n%u")
          ("e" "Errand" entry (file "~/org/biglist.org")
           "* ACTION %? :errand:\n%u")
          ("E" "Event" entry (file "~/org/biglist.org")
           "* ACTION %? :event:\n%u")
          ("r" "Read/Review" entry (file "~/org/biglist.org")
           "* ACTION %? :review:\n%u"))))

(defun smishy--set-faces ()
  "Set smishy--taskflow faces."
  (setq org-todo-keywords '((type "ACTION" "PROJECT" "SOMEDAY" "WAITING" "|" "DONE" "DELETED")
                            (type "|" "REF" "NOTE")))
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

       (define-key org-mode-map (kbd "C-c C-t") 'org-set-tags)

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
