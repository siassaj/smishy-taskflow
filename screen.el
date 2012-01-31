;; With Screen between Emacs and the "real" terminal, it doesn't occur
;; to Emacs to translate some keybindings.  Teach it about a handful
;; that are important to me (because paredit uses them).  Don't bother
;; looking at $COLORTERM, since after screen -xRR or screen -dRR, it
;; will be inaccurate.
(mapc (lambda (x)
        (define-key function-key-map (car x) (cdr x)))
      (nconc
       ;; rxvt-unicode
       '(("\e[1~" . [home])
         ("\e[4~" . [end])
         ("\e[5~" . [prior])
         ("\e[6~" . [next])
         ("\e[a" . [S-up])
         ("\e[b" . [S-down])
         ("\e[c" . [S-right])
         ("\e[d" . [S-left])
         ("\eOa" . [C-up])
         ("\eOb" . [C-down])
         ("\eOc" . [C-right])
         ("\eOd" . [C-left])
         ("\eOm" . [kp-subtract])
         ("\eOj" . [kp-multiply])
         ("\eOo" . [kp-divide])
         ("\eOM" . [kp-enter]))
       ;; xterm
       '(("\e[1;3A" . [M-up])
         ("\e[1;3B" . [M-down])
         ("\e[1;3C" . [M-right])
         ("\e[1;3D" . [M-left])
         ("\e[1;5A" . [C-up])
         ("\e[1;5B" . [C-down])
         ("\e[1;5C" . [C-right])
         ("\e[1;5D" . [C-left]))
       ;; libvte (gnome-terminal, xfce4-terminal)
       '(("\eO3A" . [M-up])
         ("\eO3B" . [M-down])
         ("\eO3C" . [M-right])
         ("\eO3D" . [M-left])
         ("\eO5A" . [C-up])
         ("\eO5B" . [C-down])
         ("\eO5C" . [C-right])
         ("\eO5D" . [C-left])
         ("\eO7A" . [C-M-up])
         ("\eO7B" . [C-M-down])
         ("\eO7C" . [C-M-right])
         ("\eO7D" . [C-M-left])
         ("\e[Z" . [S-iso-lefttab]))))

;; Every time any command is run, reify the frame title.  If it has
;; changed, tell screen what the new title is.
(when (fboundp 'format-mode-line)
  (defvar frame-title nil)
  (add-hook 'post-command-hook
            (lambda ()
              (when (or (not (featurep 'multi-tty))
                        (and (featurep 'multi-tty)
			     (tty-type)
                             (string-match "\\`screen" (tty-type))))
                (let ((frame-title* (format-mode-line frame-title-format)))
                  (unless (equal frame-title frame-title*)
                    (send-string-to-terminal
                     (concat "\ek" (setq frame-title frame-title*) "\e\\"))))))))
