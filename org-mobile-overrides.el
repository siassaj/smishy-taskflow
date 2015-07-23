(defun org-mobile-encrypt-and-move (infile outfile)
  "Encrypt INFILE locally to INFILE_enc, then move it to OUTFILE.
We do this in two steps so that remote paths will work, even if the
encryption program does not understand them."
  (let ((encfile (concat infile "_enc")))
    (org-mobile-encrypt-file infile encfile)
    (when outfile
      (copy-file encfile outfile 'ok-if-exists nil nil 'do-not-preserve-uid-guid)
      (delete-file encfile))))
