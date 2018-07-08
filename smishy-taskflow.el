;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               smishy-taskflow.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This library is my implementation of Getting Things Done.
;;;;    It uses org-mode, some helper functions and settings, and
;;;;    designed to be extremely quick and out of the way. It is nicer
;;;;    when used with a tiling window manager.
;;;;
;;;;AUTHORS
;;;;    <quazimodo> Siavash S. Sajjadi <super.quazimodo@gmail.com>
;;;;MODIFICATIONS
;;;;    2013-05-25 <quazimodo> Add License, rename
;;;;BUGS
;;;;LEGAL
;;;;    LGPL3
;;;;
;;;;    Copyright Siavash S. Sajjadi 2011 - 2013
;;;;
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later
;;;;    version.
;;;;
;;;;    This library is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU Lesser General Public License for more
;;;;    details.
;;;;
;;;;    You should have received a copy of the  GNU Lesser General
;;;;    Public License along with this library.
;;;;    If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
;;;;

(require 'screen)
(require 'org)
(require 'popup)

(load-library "org-refile-overrides")
(load-library "character")
(load-library "configure")
(load-library "auto-update-agenda-views")

g(defun smishy-start-taskflow (files-path)
  "Start the smishy task flow"
  (interactive)
  (setq inhibit-splash-screen t)

  (mapc (lambda (file)
          (add-to-list 'org-agenda-files (concat files-path file)))
        '("personal.org"
          "work.org"
          "friends.org"
          "captured.org"
          "someday.org"
          "references.org"
          "sentia.org"))
  (smishy--set-variables)
  (smishy--set-faces)
  (smishy--set-capture-templates (concat files-path "captured.org"))
  (smishy--set-key-bindings)
  ;; (smishy--auto-update-agenda-views-start)
  (org-mode)
  (org-agenda nil "h")
  (delete-other-windows))

(provide 'smishy-taskflow)
