;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               sample-init.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;    
;;;;    Sample init file
;;;;    
;;;;AUTHORS
;;;;    <quazimodo> quazimodo
;;;;MODIFICATIONS
;;;;    2013-05-25 <quazimodo> Created
;;;;BUGS
;;;;LEGAL
;;;;    LGPL3
;;;;    
;;;;    Copyright quazimodo 2013 - 2013
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

; I put libraries into vendor directory, then I load each subdirectory
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/contrib/lisp" t)
(progn (cd "~/.emacs.d/vendor")
       (normal-top-level-add-to-load-path))

; Make sure the taskflow is loaded
(require 'smishy-taskflow)

(smishy-start-taskflow "example-list.org")

