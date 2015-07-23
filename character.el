;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FROM ORIGINAL FILE:               pjb-cl.el
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2011
;;;;
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This library is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public
;;;;    License along with this library; if not, write to the Free Software
;;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
;;;;
;;;;*****************************************************************************

(defun character (character)
  "Common-Lisp: Returns the denoted character

URL:      http://www.informatimago.com/local/lisp/HyperSpec/Body/f_ch.htm
RETURN:   An emacs character.
EXAMPLES: (CHARACTER #\\a) => ?a   INVALID: # is invalid read syntax for emacs!
          (CHARACTER \"a\") => ?a
          (CHARACTER 'a)  => ?A    WARNING: emacs symbols are multicase!
          (CHARACTER '\\a) => ?a
          (CHARACTER 65.) is an error.
          (CHARACTER 'apple) is an error.
"
  (cond
    ((characterp character)   character)
    ((symbolp character)
     (if (= 1 (length (symbol-name* character)))
         (string-to-char (symbol-name* character))
         (error "character does not accept symbols of more than one character.")))
    ((stringp character)
     (string-to-char character))
    (t (error "Unexpected type fo character argument."))))
