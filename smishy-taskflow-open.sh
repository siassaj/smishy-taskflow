#!/bin/bash
# -*- mode:sh;coding:utf-8 -*-
#*****************************************************************************
#FILE:               smishy-taskflow-open.sh
#LANGUAGE:           sh
#SYSTEM:             POSIX
#USER-INTERFACE:     NONE
#DESCRIPTION
#    
#    Bind this script to a keystroke in your wm, it will pop into the existing
#    smishy-taskflow instance, or else it will create one if it doesnt exist.
#    
#AUTHORS
#    <quazimodo> Siavash S. Sajjadi <super.quazimodo@gmail.com>
#MODIFICATIONS
#    2013-05-27 <quazimodo> Fix license
#BUGS
#LEGAL
#    LGPL3
#    
#    Copyright Siavash S. Sajjadi 2011 - 2013
#    
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 3 of the License, or (at your option) any later
#    version.
#    
#    This library is distributed in the hope that it will be
#    useful, but WITHOUT ANY WARRANTY; without even the implied
#    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#    PURPOSE.  See the GNU Lesser General Public License for more
#    details.
#    
#    You should have received a copy of the  GNU Lesser General
#    Public License along with this library.
#    If not, see <http://www.gnu.org/licenses/>.
#*****************************************************************************
#

#Test to see if smishy-taskflow socket exists
config_location="~/.emacs.d/smishy-taskflow-init.el"

if [[ `screen -ls | grep -owc "smishy-taskflow"` = 1 ]]; then
    xterm -fa monaco -fs 12 -geometry 80x74 -e screen -raAd smishy-taskflow
else
    screen -dmS smishy-taskflow emacs -nw -q -l $config_location
fi
