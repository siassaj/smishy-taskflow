#!/bin/bash
##################### Script to pop open into emacs-tasks ######################
# Bind this script to a keystroke in your wm, it will pop into the existing
# emacs-tasks instance, or else it will create one if it doesnt exist :D
#
#######################################  ########################################

#Test to see if emacs-tasks socket exists

if [[ `screen -ls | grep -owc "emacs-tasks"` = 1 ]]; then
    xterm -fa monaco -fs 12 -geometry 80x74 -e screen -raAd emacs-tasks
else
    screen -dmS emacs-tasks emacs -nw -q -l /home/quazimodo/.emacs-tasks
fi