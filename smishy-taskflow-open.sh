#!/bin/bash
##################### Script to pop open into smishy-taskflow ######################
# Bind this script to a keystroke in your wm, it will pop into the existing
# smishy-taskflow instance, or else it will create one if it doesnt exist :D
#
#######################################  ########################################

#Test to see if smishy-taskflow socket exists

config_location="~/.emacs.d/smishy-taskflow-init.el"

if [[ `screen -ls | grep -owc "smishy-taskflow"` = 1 ]]; then
    xterm -fa monaco -fs 12 -geometry 80x74 -e screen -raAd smishy-taskflow
else
    screen -dmS smishy-taskflow emacs -nw -q -l $config_location
fi
