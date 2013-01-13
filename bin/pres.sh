#!/bin/bash

DIR="$( cd "$( dirname "$0" )" && pwd )"

if [ "$1" == "on" ]; then
    xrdb $DIR/../Xresources-solarized
    xset s off; xset -dpms
    echo "on"
else
    echo "off"
    xrdb $DIR/../Xresources
    xset s on; xset +dpms
fi
