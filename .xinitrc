xrandr --output HDMI3 --right-of HDMI1
xrandr --output HDMI1 --primary

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

xset +dpms
xset dpms 0 0 180
xset r rate 200 40
xset b off

urxvtd -q -f -o &
redshift -l 47.22:10.09 -t 6000:4200 &

#irexec &> /dev/null &

workrave &> /dev/null &
if [[ -x '/usr/bin/mpd' ]]; then
    mpd .config/mpd/mpd.conf
fi

hsetroot -full workspace/Pictures/tmp/background.jpg &

until dbus-launch i3 &> /dev/null; do
	true
done
