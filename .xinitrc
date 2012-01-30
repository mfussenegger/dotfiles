xrandr --output HDMI3 --right-of HDMI1

xset +dpms
xset dpms 0 0 180
xset r rate 200 40
urxvtd -q -f -o &
redshift -l 47.22:10.09 -t 6000:4200 &
xset b off
#irexec &> /dev/null &
workrave &> /dev/null &

hsetroot -full workspace/Pictures/tmp/background.jpg &

until dbus-launch i3 &> /dev/null; do
	true
done
