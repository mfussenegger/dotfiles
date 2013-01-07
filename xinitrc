systemd --user & 
xrandr --output HDMI3 --right-of HDMI1
xrandr --output HDMI1 --primary

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

xset +dpms
xset dpms 0 0 180
xset r rate 200 40
xset b off

if [[ -x '/usr/bin/blueman-applet' ]]; then
    blueman-applet &
fi

urxvtd -q -f -o &
redshift -l 47.22:10.09 -t 6000:4200 &

#irexec &> /dev/null &


DIR="$HOME/workspace/Pictures/wallpapers"

if [[ -d "${DIR}" ]]
then
    background=$(ls -1 $DIR | shuf -n1)

    hsetroot -full "${DIR}/${background}"
fi

until dbus-launch i3 &> /dev/null; do
	true
done
