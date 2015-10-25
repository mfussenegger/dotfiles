
if [[ -x '/usr/bin/autorandr' ]]; then
    autorandr --change --force
fi

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

xset +dpms
xset dpms 0 0 180
xset r rate 200 40
xset b off

redshift -l 47.22:10.09 -t 6000:4200 &


if [[ -x '/usr/bin/syndaemon' ]]; then
    syndaemon -t -k -i 2 -d &
fi

DIR="$HOME/workspace/Pictures/wallpapers"
if [[ -d "${DIR}" ]]; then
    background=$(ls -1 $DIR | shuf -n1)

    hsetroot -full "${DIR}/${background}"
fi

exec dbus-launch i3
