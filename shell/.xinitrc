xrandr --output DVI-I-2 --right-of DVI-I-3
xrandr --output DVI-I-3 --primary

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

xset +dpms
xset dpms 0 0 180
xset r rate 200 40
xset b off

redshift -l 47.22:10.09 -t 6000:4200 &


DIR="$HOME/workspace/Pictures/wallpapers"
if [[ -d "${DIR}" ]]; then
    background=$(ls -1 $DIR | shuf -n1)

    hsetroot -full "${DIR}/${background}"
fi

exec dbus-launch i3
