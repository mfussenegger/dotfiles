if hash randrctl 2> /dev/null; then
    randrctl auto
fi

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

xrandr --dpi 144
xset +dpms
xset dpms 0 0 180
xset r rate 200 40
xset b off

redshift -l 47.22:10.09 -t 6000:4200 &

systemctl --user import-environment DISPLAY XAUTHORITY
exec i3
