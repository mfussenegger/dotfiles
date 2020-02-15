[[ -f ~/.xrandr.sh ]] && ~/.xrandr.sh
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

xset +dpms
xset dpms 0 0 180
xset r rate 200 40
xset b off
setxkbmap -layout de -variant nodeadkeys -option caps:escape

redshift -l 47.22:10.09 -t 6000:4200 &

systemctl --user import-environment DISPLAY XAUTHORITY
exec i3
