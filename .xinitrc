xset +dpms
xset dpms 0 0 180
xset r rate 200 40
urxvtd -q -f -o &
redshift -l 47.22:10.09 &

until dbus-launch wmii; do
	true
done
