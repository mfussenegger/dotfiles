xset +dpms
xset dpms 0 0 180
urxvtd -q -f -o &

until wmii; do
	true
done
