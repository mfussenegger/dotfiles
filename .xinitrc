xset +dpms
xset dpms 0 0 180
urxvtd -q -f -o &
redshift -l 47.22:10.09 &

until wmii; do
	true
done
