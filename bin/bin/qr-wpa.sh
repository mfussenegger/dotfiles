#!/bin/sh

set -e

QRENCODEBIN=/usr/bin/qrencode

print_usage() {
	echo "qr-wpa.sh <SSID> <ENC> <PSK> <FILENAME>
	
<SSID>		SSID of the wireless network (may contain spaces, use quotes)
<ENC>		'WEP', 'WPA' or 'nopass'
<PSK>		Pre-shared key of the network
<FILENAME>	Filename to write QR-code

WEP/WPA/nopass respectively stands for WEP, WPA or no encryption. When nopass
is used, PSK cannot be empty, so fill in something random. Mind the capitalisation
in the ENC-directive.
"
}

if [ ! -x $QRENCODEBIN ]; then
	echo "Fatal error: qrencode not found, or not executable"
	exit 1
fi

if [[ $BASH_ARGC -lt 1 ]]; then
	print_usage
	exit 254
fi

if [[ $BASH_ARGC -lt 4 ]]; then
	echo "Not enough arguments given, expecting 4, got ${BASH_ARGC}"
	print_usage
	exit 1
fi

SSID=${1}
ENC=${2}
PSK=${3}
FILENAME=${4}

if [[ $ENC == "WPA" || $ENC == "WEP" || $ENC == "nopass" ]]; then
	echo "Good job!"
else
	echo "Expecting WPA, WEP or nopass (mind capitalisation), exiting"
	exit 1
fi

echo "WIFI:S:${SSID};T:${ENC};P:${PSK};;" | qrencode -o ${4}

echo "QR-code written to ${4}"
