
# repeat command until it works
until-success() {
    "$@"
    while [ $? -ne 0 ]; do
        "$@"
    done
}

# repeat command until it fails
until-error() {
    "$@"
    while [ $? -eq 0 ]; do
        "$@"
    done
}

mcd() {
    [[ -n "$1" ]] && mkdir -p "$1" && builtin cd "$1"
}

if-up() {
    if [ $# -lt 2 ]; then
        echo "Usage: when-up host command"
        exit 1
    fi

    if [ $1 = "ssh" ]; then
        HOST=$2
    else
        HOST=$1
    fi

    echo "Waiting for $HOST to come online..."

    ping -c 1 -W 1 $HOST >/dev/null
    while [ $? -ne 0 ]; do
        sleep 1
        ping -c 1 -W 1 $HOST >/dev/null
    done

    if [ $1 != "ssh" ]; then
        shift
    fi
    $*
}

fix-term() {
  infocmp | ssh -t $1 "tic -"
}

zebra() {
  awk '{if (NR%2 == 0) {print "\033[32m" $0 "\033[0m"} else {print}}'
}

export NNN_TMPFILE="/tmp/nnn"
n() {
  nnn "$@"
  if [ -f $NNN_TMPFILE ]; then
    . $NNN_TMPFILE
    rm $NNN_TMPFILE
  fi
}
