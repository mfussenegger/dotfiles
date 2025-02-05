
# repeat command until it works
until-success() {
    "$@"
    while [ $? -ne 0 ]; do
        "$@"
    done
}

# repeat command until it fails
until-error() {
    start_time=$(date +%s)
    "$@"
    while [ $? -eq 0 ]; do
        duration=$(($(date +%s) - $start_time))
        echo -n -e "\033]0;$(( $duration / 60 )) min $(( $duration % 60 )) sec: $@\007"
        "$@"
    done
}

mcd() {
    [[ -n "$1" ]] && mkdir -p "$1" && builtin cd "$1"
}

sudo-scp() {
  # On remote machine:
  # - Install `x11-ssh-askpass`
  # - Add `Path askpass /usr/lib/ssh/x11-ssh-askpass` in `/etc/sudo.conf`
  # - Set `X11Forwarding yes` in `/etc/ssh/sshd_config`
  rsync --progress -e "ssh -Y" --rsync-path="sudo rsync" "$@"
}

fix-term() {
  infocmp | ssh -t $1 "tic -"
}

zebra() {
  awk '{if (NR%2 == 0) {print "\033[32m" $0 "\033[0m"} else {print}}'
}
