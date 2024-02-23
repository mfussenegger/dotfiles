venv() {
  if [ -f bin/activate ]; then
    source bin/activate
  elif [ -f venv/bin/activate ]; then
    source venv/bin/activate
  elif [ -f .venv/bin/activate ]; then
      source .venv/bin/activate
  elif (( $+commands[uv] )) then
    uv venv venv
    source venv/bin/activate
  else
    python -m venv venv
    source venv/bin/activate
  fi
}


pip-upgrade-outdated() {
  python -m pip install -U $(python -m pip list --outdated | tail -n +3 | awk '{print $1}')
}
