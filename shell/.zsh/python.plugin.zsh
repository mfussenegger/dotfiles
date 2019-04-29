venv() {
    if [ -f venv/bin/activate ]
    then
        source venv/bin/activate
    else
        if [ -f .venv/bin/activate ]
        then
            source .venv/bin/activate
        else
            python -m venv venv
            venv/bin/python -m pip install -U pip
            source venv/bin/activate
        fi
    fi
}


pip-upgrade-outdated() {
  python -m pip install -U $(python -m pip list --outdated | tail -n +3 | awk '{print $1}')
}
