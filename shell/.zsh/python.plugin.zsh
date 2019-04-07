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
