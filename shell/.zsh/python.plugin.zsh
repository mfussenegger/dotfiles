pip_outdated() {
    pip freeze | cut -d = -f 1 | xargs -n 1 pip search | grep -B2 'LATEST:'
}


new_buildout() {
    wget http://downloads.buildout.org/2/bootstrap.py
    echo "[versions]" > versions.cfg
    cat > buildout.cfg << EOF
[buildout]
develop = .
extends = versions.cfg
versions = versions
show-picked-versions = true
parts = scripts
        test


[scripts]
recipe = zc.recipe.egg:script
interpreter = py
eggs = wheel

[test]
relative-paths = true
recipe = zc.recipe.testrunner
defaults = ['--auto-color']
EOF
    python3 -m venv .venv
    .venv/bin/python -m ensurepip
    .venv/bin/python bootstrap.py
}


if [[ -x '/usr/bin/virtualenvwrapper.sh' ]]; then
    source /usr/bin/virtualenvwrapper.sh
fi
