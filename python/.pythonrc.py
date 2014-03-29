"""
This file is executed when the Python interactive shell is started if
$PYTHONSTARTUP is in your environment and points to this file. It's just
regular Python commands, so do what you will. Your ~/.inputrc file can greatly
complement this file.

"""

try:
    from IPython import embed
    embed()
    raise SystemExit
except ImportError:
    print('Could not launch IPython')
except ValueError:
    pass
