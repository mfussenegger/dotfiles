#!/usr/bin/env python
# -*- coding: utf-8 -*-

import time
import sys

from subprocess import call
from datetime import datetime, timedelta

now = datetime.now()


def get_sleep_cycles():
    sleep_cycles = 5

    if now.weekday() in [4, 5] or \
            (now.weekday() == 6 and now.hour < 6):
        sleep_cycles += 1

    if now.hour <= 1:
        sleep_cycles -= 1
    elif now.hour <= 5:
        sleep_cycles -= 2
    elif now.hour > 5 and now.hour <= 20:
        sleep_cycles = 1

    return sleep_cycles


def main():
    if len(sys.argv) >= 2:
        sleep_cycles = int(sys.argv[1])
    else:
        sleep_cycles = get_sleep_cycles()

    # 20 minutes to fall asleep
    minutes_to_sleep = (sleep_cycles * 90 + 20)
    print(minutes_to_sleep)
    print('wakeup at {0:%H:%M}'.format(now + timedelta(minutes=minutes_to_sleep)))

    try:
        time.sleep(5)
    except KeyboardInterrupt:
        exit()

    seconds_to_sleep = minutes_to_sleep * 60

    call('sudo rtcwake -m mem -s {0}'.format(seconds_to_sleep), shell=True)
    try:
        call('mplayer ~/workspace/audio/wakeup/*', shell=True)
    except KeyboardInterrupt:
        sys.exit(0)

    call(['mpc', 'play'])

    time.sleep(15)

    call(['sudo', 'rc.d', 'restart', 'fancontrol'])


if __name__ == '__main__':
    main()
