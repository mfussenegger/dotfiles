#!/usr/bin/env python
# -*- coding: utf-8 -*-

import time
import sys
import re

from subprocess import call
from datetime import datetime, timedelta
from multiprocessing import Process

from sh import amixer


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


def get_vol():
    output = str(amixer('sget', 'Master'))
    match = re.findall('Front Left: .*\[(\d+)%\].*', output)
    if match:
        return int(match[0])
    return 0


def inc_vol():
    amixer('-q', 'set', 'Master', '2%+', 'unmute')


def max_volume():
    while get_vol() < 80:
        inc_vol()
        time.sleep(10)

def parse_arg(arg):
    sleep_cycles = 0
    try:
        sleep_cycles = int(arg)
    except ValueError:
        now = datetime.now()
        dt = datetime.strptime(arg, '%H:%M')
        dt = dt.replace(year=now.year, month=now.month, day=now.day)
        dt += timedelta(1)
        dt -= timedelta(minutes=20)
        print(dt)
        print(now)
        delta = dt - now
        minutes = delta.total_seconds() / 60
        sleep_cycles = minutes / 90.
    return sleep_cycles


def main():
    if len(sys.argv) >= 2:
        sleep_cycles = parse_arg(sys.argv[1])
    else:
        sleep_cycles = get_sleep_cycles()
    print('sleep cycles: ' + str(sleep_cycles))

    # 20 minutes to fall asleep
    minutes_to_sleep = (sleep_cycles * 90 + 20)
    print(minutes_to_sleep)
    print('wakeup at {0:%H:%M}'.format(
        now + timedelta(minutes=minutes_to_sleep)))

    try:
        time.sleep(5)
    except KeyboardInterrupt:
        exit()

    seconds_to_sleep = int((minutes_to_sleep - 2) * 60)

    call('sudo rtcwake -m mem -s {0}'.format(seconds_to_sleep), shell=True)
    time.sleep(2 * 60)  # time for pc to wake up
    p = Process(target=max_volume)
    p.start()
    try:
        call('mplayer ~/workspace/audio/wakeup/*', shell=True)
    except KeyboardInterrupt:
        p.terminate()
        sys.exit(0)

    call(['mpc', 'play'])
    time.sleep(15)
    call(['sudo', 'rc.d', 'restart', 'fancontrol'])


if __name__ == '__main__':
    main()
