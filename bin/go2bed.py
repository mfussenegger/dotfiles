#!/usr/bin/env python
# -*- coding: utf-8 -*-

from subprocess import call
import time
from datetime import datetime, timedelta
import sys

sleep_cycles = 5
now = datetime.now()

if now.weekday() in [4, 5] or \
        (now.weekday() == 6 and now.hour < 6):
    sleep_cycles += 1

if now.hour <= 1:
    sleep_cycles -= 1
elif now.hour <= 5:
    sleep_cycles -= 2
elif now.hour > 5 and now.hour <= 20:
    sleep_cycles = 1

# 20 minutes to fall asleep
minutes_to_sleep = (sleep_cycles * 90 + 20)
print(minutes_to_sleep)
print('wakeup at {0:%H:%M}'.format(now + timedelta(minutes=minutes_to_sleep)))

time.sleep(5)

seconds_to_sleep = minutes_to_sleep * 60

call('sudo rtcwake -m mem -s {0}'.format(seconds_to_sleep), shell=True)
call(['mpc', 'play'])

time.sleep(15)

call(['sudo', 'rc.d', 'restart', 'fancontrol'])
