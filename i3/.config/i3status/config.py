#!/usr/bin/env python
# -*- coding: utf-8 -*-

from i3pystatus import Status
import os


IS_NOTEBOOK = len(os.listdir('/sys/class/power_supply')) > 0

status = Status(standalone=True)

status.register('clock',
                format='%H:%M %d.%m',
                interval=30)
status.register('load')
status.register('temp')

if IS_NOTEBOOK:
    status.register('battery',
                    alert=True,
                    format='Batt: {percentage_design:.0f}% {status}')
    status.register('network',
                    interface='wlan0',
                    format_up='{v4} {essid} {quality:03.0f}%',
                    interval=5)
else:
    status.register('network',
                    interface='br0',
                    format_up='{v4}',
                    interval=5)

status.register('network',
                interface='eth0',
                format_up='{v4}',
                format_down='{interface}',
                interval=5)
status.register('pulseaudio', format='Volume: {volume}')
status.register('openvpn', vpn_name='lovely', format='VPN: {vpn_name}')


status.run()
