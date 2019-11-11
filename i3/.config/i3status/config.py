from i3pystatus import Status
from pathlib import Path
try:
    import netifaces
except ImportError:
    netifaces = None
import os


status = Status(standalone=True)
status.register('clock', format='%H:%M %d.%m', interval=30)
status.register('load')
status.register('temp')


batteries = [b for b in os.listdir('/sys/class/power_supply')
             if not b.startswith('hid')]
if batteries:
    status.register('battery',
                    alert=True,
                    format='Batt: {percentage_design:.0f}% {status}')


if netifaces:
    hidden_ifaces = set(['lo', 'docker0'])
    for iface in netifaces.interfaces():
        if iface in hidden_ifaces or iface.startswith('tun'):
            continue
        if iface.startswith('w'):
            fmt = '{v4} {essid} {quality:03.0f}%'
        else:
            fmt = '{v4}'
        status.register('network', interface=iface, format_up=fmt, interval=5)


status.register('pulseaudio', format='♪ {volume}')
vpn_conf_dir = Path('/etc/openvpn/client/')

for conf in vpn_conf_dir.glob('*.conf'):
    name, _ = os.path.splitext(conf.name)
    status.register(
        'openvpn',
        vpn_name=name,
        vpn_up_command='sudo systemctl start openvpn-client@%(vpn_name)s.service',
        vpn_down_command='sudo systemctl stop openvpn-client@%(vpn_name)s.service',
        status_command="bash -c 'systemctl show openvpn-client@%(vpn_name)s | grep ActiveState=active'",
        format='VPN: {vpn_name}')
status.run()
