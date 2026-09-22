#!/bin/sh
# Status line for swaybar: battery, bluetooth, date.
while :; do
    bat=$(cat /sys/class/power_supply/BAT0/capacity)
    state=$(cat /sys/class/power_supply/BAT0/status)
    if bluetoothctl show | grep -q 'Powered: yes'; then
        bt=on
        dev=$(bluetoothctl devices Connected | cut -d' ' -f3-)
        [ -n "$dev" ] && bt="on ($dev)"
    else
        bt=off
    fi
    echo "BAT $bat% $state | BT $bt | $(date +'%Y-%m-%d %X')"
    sleep 5
done
