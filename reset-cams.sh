rmmod e1000e
sleep 3
modprobe e1000e
# if for some reason the interfaces don't come up as enp?s0 restart udev:
# /etc/init.d/udev restart
for i in {4..7};do ifconfig enp$i"s0" 192.168.$i.1 ;done
for i in {4..7};do ifconfig enp$i"s0" mtu 9000;done
/etc/init.d/dhcpd restart
