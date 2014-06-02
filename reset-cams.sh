rmmod e1000e
modprobe e1000e
for i in {4..7};do ifconfig enp$i"s0" 192.168.$i.1 ;done
for i in {4..7};do ifconfig enp$i"s0" mtu 9000;done
/etc/init.d/dhcpd restart
