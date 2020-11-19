#!bin/sh

# Set the screen DPI
xrdb ~/.emacs.d/exwm/Xresource

gsettings set org.gnome.desktop.background picture-uri file:///home/netree/backgrounds/mountains.jpg

compton -b

exec dbus-launch --exit-with-session emacs -mm --debug-init
