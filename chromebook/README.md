# Chromebook

 - I've got a chromebook pixel (2013).
 - I run arch linux on it
 - here are some of the settings that it needs 


## Trackpad

To get it working with some sane defaults:

`/etc/X11/xorg.conf.d/50-synaptics.conf`

```
Section "InputClass"
	Identifier "touchpad"
	Driver "synaptics"
	MatchIsTouchpad "on"
	Option "RTCornerButton" "0" # This doesn't work
	Option "RBCornerButton" "0" # This doesn't work
EndSection

```

In order to stop it from doing a `right click` when I click on the right side the trackpad:

```
synclient RightButtonAreaLeft=0
synclient RightButtonAreaTop=0
```



