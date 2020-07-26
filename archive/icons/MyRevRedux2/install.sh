#!/bin/sh

# Cursor dirs variables
rr_dir=$HOME/.icons/RevRedux2
xc_dir=$HOME/.icons

op=`kdialog --title "RevRedux 2 Installer" --radiolist "What do you want to do?" 1 "Install for KDE 3.2" on \
2 "Install for X" off 3 "Uninstall" off`

if [ $? = 0 ]; then

#------- KDE Install

	if [ $op = 1 ]; then

		if [ -d $rr_dir ]; then
			rm -rf $rr_dir
			mkdir -p $rr_dir
		else
			mkdir -p $rr_dir
		fi

		cp -r cursors $rr_dir/
		cp index.theme $rr_dir/
		kdialog --title "RevRedux 2 Installer" --msgbox "Install for KDE 3.2 done!\nUse KControl to apply it."

	fi


#------- X Install

	if [ $op = 2 ]; then

		if [ -d $xc_dir/default ]; then
			mv -f $xc_dir/default/index.theme $xc_dir/default/index.theme.bak
		else
			mkdir -p $xc_dir/default
		fi

cat >$xc_dir/default/index.theme <<EOF
[Icon Theme]
Inherits=RevRedux2
EOF

		if [ -d $rr_dir ]; then
			rm -rf $rr_dir
			mkdir -p $rr_dir
		else
			mkdir -p $rr_dir
		fi

		cp -r cursors $rr_dir/
		kdialog --title "RevRedux 2 Installer" --msgbox "Install for X done!\nLogout to finish the apply."

	fi


#------- Uninstall

	if [ $op = 3 ]; then

		rm -rf $rr_dir
		mv -f $xc_dir/default/index.theme.bak $xc_dir/default/index.theme
		kdialog --title "RevRedux 2 Installer" --msgbox "Uninstall done!"
		
	fi

fi