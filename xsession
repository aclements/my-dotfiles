#!/usr/bin/zsh
# Amthrax's X init script
# Supports: awakening, drake

emulate zsh

determine_domain() {
    DOMAIN=$(echo $HOST | sed 's/\..*//')
}

dispatch() {
    local F=$1
    shift
    if ( whence ${F}_$DOMAIN > /dev/null ); then
	${F}_$DOMAIN $*
    else
	echo "Don't know how to $F for $DOMAIN"
    fi
}

set_settings() {
    # Set the wallpaper as early as possible to get it out of the way
    xsetroot -solid black
    local FBW="~/etc/wallpaper/1280x1024/Michael Whelan - Dragon Lake.png"
    if [[ -e ~/etc/wallpaper/walld.py ]]; then
	python ~/etc/wallpaper/walld.py ~/etc/wallpaper/active/
	#Esetroot ~/etc/wallpaper/active/AzlanCropped_1024.png
    elif [[ -e $FBW ]] then
	Esetroot $FBW
    fi

    # No beeps!
    xset b 0

    # Load my modmap
    if [[ -e ~/.xmodmap ]]; then xmodmap ~/.xmodmap; fi

    # Set up mouse
    if [[ DOMAIN == "awakening" ]]; then xset m 3/1 4; fi
}

start_wmanager() {
    fluxbox &
    WMPID=$!
    echo "Fluxbox pid $WMPID"
    sleep 1  # Kluuuudge!
}

start_apps_and_daemons_drake() {
    # Slit apps
    wmacpi &

    # Daemons
    #tpb -d  # tpb is started by a global Xsession script
}

start_apps_and_daemons_awakening() {
    # X clients
    Eterm &
    xmms &
    gkrellm &
    xwrits typetime=30 breaktime=1 beep clock after=5 flashtime=:1 \
	after=10 flashtim e=:.03 &

    # Daemons
    bbkeys -w -t &
    esd -nobeeps -d /dev/dsp1 &
}

start_apps_and_daemons() {
    dispatch start_apps_and_daemons

    xscreensaver &
}

# Wait for window manager to exit gracefully
wait_for_wm() {
    WMRES=1
    while [[ $WMRES != 0 ]]; do
	wait $WMPID
	WMRES=$?
	if [[ $WMRES != 0 ]]; then
	    echo "Window manager crashed with $WMRES, restarting it"
	    start_wmanager;
	fi;
    done
}


# SSH agent must be done before anything else because of environment
# changes
eval `ssh-agent -s`

determine_domain
set_settings
start_wmanager
start_apps_and_daemons
wait_for_wm
