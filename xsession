#!/bin/zsh
# Amthrax's X init script
# Supports: awakening, drake

emulate zsh

source .zenv

# Window manager is set later based on host
WMANAGER=false

determine_domain() {
    DOMAIN=${HOST/.*/}
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
    local FBW="$HOME/etc/wallpaper/1280x1024/Michael Whelan - Dragon Lake.png"
    if [[ -e ~/etc/wallpaper/walld.py ]]; then
	python ~/etc/wallpaper/walld.py ~/etc/wallpaper/active/
	#Esetroot ~/etc/wallpaper/active/AzlanCropped_1024.png
    elif [[ -e $FBW ]]; then
	Esetroot $FBW
    fi

    # No beeps!
    xset b 0

    # Load my modmap
    if [[ -e ~/.xmodmap ]]; then xmodmap ~/.xmodmap; fi

    # Set up mouse
    if [[ $DOMAIN == "awakening" ]]; then xset m 3/1 4; fi

    # Figure out my terminal emulator
    if whence Terminal &> /dev/null; then
	if whence Terminal.wrapper &> /dev/null; then
	    XTERMCMD=Terminal.wrapper
	else
	    XTERMCMD=Terminal
	fi
    elif whence Eterm &> /dev/null; then
	XTERMCMD=Eterm
    elif whence aterm &> /dev/null; then
	XTERMCMD=aterm
    else
	XTERMCMD=xterm
    fi
    export XTERMCMD
}

start_fluxbox() {
    fluxbox &
    WMPID=$!
    echo "Fluxbox pid $WMPID"
    sleep 1  # Kluuuudge!
}

start_ion3() {
    ion3 &
    WMPID=$!
    echo "Ion3 pid $WMPID"
}

start_apps_and_daemons_drake() {
    # Apps
    if [[ $WMANAGER != ion3 ]]; then
	torsmo -d
    fi

    # Slit apps
    if [[ $WMANAGER != ion3 ]]; then
	wmacpi &
    fi

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

    xscreensaver -nosplash &
}

# Wait for window manager to exit gracefully
wait_for_wm() {
    WMRES=1
    while [[ $WMRES != 0 ]]; do
	wait $WMPID
	WMRES=$?
	if [[ $WMRES != 0 ]]; then
	    echo "Window manager crashed with $WMRES, restarting it"
	    start_$WMANAGER
	fi
    done
}


# SSH agent must be done before anything else because of environment
# changes
if [[ -z $SSH_AGENT_PID ]]; then
    eval `ssh-agent -s`
fi

# Adapt to my environment and adapt my environment to me
determine_domain
if [[ $DOMAIN == drake ]]; then
    WMANAGER=ion3
else
    WMANAGER=fluxbox
fi
set_settings

# Start D-BUS so XFCE Terminal instances can share a process
if [[ $XTERMCMD == Terminal* && -z $DBUS_SESSION_BUS_ADDRESS ]] && \
    whence dbus-launch &> /dev/null; then
    eval `dbus-launch --sh-syntax --exit-with-session`
fi

# Start the world
start_$WMANAGER
start_apps_and_daemons
wait_for_wm
