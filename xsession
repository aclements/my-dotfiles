#!/bin/zsh
# Amthrax's X init script
# Supports: awakening, drake

emulate zsh

# Note that this depends on a number of variables that get set by
# .zprofile
if [[ -z $atc_profile_loaded ]]; then
    if [[ -r ~/sys/dotfiles/zprofile ]]; then
	# .zshrc will complain visibly about this
	source ~/sys/dotfiles/zprofile
    else
	echo "Profile not loaded.  Bad things are going to happen."
    fi
fi

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
    local FBW
    if [[ $DOMAIN == chimera ]]; then
	FBW="$HOME/etc/wallpaper/Oxygen_3840x1200.png"
    elif [[ $DOMAIN == fry ]]; then
        FBW="$HOME/etc/wallpaper/Country Stroll 2 3840x1200.png"
    else
	FBW="$HOME/etc/wallpaper/1280x1024/Michael Whelan - Dragon Lake.png"
    fi
    if [[ -e ~/etc/wallpaper/walld.py ]]; then
	python ~/etc/wallpaper/walld.py ~/etc/wallpaper/active/
    elif [[ -e ~/sys/bin/walld.py ]]; then
	python ~/sys/bin/walld.py ~/etc/wallpaper/active/
    elif [[ -e $FBW ]]; then
        if whence xsri &> /dev/null; then
            xsri $FBW
        elif whence Esetroot &> /dev/null; then
            Esetroot $FBW
        # XXX I forget how to do this with ImageMagick
#        elif whence display &> /dev/null; then
#            display -backdrop $FBW
        fi
    fi

    # No beeps!
    xset b 0

    # Load my modmap
    if [[ -e ~/.xmodmap ]]; then xmodmap ~/.xmodmap; fi

    # Set up mouse
    if [[ $DOMAIN == "awakening" ]]; then xset m 3/1 4; fi

    # Figure out my terminal emulator
    if whence xfce4-terminal &> /dev/null; then
        if whence xfce4-terminal.wrapper &> /dev/null; then
            XTERMCMD=xfce4-terminal.wrapper
        else
            XTERMCMD=xfce4-terminal
        fi
    elif whence Terminal &> /dev/null; then
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

emergency_live_stop() {
    # Gentoo and Debian will catch nicely if xsession dies too quickly,
    # but if the WM starts acting up later, invoke this
    echo "Starting an emergency xterm"
    xterm -T "Emergency xterm"
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
    #xmms &
    gkrellm &
    xwrits typetime=30 breaktime=1 beep clock after=5 flashtime=:1 \
        after=10 flashtim e=:.03 &

    # Daemons
    if [[ $WMANAGER == fluxbox ]]; then
        bbkeys -w -t &
    fi
    #esd -nobeeps -d /dev/dsp1 &
}

start_apps_and_daemons() {
    dispatch start_apps_and_daemons

    xscreensaver -nosplash &
}

# Wait for window manager to exit gracefully
wait_for_wm() {
    WMRES=1
    WMRESTARTS=0
    while [[ $WMRES != 0 ]]; do
        WMSTART=`date +%s`
	wait $WMPID
	WMRES=$?
        WMEND=`date +%s`
	if [[ $WMRES != 0 ]]; then
            echo "Window manager crashed with $WMRES"
            if (( WMEND - WMSTART < 10 )); then
                if (( WMRESTARTS == 0 )); then
                    # Let this fall through.  Wrapping script will take
                    # care of quick failures
                    return
                else
                    echo "WM lasted fewer than 10 seconds, not restarting it"
                    emergency_stop
                    return
                fi
            else
                echo "Restarting it"
                start_$WMANAGER
                WMRESTARTS=$((WMRESTARTS + 1))
            fi
	fi
    done
}

# SSH agent must be done before anything else because of environment
# changes
if [[ -z $SSH_AGENT_PID ]]; then
    ssh-agent -s > ~/.ssh/agent-env-$HOST
    source ~/.ssh/agent-env-$HOST
else
    # Probably started by display manager scripts
    echo "export SSH_AUTH_SOCK=$SSH_AUTH_SOCK" > .ssh/agent-env-$HOST
    echo "export SSH_AGENT_PID=$SSH_AGENT_PID" >> .ssh/agent-env-$HOST
    echo "echo Agent PID $SSH_AGENT_PID" >> .ssh/agent-env-$HOST
fi

# Adapt to my environment and adapt my environment to me
determine_domain
if [[ $DOMAIN == drake || $DOMAIN == chimera || $DOMAIN == awakening || $DOMAIN == fry ]]; then
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
