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
    # xwininfo -root | awk '/Width:/ {W=$2}; /Height:/ {H=$2}; END {print W "x" H}'
    if [[ -e ~/sys/bin/walld.py ]]; then
        python ~/sys/bin/walld.py ~/etc/wallpaper/active/
    elif [[ -e ~/etc/wallpaper/walld.py ]]; then
        python ~/etc/wallpaper/walld.py ~/etc/wallpaper/active/
    elif [[ -d ~/etc/wallpaper/active ]]; then
        local WPN=`find ~/etc/wallpaper/active -not -type d | wc -l`
        local FBW="$(find ~/etc/wallpaper/active -not -type d \
            | awk "BEGIN {n=int(rand()*9999)%$WPN}; NR-1==n {print \$0}")"
        if whence xsri &> /dev/null; then
            xsri $FBW
        elif whence Esetroot &> /dev/null; then
            Esetroot -center $FBW
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

    if [[ -e ~/.Xresources ]]; then xrdb -merge ~/.Xresources; fi

    # Figure out my terminal emulator
    for XTERMCMD in urxvtcd urxvt Eterm aterm xterm; do
        if whence $XTERMCMD &> /dev/null; then
            export XTERMCMD
            break
        fi
    done
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

start_xmonad() {
    xmonad &
    WMPID=$!
    echo "xmonad pid $WMPID"
}

emergency_live_stop() {
    # Gentoo and Debian will catch nicely if xsession dies too quickly,
    # but if the WM starts acting up later, invoke this
    echo "Starting an emergency xterm"
    xterm -T "Emergency xterm"
}

start_apps_and_daemons_drake() {
    # Apps
    if [[ $WMANAGER != ion3 && $WMANAGER != xmonad ]]; then
	torsmo -d
    fi

    # Slit apps
    if [[ $WMANAGER != ion3 && $WMANAGER != xmonad ]]; then
	wmacpi &
    fi

    # Daemons
    #tpb -d  # tpb is started by a global Xsession script

    #knetworkmanager &
    nm-applet &
    #xtoolwait kpowersave
    xfce4-power-manager &
    liferea --mainwindow-state=hidden &
    if ! ps awx -o command | grep ^python | grep -q ipchanged; then
        screen -d -m -S daemon python -i sys/bin/ipchanged
    fi
    screen -S daemon -X screen ~/tmi
#    screen -S daemon -X screen xrandrd
#    gnome-settings-daemon &
    # gnome-settings-daemon crashes rather often
    # https://bugs.launchpad.net/ubuntu/+source/gnome-desktop/+bug/447431
    (while xwininfo -root >& /dev/null; do gnome-settings-daemon --debug; done) &
    pidgin &
    emacs wiki2 &
    $XTERMCMD &
    $XTERMCMD -e awkscreen &

    # XXX Not really the right place for this
    killall afuse
    afuse -o mount_template="sshfs %r:/ %m -o reconnect -o cache_timeout=1 -o workaround=rename -o transform_symlinks -o idmap=user" -o unmount_template="fusermount -u -z %m" ~/ssh
}

start_apps_and_daemons_awakening() {
    # X clients
    $XTERMCMD &
    #xmms &
    #gkrellm &
    #xwrits typetime=30 breaktime=1 beep clock after=5 flashtime=:1 \
    #    after=10 flashtim e=:.03 &

    # Daemons
    if [[ $WMANAGER == fluxbox ]]; then
        bbkeys -w -t &
    fi
    #esd -nobeeps -d /dev/dsp1 &
}

start_apps_and_daemons() {
    # Compositing
    if [[ $WMANAGER == xmonad ]]; then
        # XXX -f -I 0.1 -O 0.1 to fade in and out, but some layouts
        # cause this just when switching between windows
        xcompmgr &
    fi

    # System tray
    if [[ $WMANAGER == ion3 ]]; then
        # Translate GTK tray icons into WindowMaker dockapps
        docker &
    elif [[ $WMANAGER == xmonad ]]; then
#        stalonetray -bg black --icon-size 16 --window-layer top \
#            --geometry 1x1-0+0 --grow-gravity E --icon-gravity NW &
        # --dbg-level 9 >& ~/.stalonetraylog &
        # stalonetray 0.7.6 fails to absorb existing KDE icons.
        # Better, we could wait for the appropriate log messages.
#        sleep 1
        # (June 2011) stalonetray finally gave up the ghost

        xtoolwait xfce4-panel
    fi

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
                    emergency_live_stop
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
for WMANAGER in xmonad ion3 fluxbox twm; do
    if which $WMANAGER > /dev/null; then
        break
    fi
done
set_settings

# Start D-BUS
if [[ -z $DBUS_SESSION_BUS_ADDRESS ]] && whence dbus-launch &> /dev/null; then
    eval `dbus-launch --sh-syntax --exit-with-session`
fi

# Start the world
start_$WMANAGER
start_apps_and_daemons
wait_for_wm
