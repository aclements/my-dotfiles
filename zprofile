# -*- shell-script -*-

#
# Discover architecture and hostname
#
sys=$(uname); sys=${(L)sys}
name=${HOST/.*/}
if [[ $HOST == "*.mit.edu" && $name != "awakening" ]]; then
    name=athena
fi

#
# Paths
#
typeset -U path manpath	# Uniquify entries in paths
export -TU LD_LIBRARY_PATH ld_library_path

for x in ~/bin ~/$sys/bin; do # Add bin directories to path
    [[ -r $x ]] && path=($x $path)
done
for x in ~/man ~/$sys/man; do # Add man directories to manpath
    [[ -r $x ]] && manpath=($x $manpath)
done
for x in ~/lib ~/$sys/lib; do # Add lib directories for ld
    [[ -r $x ]] && ld_library_path=($x $ld_library_path)
done
unset x

#     if [[ -r /usr/lib/j2se/1.4/ ]]; then # Java home?
# 	export JAVA_HOME=/usr/lib/j2se/1.4/
#     fi
#     if [[ -r /usr/lib/j2se/1.4/jre/lib/rt.jar ]]; then # JRT? (for Jikes)
# 	export CLASSPATH=.:/usr/lib/j2se/1.4/jre/lib/rt.jar
#     fi

#
# Plan 9 from User Space
#
if [[ -d ~/plan9 ]]; then
    export PLAN9=$HOME/plan9
    path=($path $PLAN9/bin)
fi

#
# General environment
#
export MORE=-sl
#export LESSCHARSET=latin1
export LESS="-R -h10"
export LESS="$LESS -M"
export LESS="$LESS \
-P?f%f:(pipe). ?e[EOF].%t ?pb(%pb\%).$ \
-PmFile\:?f%f:(pipe). ?e[EOF].%t ?m[%i/%m]. Line\:%lt/%L\
 ?pb(%pb\%).$ \
-PMFile\:?f%f:(pipe). ?e[EOF].%t ?m[%i/%m?x\:%x.]. Line\:%lt/%L\
 Byte\:%bt/%B. ?pb(%pb\%)."
export PAGER=less

export EDITOR=emacs
export VISUAL="emacs -nw"
if whence -p emacs21 &> /dev/null; then
    if [[ ! `whence -p emacs21` -ef `whence -p emacs` ]]; then
	export ATC_USE_EMACS21=1
	export EDITOR=emacs21
	export VISUAL="emacs21 -nw"
    fi
fi

setupgrep() {
    local foo="`grep --help`"
    if [[ -z ${foo:#*--color*} ]]; then
	export GREP_OPTIONS="--color=auto"
    fi
}
setupgrep; unfunction setupgrep

# Some systems (*cough* Athena) have wacky $HOME semantics where you
# start in a directory that is your home directory, but isn't string
# equal to $HOME.  This causes the prompt to not do ~ unexpansion.
# The following fixes this.
if [[ $HOME -ef . && $HOME != $PWD ]]; then
    cd $HOME
fi

# When at Google, do as the Goolers do
if [[ -f .zgoogle ]]; then
    source .zgoogle
fi

# When at StreamBase, do as the StreamBasers do
if [[ -f .zstreambase ]]; then
    source .zstreambase
fi

export atc_profile_loaded=1
