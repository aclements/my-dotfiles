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

#
# Degrade to an alternate TERM (e.g., when ssh'ing)
#
degradeterm() {
    local alts
    alts=($TERM)
    while (( $#alts )); do
        for alt in $alts; do
            if tput -T$alt init >& /dev/null; then
                TERM=$alt
                return
            fi
        done
        if [[ $alt == rxvt-unicode-256color ]]; then
            alts=(rxvt-256color)
        elif [[ $alt == rxvt-256color ]]; then
            alts=(rxvt-color)
        elif [[ $alt == rxvt* ]]; then
            alts=(vt102)
        fi
    done
    TERM=vt100
}
degradeterm

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
for TRY in emacs24 emacs23 emacs22 emacs21; do
    if whence -p $TRY &> /dev/null; then
        if [[ ! `whence -p $TRY` -ef `whence -p emacs` ]]; then
            export ATC_USE_EMACS=$TRY
            export EDITOR=$TRY
            export VISUAL="$TRY -nw"
        fi
        break
    fi
done
unset $TRY

# GCC 4.9 and up support color, but disable it unless GCC_COLOR is set
export GCC_COLOR=

setupgrep() {
    local foo="`grep --help`"
    if [[ -z ${foo:#*--color*} ]]; then
        export ATC_GREP_COLOR=1
    fi
}
setupgrep; unfunction setupgrep

setupls() {
    # XXX This is all rather painful.  Add caching.
    local ls lsver foundls dircolors dcver
    # The complexity of this is necessary because some systems (*cough*
    # Athena) have ancient versions of gls floating around
    for ls in `whence -ap gls ls`; do
        # The / is for BSD ls, which ignores --version (in fact, there
        # is no way to ask its version), to lock in on a directory I
        # know is local and small
        lsver="`command $ls --version / 2> /dev/null`"
        if [[ $lsver == *Stallman* ]]; then
            foundls=1
            lsver=${${=lsver}[3]}
            break
        fi
    done
    ls="command $ls"            # Just to be sure

    if (( foundls )); then
        # Find a dircolors that matches the version of ls
        for dircolors in `whence -p gdircolors dircolors`; do
            dcver="`command $dircolors --version`"
            if [[ $lsver == ${${=dcver}[3]} ]];  then
                # We have a winner
                eval `command $dircolors -b`
                export ZLS_COLORS="$LS_COLORS"
                unset LSCOLORS
                ls="$ls --color"
                break
            fi
        done
    else
        # Didn't find a satisfactory ls, fall back to what is
        # hopefully BSD ls.  This isn't quite right, because this
        # could also be an old version of GNU ls.
        ls="ls -G"
    fi

    # Show symbols after file names
    ls="$ls -F"

    export LS="$ls"
}
setupls; unfunction setupls

# Some systems (*cough* Athena) have wacky $HOME semantics where you
# start in a directory that is your home directory, but isn't string
# equal to $HOME.  This causes the prompt to not do ~ unexpansion.
# The following fixes this.
if [[ $HOME -ef . && $HOME != $PWD ]]; then
    cd $HOME
fi

# Fix LANG on broken systems (Goobuntu)
if [[ -z $LANG ]]; then
  LANG=en_US.UTF-8
fi

export atc_profile_loaded=1

export zprofile_generation=1
