#!/bin/zsh

emulate zsh

#
# Check environment
#
if [[ -z $atc_profile_loaded ]]; then
    echo "Eep!  Your zprofile isn't loaded"
    if [[ -r ~/sys/dotfiles/zprofile ]]; then
        if [[ ! -r ~/.zprofile ]]; then
            echo "You forgot to link .zprofile in from sys/dotfiles"
        elif [[ ~/.zprofile -ef ~/sys/dotfiles/zprofile ]]; then
            echo -n "Odd.  .zprofile exists and is correct.  "
            echo "Try logging out and logging back in"
        else
            echo "You have the wrong .zprofile.  Link in sys/dotfiles/.zprofile"
        fi
        echo "Sourcing sys/dotfiles/.zprofile"
        source ~/sys/dotfiles/zprofile
    else
        if [[ ! -d ~/sys/dotfiles ]]; then
            echo "You appear to be missing sys/dotfiles.  Bad things shall befall you"
        else
            echo "Odd.  You have sys/dotfiles, but no sys/dotfiles/zprofile"
        fi
    fi
fi

#
# Options
#
setopt no_hup                   # Don't HUP background processes on exit
setopt csh_null_glob            # Be happy if at least one glob expands
setopt no_beep                  # Be quiet
setopt correct                  # Enable command spelling correction
setopt no_short_loops           # No syntactic monstrosities
disable test [                  # Bash syntax

#
# History
#
HISTSIZE=1000                   # History lines to store in memory
SAVEHIST=1000                   # History lines to save to disk
HISTFILE=~/.history             # File to save history to
setopt append_history           # Append instead of replacing history
setopt inc_append_history       # .. do so as commands are entered
setopt extended_history         # Keep timestamps on history entries
setopt hist_ignore_dups         # Remove repeated commands from history
setopt hist_reduce_blanks       # Reformat whitespace in history

#
# Bindings
#
bindkey -e                      # Default to Emacs-like bindings

zle -N parent-dir
parent-dir() {
    local cursor=$CURSOR        # Save cursor position
    local buffer=$BUFFER        # Save buffer contents
    BUFFER=                     # Clear edit buffer
    zle -R                      # Redraw the now empty input line
    local i cmd=..              # Consume numeric argument
    for (( i = 1 ; i < NUMERIC ; i++ )); do
        cmd=$cmd/..
    done
    print "cd $cmd"             # So it's clear what's happening from scrollback
    cd $cmd
    # XXX reset-prompt doesn't work on zsh 4.0.x
    zle reset-prompt -N         # Redraw the prompt itself
    BUFFER=$buffer              # Restore the buffer contents
    CURSOR=$cursor              # And move the cursor back to where it was
}
bindkey '\C-u' parent-dir

#
# Line editing
#
WORDCHARS=${WORDCHARS//['\/.&']}        # Be more allowing with word skipping
# The single quotes above are a workaround for zsh 4.0 compatibility

#
# Customize prompt
#
autoload -U colors; colors      # Get control sequences for standard colors
if [[ -z $PROMPTFLUFF &&
            (-n $SSH_CONNECTION ||
                $name == 'athena') ]]; then  # Check ssh or Athena
    PROMPTFLUFF=$name
fi
PROMPT="%{${fg[white]}%}${PROMPTFLUFF+${PROMPTFLUFF}:}\
%{${fg_bold[green]}%}%16<..<%2~%<<%{${fg_no_bold[default]}%}\
%(!.%{${fg[red]}%}#%{${fg[default]}%}.>) "
RPROMPT="%B%{%(0?..${fg_bold[red]})%}%?\
%{%(0?..${fg_no_bold[default]})%}%b %*"

#
# Pretty-print return status
#
precmd_prettyreturn() {
    # Update psvar[0] to reflect pretty-printed output status
    local pretty=$result
    if (( pretty > 128 )); then
        pretty=${signals[$(( pretty - 128 + 1 ))]-$pretty}
    elif (( pretty == 127 )); then
        # Command not found
        pretty=":("
    elif (( pretty == 126 )); then
        # Command found, but permission denied
        pretty="XP"
    fi
    psvar[0]=$pretty
}
# Replace return status in prompts to use pretty-printing
PROMPT=${PROMPT//\%\?/\%v}
RPROMPT=${RPROMPT//\%\?/\%v}

#
# Convenience alises
#
alias psg="ps auxww 1 | sed '1q' ; ps auxww | grep -v grep | grep -i"
if [[ -n $ATC_USE_EMACS ]]; then
    alias emacs=$ATC_USE_EMACS
fi

#
# Directory listing
#
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
                ZLS_COLORS="$LS_COLORS"
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

    alias ls="$ls"
}
setupls; unfunction setupls

truncatedls() {
    # Print at most 10 lines of ls output
    # Note that this depends on the ls alias above for default formatting
    local lines="`ls -C $*`"
    integer nlines=$(( `wc -l <<< $lines` - 1 ))
    if (( nlines > 10 )); then
        head -n 9 <<< $lines
        print -- .. $(( nlines - 9 )) more lines ..
    else
        print -nr -- $lines
    fi
}

#
# Change directory helper
#
chpwd() {
    screentitle
    # List the new directory on change
    [[ -t 1 && -t 0 ]] || return
    truncatedls
}

#
# Screen title support
#
simplifydir() {
    local dir=$1

    # Replace home dir with ~
    dir=${dir/#${HOME}/'~'}
    # Replace other home dirs with ~username (not quite correct, but
    # generally works)
    dir=${dir/#\/home\//'~'}

    # Reverse
    local -a xsplit
    for c in ${(s:/:)dir}; do
        xsplit=($c $xsplit)
    done

    # Rejoin
    echo ${(j: :)xsplit}
}

screentitle() {
    # Set the title of the screen window or terminal to $1
    local title
    if [[ -n $1 ]]; then
        title="$1"
    elif [[ $TERM == screen ]]; then
        title="${ZSH_NAME}"
    else
        title="[$(simplifydir $PWD)]"
    fi
    if [[ $TERM == screen ]]; then
        print -n -- "\ek${title}\e\\"
    elif [[ $TERM == (*xterm*|rxvt|(dt|k|E)term) ]]; then
        print -n -- "\e]0;${title}\a"
    fi
}

preexec() {
    # Set the title to the current command, intelligently
    setopt local_options extended_glob
    typeset -a line
    line=(${(z)2})
    local cmd foo
    if [[ $line[1] == fg || $line[1] == %* ]]; then
        local jobid=${line[(r)%*]}
        if [[ -z $jobid ]]; then jobid=%+; fi
        jobs $jobid | read jobid foo
        line=(${(z)${(e):-\$jobtexts$jobid}})
    fi
    cmd=${line[(r)^(*=*|sudo)]:t}
    screentitle $cmd
}

precmd_screentitle() {
    # Return the title to the default after the command is done
    screentitle
}

#
# Chain shell-special functions
#
precmds=(${functions[(I)precmd_*]})
precmd() {
    # This chains to all functions of the form precmd_*, $result is
    # set to $?
    local result=$?
    local x
    for x in $precmds; do
        $x
    done
}

# Use completion cache
zstyle ':completion::complete:*' use-cache 1

# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete
zstyle ':completion:*' completions 1
zstyle ':completion:*' format '-- %d --'
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list '' 'l:|=* r:|=*'
zstyle ':completion:*' substitute 1
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/home/amthrax/.zshrc'

autoload -U compinit
compinit
# End of lines added by compinstall
