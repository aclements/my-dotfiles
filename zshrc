#!/bin/zsh

emulate zsh

#
# Discover architecture and hostname
#
sys=$(uname | tr "[:upper:]" "[:lower:]")
name=$(echo $HOST | sed 's/\..*//')
if [[ $HOST == "*.mit.edu" && $name != "awakening" ]]; then
    name=athena
fi

#
# Paths
#
typeset -U path manpath		# Uniquify entries in paths
export -TU LD_LIBRARY_PATH ld_library_path

for x in ~/bin ~/$sys/bin; do	# Add bin directories to path
    if [[ -r $x ]] path=($x $path)
done
for x in ~/man ~/$sys/man; do	# Add man directories to manpath
    if [[ -r $x ]] manpath=($x $manpath)
done
for x in ~/lib ~/$sys/lib; do	# Add lib directories for ld
    if [[ -r $x ]] ld_library_path=($x $ld_library_path)
done
unset x

if [[ -r /usr/lib/j2se/1.4/ ]]; then	# Java home?
    export JAVA_HOME=/usr/lib/j2se/1.4/
fi
if [[ -r /usr/lib/j2se/1.4/jre/lib/rt.jar ]]; then	# JRT? (for Jikes)
#    export CLASSPATH=.:/usr/lib/j2se/1.4/jre/lib/rt.jar
fi

#
# Options
#
setopt no_hup			# Don't HUP background processes on exit
setopt csh_null_glob		# Be happy if at least one glob expands
setopt no_beep			# Be quiet
setopt correct			# Enable command spelling correction
setopt no_short_loops		# No syntactic monstrosities
disable test [			# Bash syntax

#
# History
#
HISTSIZE=1000			# History lines to store in memory
SAVEHIST=1000			# History lines to save to disk
HISTFILE=~/.history		# File to save history to
setopt append_history		# Append instead of replacing history
setopt inc_append_history	# .. do so as commands are entered
setopt extended_history		# Keep timestamps on history entries
setopt hist_ignore_dups		# Remove repeated commands from history
setopt hist_reduce_blanks	# Reformat whitespace in history

#
# General environment
#
export MORE=-sl
export LESSCHARSET=latin1
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

if grep --help | grep -q -- --color; then
    export GREP_OPTIONS="--color=auto"
fi

#
# Bindings
#
bindkey -e			# Default to Emacs-like bindings
bindkey -rp '\C-x'		# Nuke C-x prefix
bindkey -s '\C-x' "cd ..\n"

#
# Line editing
#
WORDCHARS=${WORDCHARS//['\/.&']}	# Be more allowing with word skipping
# The single quotes above are a workaround for zsh 4.0 compatibility

#
# Customize prompt
#
autoload -U colors; colors	# Get control sequences for standard colors
if [[ -n $SSH_CONNECTION || $name == 'athena' ]]; then	# Check ssh or Athena
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
	pretty=${signals[$(( pretty - 128 + 1 ))]}
    fi
    psvar[0]=$pretty
}
# Replace return status in prompts to use pretty-printing
PROMPT=${PROMPT//\%\?/\%v}
RPROMPT=${RPROMPT//\%\?/\%v}

#
# Directory listing
#
setupls() {
    local dircolors ls
    # Find programs
    if whence -p dircolors &> /dev/null; then
	dircolors=dircolors
    elif whence -p gdircolors &> /dev/null; then
	dircolors=gdircolors
    fi
    if whence gls &> /dev/null; then
	ls=gls
    else
	ls=ls
    fi

    # Did I get the GNU version?
    if $ls --version | grep -q Stallman; then
	# Use programs
	if [[ -n $dircolors ]]; then
	    eval `$dircolors -b`       	# Set up ls color environment
	    ZLS_COLORS=$LS_COLORS	# Use same coloring for tab completion
	    ls="$ls --color"
	fi
    else
	# Only BSD version is available
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
	head -9 <<< $lines
	print -- .. $(( nlines - 9 )) more lines ..
    else
	print -nr -- $lines
    fi
}

#
# Change directory helper
#
chpwd() {
    # List the new directory on change
    [[ -t 1 ]] || return
    truncatedls
}

#
# Screen title support
#
screentitle() {
    # Set the title of the screen window or terminal to $1
    local title
    if [[ -n $1 ]]; then
	title=$1
    else
	title=${ZSH_NAME}
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

autoload -Uz compinit
compinit
# End of lines added by compinstall
