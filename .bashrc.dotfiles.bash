# Add:
# . "$HOME/.bashrc.dotfiles.bash"
# to your .bashrc file.

HISTSIZE=100000
HISTFILESIZE=200000

alias dotfiles='/usr/bin/git --git-dir="${HOME}/.dotfiles/" --work-tree="${HOME}"'

if [ -d "$HOME/go/bin" ] ; then
    PATH="$HOME/go/bin:$PATH"
fi

if [ -d "$HOME/.cargo/bin" ] ; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

source /usr/share/bash-completion/completions/git
__git_complete dotfiles __git_main

# pnpm
export PNPM_HOME="/home/jpuente/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64
export PATH=$JAVA_HOME/bin:$PATH

