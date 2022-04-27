
alias g="git"
alias ..="cd .."
alias ...="cd ../.."
alias vim="nvim"
alias v="nvim"
alias o="open"
alias ls="exa"
alias lst="exa --all --tree --ignore-glob='node_modules'"

alias fishconfig="v ~/.config/fish/config.fish"
alias zshconfig="v ~/.zshrc"
alias vimconfig="v ~/.config/nvim/init.vim"
alias tmuxconfig="v ~/.tmux.conf"
alias gitconfig="v ~/.gitconfig"
alias tmux="env TERM=screen-256color tmux -u"

alias fd="fdfind"
export FZF_DEFAULT_COMMAND="fdfind --hidden --exclude={.git,node_modules} --type f"

export EDITOR="nvim"
export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export TZ="europe/Stockholm"

export _Z_CMD="j"
. $HOME/dev/z/z.sh


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# fnm
export PATH=/home/pi/.fnm:$PATH
[ -f ~/.fnm ] && eval "`fnm env --multi`"

[ -f ~/.bashrc.local ] && . ~/.bashrc.local

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
. "$HOME/.cargo/env"
