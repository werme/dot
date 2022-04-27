
HYPHEN_INSENSITIVE="true"

. $HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
. $HOME/.zsh/zsh-history-substring-search/zsh-history-substring-search.zsh
. $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

autoload -Uz compinit && compinit

fpath+=$HOME/.zsh/pure
autoload -U promptinit; promptinit
prompt pure

bindkey -e

alias g="git"
alias ..="cd .."
alias ...="cd ../.."
alias vim="nvim"
alias v="nvim"
alias j="z"
alias o="open"
alias ls="exa"
alias lst="exa --all --tree --ignore-glob='node_modules'"
alias cap="ffmpeg -framerate 10 -s 640x480 -f avfoundation -i \"1\" -vframes 1 -f image2pipe - | jp2a - -f"

alias fishconfig="v ~/.config/fish/config.fish"
alias zshconfig="v ~/.zshrc"
alias vimconfig="v ~/.config/nvim/init.vim"
alias tmuxconfig="v ~/.tmux.conf"
alias alacrittyconfig="v ~/.config/alacritty/alacritty.yml"
alias gitconfig="v ~/.gitconfig"
alias tmux="env TERM=screen-256color tmux -u"
alias emacs="env TERM=screen-256color ~/dev/emacs/nextstep/Emacs.app/Contents/MacOS/Emacs"

export EDITOR="nvim"
export FZF_DEFAULT_COMMAND="fd --hidden --exclude={.git,node_modules} --type f"
export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export TZ="europe/Stockholm"

export PATH="/usr/local/sbin:$PATH"

set PATH ~/.bin $PATH

export _Z_CMD="j"
. $HOME/dev/z/z.sh

# . $(brew --prefix asdf)/asdf.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

eval "$(fnm env --multi)"

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

. ~/.zshrc.work

# fnm
export PATH=/Users/olle/.fnm:$PATH
eval "`fnm env`"
