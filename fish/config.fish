if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

alias g="git"
alias ..="cd .."
alias ...="cd ../.."
alias vim="nvim"
alias v="nvim"
alias j="z"
alias o="open"
alias ls="exa"
alias lst="exa --all --tree --ignore-glob='node_modules'"

alias fishconfig="v ~/.config/fish/config.fish"
alias vimconfig="v ~/.config/nvim/init.vim"
alias tmuxconfig="v ~/.tmux.conf"
alias alacrittyconfig="v ~/.config/alacritty/alacritty.yml"
alias tmux="env TERM=screen-256color tmux -u"
alias tx="tmux"
alias emacs="env TERM=screen-256color emacs"
alias vs="v -S ~/.vim/sessions/Prev.vim"

set -x EDITOR "nvim"
set -x FZF_DEFAULT_COMMAND "fd --hidden --exclude={.git,node_modules} --type f"
set -x LC_ALL "en_US.UTF-8"
set -x LANG "en_US.UTF-8"
set -x LANGUAGE "en_US.UTF-8"

