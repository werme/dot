# Path
set PATH /usr/local/bin $PATH
set PATH ~/.bin $PATH
set PATH ~/.local/bin $PATH

# Editor defaults
if set -q SSH_CONNECTION
  set -x EDITOR "nvim"
  set -x GIT_EDITOR "nvim"
else
  set -x EDITOR "zed"
  set -x GIT_EDITOR "zed --wait"
end

# Command rewrites and arg defaults
alias ls="eza -a --git-ignore -I '.git'"
alias tree="eza --tree -a --git-ignore -I '.git'"
# alias tmux="env TERM=screen-256color tmux -u"

# Shortcuts
alias v="$EDITOR"
alias g="git"
alias ..="cd .."
alias ...="cd ../.."
alias lst="tree"
alias l="tree"
alias o="open"
alias tx="tmux"
alias lg="lazygit"
alias c="claude"
alias goo="goose"
alias fv="fzf | xargs $EDITOR"

# Quick config access
alias fishconfig="$EDITOR ~/.config/fish/config.fish"
alias vimconfig="$EDITOR ~/.config/nvim/init.lua"

# Misc localization and timezone
set -x LC_ALL "en_US.UTF-8"
set -x LANG "en_US.UTF-8"
set -x LANGUAGE "en_US.UTF-8"
set -x TZ "Europe/Stockholm"

# z
set -U Z_CMD "j"

# fzf
set -x FZF_DEFAULT_COMMAND "fd --hidden --exclude={.git,node_modules} --type f"

# fnm
fnm env --use-on-cd | source

# Docker
[ -d "$HOME/.docker/bin" ] && set PATH $PATH "$HOME/.docker/bin"

# pnpm
set -gx PNPM_HOME "$HOME/Library/pnpm"
[ -d "$PNPM_HOME" ] && set -gx PATH "$PNPM_HOME" $PATH

# Bun
set -x BUN_INSTALL "$HOME/.bun"
[ -d "$BUN_INSTALL/bin" ] && set -x PATH $BUN_INSTALL/bin $PATH

# Rust
[ -d "$HOME/.cargo/bin" ] && set PATH $PATH "$HOME/.cargo/bin"

# Local config
[ -f ~/.fish.config.local ] &&source ~/.fish.config.local
