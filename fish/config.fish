# Path
set PATH /usr/local/bin $PATH
set PATH ~/bin $PATH
set PATH ~/.bin $PATH
set PATH ~/.local/bin $PATH

set -x GUI 1  # Default value
if set -q TMUX && test (tmux display -p "#{window_width}") -lt 100
    set -x GUI 0
else if not set -q TMUX && set -q SSH_CONNECTION
    set -x GUI 0
end

# Editor defaults
if test "$GUI" = "1"
  set -x EDITOR "zed"
  set -x GIT_EDITOR "zed --wait"
else
  set -x EDITOR "nvim"
  set -x GIT_EDITOR "nvim"
end

# Command rewrites and arg defaults
alias ls="eza -a"
alias tree="eza --tree -a -I '.git|node_modules'"
alias tmux="env TERM=screen-256color tmux -u"

# Shortcuts
alias v="$EDITOR"
alias g="git"
alias nv="nvim"
alias ..="cd .."
alias ...="cd ../.."
alias lst="tree"
alias glst="tree --git-ignore"
alias l="tree"
alias jj="cd ~"
alias o="open"
alias tx="tmux"
alias lg="lazygit"
alias c="SHELL=/bin/bash claude"
alias co="OPENAI_API_KEY=$OPENAI_API_KEY___ZED codex"
alias fv="fzf | xargs $EDITOR"
alias ai="aichat"
alias px="perplexity"

# Quick config access (single quotes needed to resolve env var lazily)
alias fishconfig='$EDITOR ~/.config/fish/config.fish'
alias vimconfig='$EDITOR ~/.config/nvim/init.lua'
alias gitconfig='$EDITOR ~/.gitconfig'
alias tmuxconfig='$EDITOR ~/.tmux.conf'

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
