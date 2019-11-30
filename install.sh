#!/bin/bash

stow -R -t ~ git tmux
stow -R -t ~/.config/fish fish
stow -R -t ~/.config/nvim nvim

# vim-plug
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
