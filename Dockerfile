FROM ubuntu:latest
RUN apt-get update
RUN apt-get install -y software-properties-common locales

RUN locale-gen en_US.UTF-8 && \
    apt-get install -y neovim httpie ssh git curl gnupg2 apt-transport-https make binutils bison gcc build-essential ca-certificates

RUN apt-add-repository ppa:fish-shell/release-3 && \
    apt-get update && \
    apt-get install -y fish

RUN useradd -m olle && \
    echo "olle ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers && \
    chsh -s /usr/bin/fish olle 
# Replace shell with bash so we can source files
# RUN rm /bin/sh && ln -s /bin/zsh /bin/sh

RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add - && \
    add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" && \
    apt-get update && apt-get install -y docker-ce

# ADD files/ssh/main-id_rsa.pub /home/ryan/.ssh/authorized_keys

# RUN chown -R ryan:ryan /home/ryan/.ssh

# Install Dotfiles
ADD . /home/olle/dev/dotfiles
USER olle
RUN cd ~/dev/dotfiles && sh install.sh

USER root
RUN chown -R olle:olle /home/olle
USER olle

# RUN cd /tmp && git clone https://github.com/tmux/tmux.git && cd tmux && \
#     sudo apt-get install -y libevent-dev automake pkg-config libncurses5-dev && \
#     git reset --hard 2.0 && \
#     curl https://gist.githubusercontent.com/JohnMorales/0579990993f6dec19e83/raw/75b073e85f3d539ed24907f1615d9e0fa3e303f4/tmux-24.diff | git apply && \
#     ./autogen.sh && ./configure && make && sudo make install && \
#     rm -rf /tmp/tmux
 
# RUN mkdir /home/olle/.config && \
#     ln -s /home/olle/.dotfiles/nvim /home/olle/.config/nvim && \
#     ln -s /home/olle/.dotfiles/nvimrc /home/ryan/.config/nvim/init.vim && \
#     curl -fLo ~/.nvim/autoload/plug.vim --create-dirs \
#     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim && \
#     nvim +PlugInstall +qall --headless


EXPOSE 22
# VOLUME /src

# ADD start.sh /
# 
# USER root
# CMD ["/start.sh"]
