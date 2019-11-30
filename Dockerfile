FROM ubuntu:latest
RUN apt-get update && \
    apt-get install -y software-properties-common locales

RUN locale-gen en_US.UTF-8 && \
    apt-get install -y neovim httpie ssh git curl gnupg2 apt-transport-https make binutils bison gcc build-essential ca-certificates

RUN apt-add-repository ppa:fish-shell/release-3 && \
    apt-get update && \
    apt-get install -y fish

RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add - && \
    add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" && \
    apt-get update && \
    apt-get install -y docker-ce

RUN useradd -m olle && \
    echo "olle ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers && \
    chsh -s /usr/bin/fish olle 

ADD . /home/olle/dev/dot

USER root
RUN chown -R olle:olle /home/olle

USER olle
RUN sh ~/dev/dot/install.sh

EXPOSE 22

USER root
CMD ["/home/olle/dev/dot/start.sh"]
