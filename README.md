## dotfiles

### Usage

Use [GNU Stow](http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html) to manage the dotfiles.

### Test it up with docker

Inside dockerfile,
```
FROM ubuntu:14.04
MAINTAINER Xiao Jiang "showbufire@gmail.com"
ENV REFRESHED_AT 2015-08-16

RUN apt-get -yqq update
RUN apt-get -y install emacs git stow tmux

RUN useradd -m me && echo "me:lalala" | chpasswd && adduser me sudo
USER me

WORKDIR /home/me/
RUN git clone https://github.com/showbufire/dotfiles.git

WORKDIR /home/me/dotfiles
RUN git checkout -b dev origin/dev

RUN stow emacs git tmux bash

RUN echo 'if [ -f ~/.bashrc_custom ]; then\n\
    . ~/.bashrc_custom\n\
fi' >> ~/.bashrc

RUN . ~/.bashrc
```
