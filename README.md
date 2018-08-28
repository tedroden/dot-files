# Ted Roden's various dotfiles

This is my collection of dotfiles. I like to use a lot of different computers and his helps keep me sane on all of them.

They *should* have nothing personal, so you can just install and go. If you need personal stuff, the emacs config supports custom.el (for customize mode) and personal.el (for whatever you need that no one else would want). Just put those in the directory and it'll load those. Personally, I store those files in dropbox and `ln -s` them to my (newly created) `~/.emacs.d`

## .emacs

This sets up some sane (IMO) defaults and installs useful packages.

It allows for two special files:

1. `custom.el`: will store things set with `custom`
2. `personal.el`: We'll load this if it exists for things that don't belong in git. 

Let me know what you think.

The first time you install this, it will take a long time. Let me know if something doesn't work!

*TODO*: Why doesn't magit know that this is a git project when cloned to ~/.emacs.d/

## .xinitrc

This is pretty basic, probably not usefull to anyone else.

We start `xmodmap` and `cinnamon`

## .xmodmap

This if for my [Chromebook Pixel](http://www.amazon.com/gp/product/B00BM7Y7DQ/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B00BM7Y7DQ&linkCode=as2&tag=tedrodcom-20&linkId=SICDVA2IRDFKLAU3). All it does is change the `search` key into a control key.

## .zshrc

This is basically the stock [`oh-my-zsh`](https://github.com/robbyrussell/oh-my-zsh/) with `$GOPATH` added.


## install.sh

This will install the files (it'll write over your existing ones probably). So be careful.

- TODO: make sure it doesn't write over existing files.
