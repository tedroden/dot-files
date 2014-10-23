# Ted Roden's .emacs.d directory

I've totally redone my .emacs.d stuff. 

This sets up some sane (IMO) defaults and installs useful packages.

It allows for two special files:

1. `custom.el`: will store things set with `custom`
2. `personal.el`: We'll load this if it exists for things that don't belong in git. 

Let me know what you think

TODO: only do the mac only stuff on a mac
TODO: check to see if a package is available before `package-install` ... if not, run `package-refresh-contents` 
TODO: Why doesn't magit know that this is a git project when cloned to ~/.emacs.d/
