helm-mu
=======

Helm sources for searching emails and contacts using mu and mu4e.  Mu is an indexer for maildirs and mu4e is a mutt-like MUA for Emacs build on top of mu.  Mu is highly efficient making it possible to get instant results even for huge maildirs.  It also provides search operators similar to Google mail, e.g:

    from:Peter to:Anne flag:attach search term

## Install

Helm-mu requires a fully configured mu4e setup and the latest version of mu (version from Sept 27 2013 or later).

Copy `helm-mu.el` to a directory in your `load-path`.  And add the following to your init file:

    (require 'helm-mu)

To run mu, helm-mu uses the elisp function `start-process-shell-command`.  It assumes that the shell called by that function is compatible with the Bourne shell (e.g. bash).  If your shell is incompatible, the mu command may not work.

GNU sed is used to do some filtering of the results returned by mu.  GNU sed is standard on Linux but OSX users may have to install it since the pre-installed BSD sed has different command line options.

To search for emails use `helm-mu` and to search for contacts use `helm-mu-contacts`.

Some things that can be configured:

- helm-mu-default-search-string
- helm-mu-contacts-name-colwidth
- helm-mu-contacts-name-replace
- helm-mu-contacts-after
- helm-mu-contacts-personal

Consult the documentation in Emacs or the source code to find explanations of these variables.

