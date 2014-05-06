helm-mu
=======

Helm sources for searching emails and contacts using mu and mu4e.  Mu is an indexer for maildirs and mu4e is a mutt-like MUA for Emacs build on top of mu.  Mu is highly efficient making it possible to get instant results even for huge maildirs.  It also provides search operators similar to Google mail, e.g:

    from:Peter to:Anne flag:attach search term

## Install

Helm-mu requires a fully configured mu4e setup and the latest version of mu (version from Sept 27 2013 or later).

Copy `helm-mu.el` to a directory in your `load-path`.  And add the following to your init file:

    (require 'helm-mu)

Alternatively, you can use the autoload facility:

    (autoload 'helm-mu "helm-mu" "" t)
    (autoload 'helm-mu-contacts "helm-mu" "" t)

To run mu, helm-mu uses the function `start-process-shell-command`.  It assumes that the shell called by that function is compatible with the Bourne shell (e.g., bash).  If your shell is incompatible, the mu command may not work.

GNU sed is used to do some filtering of the results returned by mu.  GNU sed is standard on Linux but OSX users may have to install it since the pre-installed BSD sed has different command line options.

Some things that can be configured:

- `helm-mu-default-search-string`
- `helm-mu-contacts-name-colwidth`
- `helm-mu-contacts-name-replace`
- `helm-mu-contacts-after`
- `helm-mu-contacts-personal`

Consult the documentation in Emacs or the source code for explanations of these variables.

## Usage

To search for emails use `helm-mu`.  When you would like to read an email without finishing the helm session, you can select the email and press Ctrl-z.  This will split the screen horizontally and show the email in the new window while keeping the search results in the other.  Alternatively, you can open the email using the enter key and return to the helm session using the command `helm-resume`.

To search for contacts use `helm-mu-contacts`.  Note that search terms are interpreted differently by `helm-mu-contacts` than by `helm-mu`.  `helm-mu` assumes that the search terms are complete words, i.e., that they are surrounded by white spaces or punctuation.  So if you search for `jo` it will only return emails in which `jo` occurs as a word.  In contrast to that, `helm-mu-contacts` will return all contacts in which `jo` occurs as a substring.  `helm-mu-contacts` uses the grep tool for searching.  That means that any regular expression supported by grep can be used when searching for contacts.
