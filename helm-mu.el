;;; helm-mu.el --- Helm sources for searching emails and contacts

;; Copyright (C) 2013 Titus von der Malsburg <malsburg@posteo.de>

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; URL: https://github.com/emacs-helm/helm-mu
;; Version: 1.0.0
;; Package-Requires: ((helm "1.5.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Helm sources for searching emails and contacts using mu and
;; mu4e. Mu is an indexer for maildirs and mu4e is a mutt-like MUA for
;; Emacs build on top of mu. Mu is highly efficient making it possible
;; to get instant results even for huge maildirs. It also provides
;; search operators similar to Google mail, e.g:
;;
;;     from:Peter to:Anne flag:attach search term

;;; Install:

;; Helm-mu requires a fully configured mu4e setup and the latest
;; version of mu (version from Sept 27 2013 or later).
;;
;; Copy helm-mu.el to a directory in your load-path.  And add the
;; following to your init file:
;;  
;;     (require 'helm-mu)
;;  
;; Alternatively, you can use the autoload facility:
;;
;;     (autoload 'helm-mu "helm-mu" "" t)
;;     (autoload 'helm-mu-contacts "helm-mu" "" t)
;;
;; To run mu, helm-mu uses the function `start-process-shell-command'.
;; It assumes that the shell called by that function is compatible
;; with the Bourne shell (e.g. bash).  If your shell is incompatible,
;; the mu command may not work.
;;
;; GNU sed is used to do some filtering of the results returned by
;; mu.  GNU sed is standard on Linux but OSX users may have to install
;; it since the pre-installed BSD sed has different command line
;; options.
;;
;; Some things that can be configured:
;;
;; - `helm-mu-default-search-string'
;; - `helm-mu-contacts-name-colwidth'
;; - `helm-mu-contacts-name-replace'
;; - `helm-mu-contacts-after'
;; - `helm-mu-contacts-personal'
;;
;; Consult the documentation in Emacs or the source code below for
;; explanations of these variables.

;;; Usage:

;; To search for emails use `helm-mu'.  When you would like to read an
;; email without finishing the helm session, you can select the email
;; and press Ctrl-z.  This will split the screen horizontally and show
;; the email in the new window while keeping the search results in the
;; other.  Alternatively, you can open the email using the enter key
;; and return to the helm session using the command `helm-resume'.
;;
;; To search for contacts use `helm-mu-contacts'.  Note that search
;; terms are interpreted differently by `helm-mu-contacts' than by
;; `helm-mu'.  `helm-mu' assumes that the search terms are complete
;; words, i.e., that they are surrounded by white spaces or
;; punctuation.  So if you search for "jo" it will only return emails
;; in which "jo" occurs as a word.  In contrast to that,
;; `helm-mu-contacts' will return all contacts in which "jo" occurs as
;; a substring.  `helm-mu-contacts' uses the grep tool for
;; searching.  That means that any regular expression supported by
;; grep can be used when searching for contacts.

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'mu4e)

(defgroup helm-mu nil
  "Helm completion for mu."
  :group 'mu4e)

(defcustom helm-mu-default-search-string ""
  "A default search string for new searches. By default mu
searches all maildirs.  That includes mails from trash and drafts
folders.  The default search string can be used to restrict the
search to only emails in a specific maildir.  For instance, in
order to restrict the search to the inbox, the following can be
used: maildir:/INBOX"
  :group 'helm-mu
  :type  'string)

(defcustom helm-mu-contacts-name-colwidth 22
  "The width of the column showing names when searching contacts."
  :group 'helm-mu
  :type  'integer)

(defcustom helm-mu-contacts-name-replace '("[\"']" "")
  "This can be used for basic transformations of the names.  The
default value removes quotation marks."
  :group 'helm-mu
  :type  '(list string string))

(defcustom helm-mu-contacts-after "01-Jan-1970 00:00:00"
  "Only show contacts from mails received after that time."
  :group 'helm-mu
  :type  'integer)

(defcustom helm-mu-contacts-personal nil
  "If non-nil, only show addresses seen in messages where one of
'my' e-mail addresses was seen in one of the address fields; this
is to exclude addresses only seen in mailing-list messages. See
the --my-address parameter in mu index."
  :group 'helm-mu
  :type  'integer)

(easy-menu-add-item nil '("Tools" "Helm" "Tools") ["Mu" helm-mu t])
(easy-menu-add-item nil '("Tools" "Helm" "Tools") ["Mu contacts" helm-mu-contacts t])


(defface helm-mu-contacts-name-face
  '((t :foreground "black"))
  "Face for names in contacts list."
  :group 'helm-mu-faces)

(defface helm-mu-contacts-address-face
  '((t :foreground "dim gray"))
  "Face for email addresses in contacts list."
  :group 'helm-mu-faces)


(defvar helm-mu-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c C-c") 'helm-mu-open-headers-view)
    (define-key map (kbd "S-<return>") 'helm-mu-open-headers-view)
    map)
  "Keymap used in helm-mu.")


(defvar helm-source-mu
  (helm-build-async-source "Search email with mu"
    :candidates-process #'helm-mu-init
    :candidate-transformer
    '(helm-mu-candidate-parser helm-mu-candidates-formatter)
    :delayed t
    :matchplugin nil
    :nohighlight t
    :requires-pattern 3
    :persistent-action #'helm-mu-persistent-action
    :action '(("Display message in mu4e" . helm-mu-display-email))))

(defvar helm-source-mu-contacts
  (helm-build-in-buffer-source "Search contacts with mu"
    :data #'helm-mu-contacts-init
    :filtered-candidate-transformer #'helm-mu-contacts-transformer
    :fuzzy-match nil
    :action '(("Compose email addressed to this contact" . helm-mu-compose-mail))))


(defun helm-mu-init ()
  "Initialize async mu process for `helm-source-mu'."
  (let ((process-connection-type nil)
        (maxnum (helm-candidate-number-limit helm-source-mu))
        (mucmd "mu find -f $'i\td\tf\tt\ts' --sortfield=d --maxnum=%d --reverse --format=sexp ")
        (sedcmd "sed -e ':a;N;$!ba;s/\\n\\(\\t\\|\\()\\)\\)/ \\2/g'"))
    (prog1
      (start-process-shell-command "helm-mu" helm-buffer
        (concat (format mucmd maxnum)
                (mapconcat 'shell-quote-argument
                           (split-string helm-pattern " ")
                           " ")
                 " | " sedcmd))
      (set-process-sentinel
        (get-buffer-process helm-buffer)
        #'(lambda (process event)
            (if (string= event "finished\n")
                (with-helm-window
                  (setq mode-line-format
                        '(" " mode-line-buffer-identification " "
                          (line-number-mode "%l") " "
                          (:eval (propertize
                                  (format "[Mu Process Finish- (%s results)]"
                                          (max (1- (count-lines
                                                    (point-min) (point-max))) 0))
                                  'face 'helm-grep-finish))))
                  (force-mode-line-update))
                (helm-log "Error: Mu %s"
                          (replace-regexp-in-string "\n" "" event))))))))

(defun helm-mu-contacts-init ()
  "Retrieves contacts from mu."
  (let ((cmd (concat
              "mu cfind --format=mutt-ab"
              (if helm-mu-contacts-personal " --personal" "")
              (format
                " --after=%d"
                (truncate (float-time (date-to-time helm-mu-contacts-after)))))))
    (cdr (split-string (shell-command-to-string cmd) "\n"))))


(defun helm-mu-candidate-parser (candidates)
  "Parses the sexps obtained from mu find."
  (cl-loop for i in candidates
        if (string= i "mu: no matches for search expression")
          collect i
        else
          collect (car (read-from-string i))))

;; The following function recyles code from
;; mu4e~headers-header-handler in order to achieve the same formatting
;; as used in mu4e-headers-view.
(defun helm-mu-candidate-formatter (candidate)
  "Formats a candidate to look like entries in mu4e headers view."
  (let ((line " "))
    (dolist (f-w mu4e-headers-fields)
      (let ((field (car f-w))
            (width (cdr f-w))
            (val (mu4e-message-field candidate (car f-w))) (str))
        (setq str
          (case field
            (:subject
              (concat
                (mu4e~headers-thread-prefix (mu4e-message-field candidate :thread))
                val))
            ((:maildir :path) val)
            ((:to :from :cc :bcc) (mu4e~headers-contact-str val))
            (:from-or-to (mu4e~headers-from-or-to candidate))
            (:date (format-time-string mu4e-headers-date-format val))
            (:mailing-list (mu4e~headers-mailing-list val))
            (:human-date (mu4e~headers-human-date candidate))
            (:flags (propertize (mu4e~headers-flags-str val)
                      'help-echo (format "%S" val)))
            (:tags (propertize (mapconcat 'identity val ", ")))
            (:size (mu4e-display-size val))
            (t (mu4e-error "Unsupported header field (%S)" field))))
        (when str
          (setq line (concat line
              (if (not width) str
                (truncate-string-to-width str width 0 ?\s t)) " ")))))
    (propertize line 'face
      (let ((flags (mu4e-message-field candidate :flags)))
        (cond
          ((memq 'trashed flags) 'mu4e-trashed-face)
          ((memq 'draft flags)   'mu4e-draft-face)
          ((or
             (memq 'unread flags)
             (memq 'new flags))  'mu4e-unread-face)
          ((memq 'flagged flags) 'mu4e-flagged-face)
          ((memq 'replied flags) 'mu4e-replied-face)
          ((memq 'passed flags)  'mu4e-forwarded-face)
          (t                     'mu4e-header-face))))))

;; The function `window-width' does not necessarily report the correct
;; number of characters that fit on a line.  This is a
;; work-around.  See also this bug report:
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=19395
(defun helm-mu-window-width ()
  (if (and (not (featurep 'xemacs))
           (display-graphic-p)
           overflow-newline-into-fringe
           (/= (frame-parameter nil 'left-fringe) 0)
           (/= (frame-parameter nil 'right-fringe) 0))
      (window-body-width)
    (1- (window-body-width))))

(defun helm-mu-candidates-formatter (candidates)
  "Formats the candidates to look like the entries in mu4e headers view."
  (if (equal candidates '("mu: no matches for search expression"))
      (list (propertize (car candidates) 'face 'mu4e-system-face))
    (cl-loop for i in candidates
          for width = (save-excursion (with-helm-window (helm-mu-window-width)))
          for line = (helm-mu-candidate-formatter i)
          collect (cons (truncate-string-to-width line width) i))))

(defun helm-mu-contacts-transformer (candidates source)
  "Formats the contacts to display in two columns, name and
address.  The name column has a predefined width."
  (cl-loop for i in (helm-remove-if-match
                     "\\`\\(reply.*reply\\.github\\.com\\)\\|\\(noreply\\)"
                     candidates)
        for contact = (split-string i "\t")
        for name = (replace-regexp-in-string
                     (car helm-mu-contacts-name-replace)
                     (cadr helm-mu-contacts-name-replace)
                     (cadr contact))
        for address = (car contact)
        for width = (save-excursion (with-helm-window (helm-mu-window-width)))
        collect
        (cons (concat
                (propertize
                  (truncate-string-to-width
                    name helm-mu-contacts-name-colwidth 0 ?\s)
                  'face 'helm-mu-contacts-name-face)
                " "
                (propertize
                  (truncate-string-to-width
                    address (- width helm-mu-contacts-name-colwidth 1) 0 ?\s)
                  'face 'helm-mu-contacts-address-face))
              i)))


(defun helm-mu-open-headers-view ()
  "Open current helm search in mu4e-headers-view."
  (interactive)
  (helm-run-after-quit 'mu4e-headers-search helm-pattern))

(defun helm-mu-display-email (candidate)
  "Open an email using mu4e."
  (mu4e-view-message-with-msgid (plist-get candidate :message-id)))

(defun helm-mu-compose-mail (candidate)
  "Compose a new email directed to the selected contacts."
  (let* ((cand (split-string candidate "\t"))
         (name (cadr cand))
         (address (car cand)))
    (mu4e~compose-mail (concat name " <" address ">"))))

(defun helm-mu-persistent-action (candidate)
  (save-selected-window
    (helm-mu-display-email candidate))
  ;; Redisplay.
  (sit-for 0.1))


;;;###autoload
(defun helm-mu ()
  "Search for emails.  If started in mu4e-headers-view, the
current query will be used to initialize the search.  Otherwise
`helm-mu-default-search-string' will be used."
  (interactive)
  (let ((input (if (eq major-mode 'mu4e-headers-mode)
                   (mu4e-last-query)
                 (concat helm-mu-default-search-string " "))))
    (helm :sources 'helm-source-mu
          :buffer "*helm mu*"
          :full-frame t
          :keymap helm-mu-map
          :input input
          :candidate-number-limit 500)))

;;;###autoload
(defun helm-mu-contacts ()
  "Search for contacts."
  (interactive)
  (helm :sources 'helm-source-mu-contacts
        :buffer "*helm mu contacts*"))

(provide 'helm-mu)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-mu.el ends here
