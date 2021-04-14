;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     javascript
     restclient
     html
     ansible
     csv
     ruby
     go
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t)
     better-defaults
     c-c++
     ;eos
     emacs-lisp
     git
     gtags
     geolocation
     haskell
     markdown
     org
     python
     (shell :variables
             shell-default-height 30
             shell-default-position 'bottom)
     spell-checking
     syntax-checking
     ;semantic
     version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     circe
     multiple-cursors
     helm
     helm-gtags
     vlf
     salt-mode
     yapfify
     uuidgen
     rase
     py-isort org-projectile org-download mwim live-py-mode
link-hint intero hlint-refactor helm-hoogle git-link flyspell-correct-helm
flyspell-correct eyebrowse
eshell-z dumb-jump company-ghci column-enforce-mode monky calfw jinja2-mode
terraform-mode material-theme dockerfile-mode docker rainbow-mode smart-compile
ix salt-mode restclient-helm ag helm-company helm-c-yasnippet company-statistics
company-quickhelp company-ghc company-cabal company-c-headers company-anaconda
company auto-yasnippet ac-ispell vlf xterm-color toc-org theme-changer sunshine
stickyfunc-enhance srefactor smeargle shm shell-pop pyvenv pytest pyenv-mode
pip-requirements osx-location orgit org-repo-todo org-present org-pomodoro
org-plus-contrib org-bullets multiple-cursors multi-term mmm-mode markdown-toc
markdown-mode magit-gitflow hy-mode htmlize hindent helm-pydoc helm-gtags
helm-gitignore helm-flyspell haskell-snippets gnuplot gitignore-mode
gitconfig-mode gitattributes-mode git-timemachine git-messenger
git-gutter-fringe+ git-gutter-fringe git-gutter+ git-gutter ghc gh-md ggtags
flycheck-pos-tip flycheck-haskell flycheck eshell-prompt-extras
esh-help disaster diff-hl cython-mode cmm-mode cmake-mode clang-format
auto-dictionary anaconda-mode helm-circe ws-butler window-numbering which-key
volatile-highlights vi-tilde-fringe use-package spacemacs-theme spaceline
solarized-theme smooth-scrolling restart-emacs rainbow-delimiters quelpa popwin
popup persp-mode pcre2el paradox page-break-lines open-junk-file neotree
move-text monokai-theme macrostep lorem-ipsum linum-relative leuven-theme info+
indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses
highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop
helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag
google-translate golden-ratio flx-ido fill-column-indicator fancy-battery
expand-region exec-path-from-shell  eval-sexp-fu elisp-slime-nav
define-word clean-aindent-mode circe buffer-move auto-highlight-symbol
auto-compile aggressive-indent ace-window ace-link
ace-jump-helm-line mu4e-maildirs-extension magithub
yasnippet-snippets circe-notifications company-rtags rtags cmake-ide irony-mode
company-irony company-irony-c-headers helm-rtags flycheck-rtags modern-cpp-font-lock
howdoyou doom-themes
;; https://github.com/syl20bnr/spacemacs/issues/14321#issuecomment-769244043
;; to allow me to use the master branch of spacemacs (for stability reasons)
(evil-magit :location (recipe
                       :fetcher github
                       :repo "emacs-evil/evil-magit"))


)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages nil))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         solarized-dark
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   ;; dotspacemacs-default-font '("Inconsolata"
   ;;                             :size 16
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 1.1)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1
                               :antialias true
                               :hinting false)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all',
   ;; `trailing', `changed' or `nil'. Default is `changed' (cleanup whitespace
   ;; on changed lines) (default 'changed)
   dotspacemacs-whitespace-cleanup 'changed
   dotspacemacs-helm-use-fuzzy 'source
   ))
;(setq dotspacemacs-helm-use-fuzzy 'source)
(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost any
user code here.  The exception is org related code, which should be placed in
`dotspacemacs/user-config'."
  ;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  ;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
  (require 'netrc)
  ;(require 'notmuch)
  ;(require 'mu4e)
  ;(require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text
        mu4e-compose-dont-reply-to-self t)
  (setq mail-user-agent 'message-user-agent)
)



;(load-file "~/.emacs.d/private/secrets.el")
;; (setq circe-network-options
;;       `(
;;         ("ZNC"
;;          :host "furnace.firrre.com"
;;          :port 9090
;;          :tls t
;;          :pass ,znc-pass)
;;         ("OFTC-ZNC"
;;          :host "furnace.firrre.com"
;;          :port 9090
;;          :tls t
;;          :pass ,znc-oftc-pass)
;;         ("SUSE"
;;          :host "irc.suse.de"
;;          :port 6697
;;          :tls t
;;          :nick "abhi-"
;;          :channels ("#ceph" "cloud" "#suse" "#opensuse")))
;;       circe-reduce-lurker-spam t)

;; (defun irc ()
;;     "Connect to irc, circe style"
;;     (interactive)
;;     (circe "ZNC")
;;     (circe "OFTC-ZNC")
;;     (circe "SUSE"))

(defun circe-network-connected-p (network)
  "Return non-nil if there's any Circe server-buffer whose
`circe-server-netwok' is NETWORK."
  (catch 'return
    (dolist (buffer (circe-server-buffers))
      (with-current-buffer buffer
        (if (string= network circe-server-network)
            (throw 'return t))))))

(defun circe-maybe-connect (network)
  "Connect to NETWORK, but ask user for confirmation if it's
already been connected to."
  (interactive "sNetwork: ")
  (if (or (not (circe-network-connected-p network))
          (y-or-n-p (format "Already connected to %s, reconnect?" network)))
      (circe network)))

(defun weekly-report()
  (interactive)
  (let ((recipient "ceph@suse.de")
        (subject
         (concat "Weekly Report: week "
                 (format "%s " (format-time-string "%W")) )) ; Assuming sending a prev. week report
        (body "[red]\n\n[amber]\n\n[green]\n\n\[outlook]\n\n"))
    (message-mail recipient subject)
    (message-goto-body)
    (insert body)))

(defun fc/isearch-yank-symbol ()
  "Yank the symbol at point into the isearch minibuffer.

C-w does something similar in isearch but it only looks for
the rest of the word. I want to look for the whole string. And
symbol, not word, as I need this for programming the most."
  (interactive)
  (isearch-yank-string
   (save-excursion
     (when (and (not isearch-forward)
                isearch-other-end)
       (goto-char isearch-other-end))
     (thing-at-point 'symbol))))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (setq package-enable-at-startup nil)
  (package-initialize)
  (require 'multiple-cursors)
  (require 'circe)
  (dolist (message '("354" "315")) (circe-set-display-handler message 'circe-display-ignore))
  (require 'smtpmail)
  (require 'vlf-setup) ; for very large files
  ;(require 'mu4e-maildirs-extension)
  ;(mu4e-maildirs-extension)
  (global-company-mode)
  (enable-circe-color-nicks)
  (setq user-name "Abhishek Lekshmanan")
  (setq user-mail-address "abhishek.l@cern.ch")
  (setq default-input-method "TeX")
  (windmove-default-keybindings 'shift)
  (global-set-key (kbd "C-c r") 'revert-buffer)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "M-<up>") 'move-line-up)
  (global-set-key (kbd "M-<down>") 'move-line-down)

  (setq z3-solver-cmd "z3")
  (setq org-todo-keywords
        '((sequence "TODO" "IN_PROGRESS(i@)" "|" "DONE")
          (sequence "NEW" "VERIFIED" "IN_PROGRESS" "NEEDINFO" "NEED_REVIEW" "|" "MERGED")))

  (setq org-log-done 'time)
  (setq org-log-done 'note)
  (setq org-todo-keyword-faces
        '(("IN_PROGRESS" . (:foreground "blue" :weight bold))
          ("NEEDINFO" . (:foreground "yellow" :weight bold))
          ))
  (bind-keys
   :map smartparens-mode-map
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)

   ("C-<down>" . sp-down-sexp)
   ("C-<up>"   . sp-up-sexp)
   ("M-<down>" . sp-backward-down-sexp)
   ("M-<up>"   . sp-backward-up-sexp)

   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)

   ("C-S-f" . sp-forward-symbol)
   ("C-S-b" . sp-backward-symbol)

   ("C-<right>" . sp-forward-slurp-sexp)
   ("M-<right>" . sp-forward-barf-sexp)
   ("C-<left>"  . sp-backward-slurp-sexp)
   ("M-<left>"  . sp-backward-barf-sexp)

   ("C-M-t" . sp-transpose-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-k"   . sp-kill-hybrid-sexp)
   ("M-k"   . sp-backward-kill-sexp)
   ("C-M-w" . sp-copy-sexp)

   ("C-M-d" . delete-sexp)

   ("M-<backspace>" . backward-kill-word)
   ("C-<backspace>" . sp-backward-kill-word)
   ([remap sp-backward-kill-word] . backward-kill-word)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)

   ("C-x C-t" . sp-transpose-hybrid-sexp)

   ("C-c ("  . wrap-with-parens)
   ("C-c ["  . wrap-with-brackets)
   ("C-c {"  . wrap-with-braces)
   ("C-c '"  . wrap-with-single-quotes)
   ("C-c \"" . wrap-with-double-quotes)
   ("C-c _"  . wrap-with-underscores)
   ("C-c `"  . wrap-with-back-quotes)
   ;("M-."    . helm-gtags-find-tag)
   )
  (require 'rtags)
  (require 'company-rtags)
  (require 'flycheck-rtags)
  (require 'irony)
  (require 'company-irony-c-headers)

  (setq rtags-completions-enabled t
        rtags-autostart-diagnostics t
        rtags-use-helm t
        rtags-display-result-backend 'helm)
  (defun ab/flycheck-rtags-setup()
    (flycheck-select-checker 'rtags)
    (setq flycheck-highlighting-mode nil
          flycheck-check-syntax-automatically nil))
  ;; (defun ab/irony-mode-hook()
  ;;   (define-key irony-mode-map [remap completion-at-point]
  ;;     'irony-completion-at-point-async))
  (rtags-enable-standard-keybindings c-mode-base-map "\C-ct")
  (eval-after-load 'company
    '(add-to-list
      'company-backends '(company-irony-c-headers company-irony company-rtags)))
  (put 'cmake-ide-build-dir 'safe-local-variable #'stringp)
  (put 'cmake-ide-make-command 'safe-local-variable #'stringp)
  (cmake-ide-setup)

  (add-hook 'c-mode-common-hook #'ab/flycheck-rtags-setup)
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (dolist (message '("354" "315")) (circe-set-display-handler message 'circe-display-ignore))
  (load-theme 'doom-solarized-dark t)
  )

  (setq notmuch-archive-tags '("-unread"))
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-@") 'mc/mark-all-like-this)
  (setq epa-pinentry-mode 'loopback)
  (define-key isearch-mode-map (kbd "C-d") 'fc/isearch-yank-symbol)
  (setq calendar-location-name "Nurnberg, Germany"
        calendar-latitude 49.45
        calendar-longitude 11.08)
  (defvar spacemacs-mode-line-new-version-lighterp t)
  (setq mu4e-maildir (expand-file-name "~/mbsync")
        mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder   "/Sent"
        mu4e-trash-folder "/Trash"
        mu4e-get-mail-command "mbsync suse-mail"
        mu4e-attachment-dir "~/Downloads/mail"
        mu4e-update-interval 60
        mu4e-headers-full-search nil
        mu4e-index-cleanup nil
        mu4e-index-lazy-check t
        mu4e-maildir-shortcuts  '(("/INBOX" . ?i)
                                  ("/Sent" . ?s)
                                  ("/Trash" . ?t))
        message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials '(("imap.suse.de" 587 nil nil))
        smtpmail-smtp-service 587
        smtpmail-smtp-server "imap.suse.de"
        smtpmail-smtp-user "alekshmanan@suse.de"
        smtpmail-stream-type 'starttls
        starttls-use-gnutls t
        smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
        mu4e-view-prefer-html t
        mu4e-change-filenames-when-moving t
        )
(setq custom-safe-themes t)
;(load-theme 'doom-solarized-dark t)
  ;; (if (display-graphic-p)
  ;;    (load-theme 'solarized-dark t)
  ;;  (load-theme 'monokai t))
                                        ;(load-file "~/.emacs.d/circe-notifications.el")
  ;(add-hook 'circe-server-connected-hook 'enable-circe-notifications)
  ;(load "lui-logging" nil t)
;

(defun leo-markdown-fontify-buffer-wiki-links-empty ()
  "Empty replacement for `markdown-fontify-buffer-wiki-links` due to hanging bug."
  (interactive))

(eval-after-load "markdown-mode"
  '(progn
     (fset 'markdown-fontify-buffer-wiki-links
           'leo-markdown-fontify-buffer-wiki-links-empty)))
  ;(enable-lui-logging-globally)
  (defun offlineimap-get-password (host port)
    "Used by ~/offlineimap.py"
    (let*
        ((auth-credentials (netrc-parse smtpmail-auth-credentials))
         (hostentry (netrc-machine auth-credentials host port port)))
      (when hostentry
        (netrc-get hostentry "password"))))

(defadvice epg--start (around advice-epg-disable-agent disable)
  "Make epg--start not able to find a gpg-agent"
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))

(defun epg-disable-agent ()
  "Make EasyPG bypass any gpg-agent"
  (interactive)
  (ad-enable-advice 'epg--start 'around 'advice-epg-disable-agent)
  (ad-activate 'epg--start)
  (message "EasyPG gpg-agent bypassed"))

(defun epg-enable-agent ()
  "Make EasyPG use a gpg-agent after having been disabled with epg-disable-agent"
  (interactive)
  (ad-disable-advice 'epg--start 'around 'advice-epg-disable-agent)
  (ad-activate 'epg--start)
  (message "EasyPG gpg-agent re-enabled"))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol t)
 '(highlight-parentheses-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(package-selected-packages
   '(evil-magit ripgrep protobuf-mode yasnippet-snippets yapfify xterm-color window-numbering web-mode web-beautify vlf unfill theme-changer terraform-mode hcl-mode tagedit sunshine stickyfunc-enhance srefactor smeargle smart-compile slim-mode shm shell-pop scss-mode sass-mode salt-mode mmm-jinja2 yaml-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reveal-in-osx-finder restclient-helm rbenv rase rake rainbow-mode quelpa pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements pbcopy osx-trash osx-location osx-dictionary orgit org-repo-todo org-projectile org-category-capture org-present org-pomodoro org-mime org-download ob-restclient ob-http mwim multi-term mu4e-maildirs-extension monokai-theme monky modern-cpp-font-lock mmm-mode minitest material-theme markdown-toc magithub markdown-mode ghub+ apiwrap ghub treepy magit-gitflow magit-popup magit livid-mode skewer-mode simple-httpd live-py-mode leuven-theme launchctl js2-refactor multiple-cursors js2-mode js-doc jinja2-mode ix grapnel hy-mode dash-functional htmlize howdoyou promise hlint-refactor hindent helm-rtags helm-pydoc helm-hoogle helm-gtags helm-gitignore helm-flyspell helm-css-scss helm-company helm-circe helm-c-yasnippet haskell-snippets haml-mode go-guru go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter git-commit with-editor gh-md ggtags fuzzy flyspell-correct-helm flyspell-correct flycheck-rtags flycheck-pos-tip flycheck-haskell flycheck eshell-z eshell-prompt-extras esh-help emmet-mode doom-themes dockerfile-mode docker transient tablist json-mode docker-tramp json-snatcher json-reformat disaster diff-hl cython-mode csv-mode company-web web-completion-data company-statistics company-rtags rtags company-restclient restclient know-your-http-well company-quickhelp pos-tip company-irony-c-headers company-irony irony company-go go-mode company-ghci haskell-mode company-cabal company-c-headers company-ansible company-anaconda company coffee-mode cmm-mode cmake-mode cmake-ide levenshtein clang-format circe-notifications alert log4e gntp circe chruby calfw bundler inf-ruby buffer-move auto-yasnippet yasnippet auto-dictionary ansible-doc ansible anaconda-mode pythonic ag ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package undo-tree toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired f evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg eval-sexp-fu elisp-slime-nav dumb-jump dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#839496" :background "#002b36")))))
