(require 'package)
(require 'cl)
;; mostly borrowed from prelude, installing my packages
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
			 ("ELPA" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("MELPA" . "http://melpa.milkbox.net/packages/" )
			 ("elpy" . "http://jorgenschaefer.github.io/packages/")))

(defvar abhi/packages
  '(ac-nrepl
    ace-jump-mode
    alpha
    apache-mode
    circe
    color-theme-buffer-local
    color-theme-heroku
    color-theme-sanityinc-tomorrow
    color-theme-solarized
    color-theme
    company
    elisp-slime-nav
    elnode
    elpy
    auto-complete
    find-file-in-project
    gist
    gh
    git-gutter
    grapnel
    hackernews
    highlight-symbol
    htmlize
    hy-mode
    ido-ubiquitous
    idomenu
    ix
    inf-ruby
    lcs
    lorem-ipsum
    lua-mode
    magit
    markdown-mode
    multiple-cursors
    nrepl
    clojure-mode
    org-pomodoro
    paredit
    parscope
    projectile
    list-utils
    rainbow-delimiters
    rainbow-mode
    sauron
    scratch
    smarter-compile
    smartparens
    dash
    smex
    synonyms
    twilight-anti-bright-theme
    twilight-bright-theme
    twilight-theme
    typo
    writegood-mode
    wtf
    yascroll
    yasnippet))

(defun abhi/package-installed? (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun abhi/install-packages ()
  (unless (every 'package-installed-p abhi/packages)
    (message "refreshing and installing missing packages..hold on")
    (package-refresh-contents)
    (mapc 'abhi/package-installed? abhi/packages)
    (message "done bootstrapping...enjoy your emacs session!")))

(package-initialize)
(abhi/install-packages)
