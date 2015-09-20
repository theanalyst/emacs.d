;;--------------------------------------------
;; Setting up configuration for emacs packages
;;--------------------------------------------
(load-file "~/.emacs.d/bootstrap-packages.el")
(load-file "~/.emacs.d/moar-defuns.el")
(load-file "~/.emacs.d/secrets.el")
;;--------------------------------------------
;; Environment Variables for user
;;--------------------------------------------
(setq user-full-name "Abhishek L"
      user-mail-address "abhishek.lekshmanan@gmail.com")

;;--------------------------------------------
;; GUI preferences
;;--------------------------------------------
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(display-battery-mode)
(require 'alpha)

(set-frame-parameter (selected-frame) 'alpha '(80 50))
(dolist (frame-param-list '((alpha 80 50)
			    (font . "-unknown-CosmicSansNeueMono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")))
  (add-to-list 'default-frame-alist frame-param-list))
(load-theme 'molokai t)

;;--------------------------------------------
;; the quintessential better defaults
;;--------------------------------------------
(require 'uniquify)
(require 'saveplace)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'powerline)
;;IDO -> HELM
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-autoresize-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t
      helm-semantic-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-lisp-fuzzy-completion t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

(helm-mode 1)


;; C/C++ Dev environment
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)
(semantic-add-system-include "/usr/include/boost" 'c++-mode)

(require 'function-args)
(fa-config-default)
;(define-key c-mode-map  [(contrl tab)] 'moo-complete)
;(define-key c++-mode-map  [(control tab)] 'moo-complete)
;(define-key c-mode-map (kbd "M-o")  'fa-show)
;(define-key c++-mode-map (kbd "M-o")  'fa-show)

;; (ido-mode t)
;; (ido-ubiquitous t)
;; (recentf-mode 1)
;; (setq ido-enable-flex-matching t
;;       ido-use-filename-at-point 'guess
;;       ido-use-virtual-buffers t
;;       ido-handle-duplicate-virtual-buffers 2)

;; buffer-file-coding-system "utf-8-unix"

(setq column-number-mode t
      visible-bell t
      dired-dwim-target t
      custom-safe-themes t
      debug-on-error t
      save-place t
      winner-mode t
      projectile-global-mode t
      indent-tabs-mode nil
      global-hl-line-mode 1
      global-git-gutter-mode 1
      git-gutter:update-interval 2
      uniquify-buffer-name-style 'forward
      backup-directory-alist '((".". "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves\\1" t))
      save-place-file "~/.emacs.d/places"
      browse-url-generic-program (executable-find "conkeror")
      browse-url-browser-function 'browse-url-generic)
(show-paren-mode)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c r") 'revert-buffer)

;(require 'framemove)
(windmove-default-keybindings 'shift)
(setq framemove-hook-into-windmove)
;;--------------------------------------------
;; Setting some programming preferences
;;--------------------------------------------
(require 'smarter-compile)
(require 'auto-complete)
(require 'yasnippet)
(yas-global-mode t)
(global-auto-complete-mode)
(setq compilation-scroll-output t
      default-input-method "TeX")

(setq c-default-style "linux"
      c-basic-offset 4)

(elpy-enable)
;;--------------------------------------------
;; Lispy preferences, mostly borrowed from esk
;;--------------------------------------------
;; Package: smartparens
(require 'smartparens)
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; when you press RET, the curly braces automatically
;; add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))


(dolist (lisp-mode '(scheme emacs-lisp lisp clojure hy))
  (add-hook (intern (concat (symbol-name lisp-mode) "-mode-hook")) 
	    'paredit-mode))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;--------------------------------------------
;; Some major modes
;;--------------------------------------------
;;(load-file "~/.emacs.d/dos.el")
;;(load-file "~/.emacs.d/gnuplot.el")
(require 'markdown-mode)
(dolist (mode '(("\\.md$" . markdown-mode)               
		("\\.rkt" . scheme-mode)                 
		("\\.bat$" . dos-mode)                   
		("\\.mn$" . nxml-mode)                   
		("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)))
  (add-to-list 'auto-mode-alist mode))

;;--------------------------------------------
;; Multiple cursors and other key prefs
;;--------------------------------------------
(require 'multiple-cursors)
(require 'ace-jump-mode)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-@") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;--------------------------------------------
;; Some writing mode & preferences
;;--------------------------------------------
(require 'typo)
(require 'writegood-mode)
(setq ispell-program-name "aspell")
;(load-file "~/.emacs.d/wc-mode.el")
;(load-file "~/.emacs.d/journal.el")

;;--------------------------------------------
;; irc settings
;;--------------------------------------------
(setq circe-network-options
      `(("Freenode"
         :nick "abhi-"
	 :realname "Abhi"
	 :pass ,freenode-pass
         :channels ("#jiocloud" "#jiocloud-dev")
         :nowait-on-connect nil
         :port (8000 . 8001))
	("ZNC"
	 :host "furnace.firrre.com"
	 :port 9090
	 :tls t
	 :pass ,znc-pass)
	("OFTC-ZNC"
	 :host "furnace.firrre.com"
	 :port 9090
	 :tls t
	 :pass ,znc-oftc-pass))
      circe-reduce-lurker-spam t)

(enable-circe-color-nicks)

(defun irc ()
  "Connect to irc, circe style"
  (interactive)
  (circe "ZNC")
  (circe "OFTC-ZNC")
  (circe "Freenode"))

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

(defvar my-circe-bot-list '("fsbot" "rudybot" "jiocloudbot"))
(defun my-circe-message-option-bot (nick &rest ignored)
  (when (member nick my-circe-bot-list)
    '((text-properties . (face circe-fool-face
                          lui-do-not-track t)))))
(add-hook 'circe-message-option-functions 'my-circe-message-option-bot)

(require 'lui-autopaste)
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

;;--------------------------------------------
;; Some prettiness in prog langs & modelines
;;--------------------------------------------

(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (lisp-interaction-mode . " λ")
    (smartparens-mode "§")
    (paredit-mode " {}")
    (yas-minor-mode " ¥")
    (magit-diff-mode "Δ")
    ;; Major Modes
    (python-mode . "Py")
    (emacs-lisp-mode . "ξ")
    (nxml-mode . "χ")
    (inferior-scheme-mode . "∫λ")
    (scheme-mode . "Sc")
    (ielm-mode . "ξλ")
    )
 "Alist for `clean-mode-line'.
 
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original."
)

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))
  
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(load-file "~/.emacs.d/pretty-symbols.el")
(setq pretty-symbol-categories '(lambda relational logical))

(dolist (prog-modes '(c-mode-hook c++-mode-hook go-mode-hook 
		     java-mode-hook js-mode-hook
                     python-mode-hook ruby-mode-hook
		     emacs-lisp-mode-hook inferior-lisp-mode-hook
		     lisp-mode-hook scheme-mode-hook hy-mode-hook))
  (add-hook prog-modes 'pretty-symbols-mode))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)))
;(add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(setq mu4e-maildir (expand-file-name "~/Maildir")
      mu4e-drafts-folder "/Gmail/[Gmail].Drafts"
      mu4e-sent-folder   "/Gmail/[Gmail].Sent Mail"
      mu4e-trash-folder "/Gmail/[Gmail].Trash"
      mu4e-get-mail-command "offlineimap"
      mu4e-attachment-dir "~/Downloads/mail"
      mu4e-update-interval 300
      mu4e-maildir-shortcuts  '(("/Gmail/INBOX" . ?i)
				("/Gmail/[Gmail].Sent Mail" . ?s)
				("/Gmail/[Gmail].Trash" . ?t))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      mu4e-maildir-shortcuts  '(("/Gmail/INBOX" . ?i)
				("/Gmail/[Gmail].Sent Mail" . ?s)
				("/Gmail/[Gmail].Trash" . ?t)
                                ("/Work/INBOX" . ?w)
                                ("/Work/Sent" . ?p)))



(defvar my-mu4e-account-alist
  '(("Gmail"
     (mu4e-drafts-folder "/Gmail/[Gmail].Drafts")
     (mu4e-sent-folder   "/Gmail/[Gmail].Sent Mail")
     (mu4e-trash-folder "/Gmail/[Gmail].Trash")
     (mu4e-get-mail-command "offlineimap -a Gmail")
     (mu4e-attachment-dir "~/Downloads/mail")
     (mu4e-update-interval 300)
     (user-mail-address "abhishek.lekshmanan@gmail.com")
     (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587)
     (starttls-use-gnutls t))
    
     ("Work"
      (mu4e-sent-folder "/Work/Sent")
      (mu4e-drafts-folder "/Work/Drafts")
      (mu4e-trash-folder "/Work/Trash")
      (user-mail-address "abhishek.lekshmanan@ril.com")
      (smtpmail-smtp-user "abhishek.lekshmanan")
      (smtpmail-smtp-server "localhost")
      (smtpmail-starttls-credentials '(("localhost" 465 nil nil)))
      (smtpmail-smtp-service 465)
      (smtpmail-stream-type plain)
      (smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")))))



(setq mail-user-agent 'mu4e-user-agent)
(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq mu4e-compose-signature "Abhishek")

(defadvice mml2015-sign (after mml2015-sign-rename (cont) act)
    (save-excursion
      (search-backward "Content-Type: application/pgp-signature")
      (goto-char (point-at-eol))
      (insert "; name=\"signature.asc\"; description=\"Digital signature\"")))

(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

(require 'netrc)

(defun offlineimap-get-password (host port)
  "Used by ~/offlineimap.py"
(let*
    ((auth-credentials (netrc-parse smtpmail-auth-credentials))
     (hostentry (netrc-machine auth-credentials host port port)))
  (when hostentry
    (netrc-get hostentry "password"))))
