;;--------------------------------------------
;; Setting up configuration for emacs packages
;;--------------------------------------------
(load-file "~/.emacs.d/bootstrap-packages.el")
(load-file "~/.emacs.d/moar-defuns.el")
;;--------------------------------------------
;; Environment Variables for user
;;--------------------------------------------
(setq user-full-name "Abhishek L"
      user-mail-address "abhishekl.2006@gmail.com")

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
			    (font . "-unknown-CosmicSansNeueMono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")))
  (add-to-list 'default-frame-alist frame-param-list))
(load-theme 'molokai t)

;;--------------------------------------------
;; the quintessential better defaults
;;--------------------------------------------
(require 'uniquify)
(require 'saveplace)
(defalias 'yes-or-no-p 'y-or-n-p)
(ido-mode t)
(ido-ubiquitous t)
(recentf-mode 1)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2)

(setq column-number-mode t
      visible-bell t
      dired-dwim-target t
      custom-safe-themes t
      debug-on-error t
      save-place t
      winner-mode t
      projectile-global-mode t
      uniquify-buffer-name-style 'forward
      backup-directory-alist '((".". "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves\\1" t))
      save-place-file "~/.emacs.d/places")
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
(global-rainbow-delimiters-mode)
(global-auto-complete-mode)
(setq compilation-scroll-output t
      default-input-method "TeX")

;;--------------------------------------------
;; Lispy preferences, mostly borrowed from esk
;;--------------------------------------------
(require 'smartparens)
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
(global-set-key (kbd "C-@") 'mc/mark-all-like-this)
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
         :nick "theanalyst"
	 :realname "Abhi"
	 :pass ,freenode-pass
         :channels ("#emacs" "#emacs-circe" "#hy")
         :nowait-on-connect nil
         :port (8000 . 8001)))
      circe-reduce-lurker-spam t
)
(enable-circe-color-nicks)
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
    (scheme-mode . "λ")
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



