;;----------------------------------------
;; hl-faces
;;----------------------------------------
(defun set-dark-hl-face ()
  "Sets a dark hl-line face"
  (interactive)
  (set-face-background hl-line-face "gray30"))

(defun set-light-hl-face ()
  "Sets a light hl-line face"
  (interactive)
  (set-face-background hl-line-face "WhiteSmoke"))

;;----------------------------------------
;; Eval and replace sexp with result
;;----------------------------------------
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-end-replace)

;;----------------------------------------
;; C-s C-d for searching word at point
;;----------------------------------------
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

(define-key isearch-mode-map (kbd "C-d") 'fc/isearch-yank-symbol)

;;----------------------------------------
;; hs-mode to hide comments as well
;;----------------------------------------
(defun hs-hide-all-comments ()
  "Hide all top level blocks, if they are comments, displaying only first line.
Move point to the beginning of the line, and run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (unless hs-allow-nesting
       (hs-discard-overlays (point-min) (point-max)))
     (goto-char (point-min))
     (let ((spew (make-progress-reporter "Hiding all comment blocks..."
                                         (point-min) (point-max)))
           (re (concat "\\(" hs-c-start-regexp "\\)")))
       (while (re-search-forward re (point-max) t)
         (if (match-beginning 1)
           ;; found a comment, probably
           (let ((c-reg (hs-inside-comment-p)))
             (when (and c-reg (car c-reg))
               (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                   (hs-hide-block-at-point t c-reg)
                 (goto-char (nth 1 c-reg))))))
         (progress-reporter-update spew (point)))
       (progress-reporter-done spew)))
   (beginning-of-line)
   (run-hooks 'hs-hide-hook)))

;;----------------------------------------
;; Moving lines around
;;----------------------------------------
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;;----------------------------------------
;; Kill all buffers except current one
;;----------------------------------------
(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

;;----------------------------------------
;; Runs a bash shell in windows emacs
;;----------------------------------------
(defun run-bash ()
  "Runs a bash shell in windows emacs"
  (interactive)
  (let ((explicit-shell-file-name (executable-find "sh"))
         (explicit-sh.exe-args '("--login" "-i")))
    (shell (generate-new-buffer-name "*bash*"))))

;;----------------------------------------
;; Remove non print ascii chars
;;----------------------------------------
(defun remove-non-print-ascii()
  "Remove non printable ascii characters from buffer"
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[^ -~\n]" (point-max) t)
	(replace-match "" nil nil))
      (message "Removed non-printable ascii chars"))))

;;----------------------------------------
;; ask magit to do a pull --rebase
;;----------------------------------------
(defun magit-pull-rebase ()
  " Do a pull rebase operation, instead of normal pull "
  (interactive)
  (magit-run-git-async "pull" "--rebase" "-v"))

;;----------------------------------------
;; Clear shell buffers
;;----------------------------------------
(defun clear-shell ()
   (interactive)
   (let ((comint-buffer-maximum-size 0))
     (comint-truncate-buffer)))
