(provide 'pp-keybindings)

(global-set-key (kbd "M-x") 'counsel-M-x)

(evil-leader/set-leader "<SPC>")

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(defun pp-vterm (vtname)
  (if (not (get-buffer-window vtname))
      (if (not (get-buffer vtname))
	  (vterm vtname)
	(switch-to-buffer vtname))
    (select-window (get-buffer-window vtname))))

(evil-leader/set-key
  "<SPC>" 'pp-avy-switch
  ";" 'comment-dwim
  "F" 'ag-project
  "R" 'lsp-rename
  "W" 'ace-swap-window
  "a" 'lsp-execute-code-action 
  "d" '(lambda () (interactive) (dired default-directory))
  "e" 'eval-buffer
  "g" '(lambda () (interactive) (revert-buffer t t))
  "h" 'lsp-describe-thing-at-point
  "j" 'avy-goto-char-timer
  "k" 'goto-last-change
  "DEL" 'switch-to-prev-buffer
  "n" '(lambda () (interactive) (org-capture))
  "s" 'save-buffer
  "1" '(lambda () (interactive) (shell "*sh1*"))
  "2" '(lambda () (interactive) (shell "*sh2*"))
  "3" '(lambda () (interactive) (pp-vterm "*vt3*"))
  "4" '(lambda () (interactive) (pp-vterm "*vt4*"))
  "w" 'ace-window
  "b" '(lambda() (interactive) (pp-cpp-build nil))
  "B" '(lambda() (interactive) (pp-cpp-build t))
  )

(evil-leader/set-key
  "mm" 'magit-status
  "ml" 'magit-log-buffer-file
  "md" 'magit-diff-buffer-file
  "mb" 'vc-annotate) 

(evil-leader/set-key-for-mode 'c++-mode
  "f" 'c-mark-function
  "v" 'pp-cpp-goto-related-file
  "i" 'pp-avy-include)

(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(evil-leader/set-key
  "ld" 'xref-find-definitions
  "lD" 'xref-find-definitions-other-window
  "lr" 'xref-find-references
  "la" 'eglot-code-actions
  "lR" 'eglot-rename
  "lh" 'eldoc-print-current-symbol-info
  "lf" '(lambda()
	  (interactive)
	  (if (equal major-mode 'python-mode)
	      (progn
		(save-buffer)
		(shell-command (format "black %s" buffer-file-name))
		(revert-buffer 1 1))
	   (if (equal major-mode c++-mode)
		 (eglot-format (region-beginning) (region-end)))))
  "lt" 'eglot-find-typeDefinition
  )

(evil-leader/set-key
  "[" 'origami-forward-toggle-node
  "{" 'origami-open-node-recursively
  "}" 'origami-close-node-recursively
  "\\" 'origami-undo
  "|" 'origami-redo
  )

(evil-leader/set-key
  "tn" 'tab-next
  "tc" 'tab-new
  "tt" 'tab-list
  "tp" 'tab-previous)

(evil-leader/set-key
  "cc" 'denote
  "cd" 'denote-subdirectory
  "cl" 'denote-link)


;; git
(evil-leader/set-key "RET" 'magit-status)

(windmove-default-keybindings)

(global-set-key (read-kbd-macro "M-SPC") 'dabbrev-expand)
(global-set-key (kbd "<f6>") 'display-line-numbers-mode)
(global-set-key (kbd "<f5>") 'indent-whole-buffer)
(global-set-key (kbd "M-e") 'my-next-error)
(global-set-key (kbd "M-E") 'my-previous-error)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-=") 'balance-windows)
(global-set-key (kbd "M-j") 'forward-paragraph)
(global-set-key (kbd "M-k") 'backward-paragraph)
(global-set-key (kbd "M-RET") 'newline-and-indent)
(global-set-key (kbd "<f12>") 'pp-avy-switch)
(global-set-key (kbd "S-<f12>") 'vterm-toggle)
(global-set-key (kbd "<f13>") 'evil-goto-line)

(global-set-key (kbd "<M-left>") 'evil-jump-backward)
(global-set-key (kbd "<M-right>") 'evil-jump-forward)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)



(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-insert-state-map (kbd "S-<left>") 'windmove-left)
(define-key evil-insert-state-map (kbd "S-<right>") 'windmove-right)

(defun my-c++-keybindings ()
  "Set C++ specific keybindings"
  (interactive)
  (define-key c-mode-base-map (kbd "M-e") 'my-next-error)
  (define-key c-mode-base-map (kbd "M-E") 'my-previous-error)
  (define-key c-mode-base-map (kbd "M-j") 'forward-paragraph)
  (define-key c-mode-base-map (kbd "M-k") 'backward-paragraph)
  (define-key c-mode-base-map (kbd "M-n") 'pp-cpp-insert-file-name-stem)
  (define-key c-mode-base-map (kbd "M-RET") 'yas-expand)
  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete)))

(add-hook 'c++-mode-hook (lambda () (my-c++-keybindings)))

