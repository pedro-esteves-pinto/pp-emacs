(provide 'pp-keybindings)

(global-set-key (kbd "M-x") 'counsel-M-x)

(evil-leader/set-leader "<SPC>")

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(evil-leader/set-key
  "!" '(lambda() (interactive) (window-configuration-to-register ?1))
  "/" 'deft
  "1" '(lambda() (interactive) (jump-to-register ?1))
  "2" '(lambda() (interactive) (jump-to-register ?2))
  ";" 'comment-dwim
  "<SPC>" 'pp-avy-switch
  "@" '(lambda() (interactive) (window-configuration-to-register ?2))
  "D" 'xref-find-definitions-other-window
  "F" 'ag-project
  "R" 'lsp-rename
  "W" 'ace-swap-window
  "a" 'lsp-execute-code-action
  "d" 'xref-find-definitions
  "e" 'eval-buffer
  "g" '(lambda () (interactive) (revert-buffer t t))
  "h" 'lsp-describe-thing-at-point
  "j" 'avy-goto-char-timer
  "k" 'goto-last-change
  "n" '(lambda () (interactive) (org-capture))
  "r" 'xref-find-references
  "s" 'save-buffer
  "t" 'shell
  "u" 'org-cliplink
  "w" 'ace-window
  "x" 'counsel-M-x
  )

(evil-leader/set-key-for-mode 'c++-mode 
  "b" '(lambda() (interactive) (pp-cpp-build nil))
  "B" '(lambda() (interactive) (pp-cpp-build t))
  "m" 'c-mark-function
  "v" 'pp-cpp-goto-related-file
  "i" 'pp-avy-include)

(evil-leader/set-key-for-mode 'python-mode 
  "f" '(lambda() (interactive) (python-format)))

(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(evil-leader/set-key
  "ld" 'lsp-ui-peek-find-definitions 
  "ls" 'helm-lsp-workspace-symbol
  "la" 'lsp-execute-code-action
  "lr" 'lsp-ui-peek-find-references
  "lR" 'lsp-rename
  "lh" 'lsp-ui-doc-glance
  "lf" 'lsp-format-buffer
  "l[" 'lsp-ui-peek-jump-backward
  "l]" 'lsp-ui-peek-jump-forward 
  )

(evil-leader/set-key
  "zc" 'vimish-fold
  "zd" 'vimish-fold-delete
  "zt" 'vimish-fold-toggle)

(evil-leader/set-key
  "oo" 'org-roam-node-find
  "oc" 'org-roam-capture
  "oY" 'org-download-screenshot
  "oy" 'org-download-yank
  "ol" 'org-roam-node-insert)

(global-set-key (kbd "C-l") 'org-roam-node-insert)

;; git 
(evil-leader/set-key "RET" 'magit-status)

(windmove-default-keybindings)
(global-set-key (read-kbd-macro "M-SPC") 'dabbrev-expand)
(global-set-key (kbd "C-<f5>") 'display-line-numbers-mode)

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
(global-set-key (kbd "<f13>") 'evil-goto-line)

(global-set-key (kbd "<M-left>")     'evil-jump-backward)
(global-set-key (kbd "<M-right>")     'evil-jump-forward)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)
(global-set-key (kbd "C-+")  'text-scale-increase)
(global-set-key (kbd "C--")  'text-scale-decrease)
(global-set-key (kbd "C-s")  'swiper-isearch)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "<f9>") 'recompile)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

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

