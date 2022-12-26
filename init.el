;; Install straight.el if needed
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
;; End straight.el

(setq package-enable-at-startup nil)

(use-package evil
  :init
  (setq evil-want-integration t)
  ;; http://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working
  (setq evil-want-C-i-jump nil)
  ;; See https://github.com/emacs-evil/evil-collection/issues/60 for more details.
  (setq evil-want-keybinding nil)
  :config
  (add-to-list 'evil-insert-state-modes 'shell-mode)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-leader
  :config
  (global-evil-leader-mode))

(use-package evil-args
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package avy)

(use-package counsel
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (counsel-mode 1))

(use-package ivy
  :config
  (setq ivy-use-selectable-prompt t)
  (ivy-mode 1))

(use-package which-key
  :init
  (which-key-mode))

(use-package org
  :init
  (setq org-confirm-babel-evaluate nil)
  (setq org-startup-indented t)
  (setq org-export-html-style-include-scripts nil
	org-export-html-style-include-default nil)
  (setq org-return-follows-link t)
  (setq org-clock-task-overrun nil)
  (setq org-return-follows-link 1)
  (setq org-startup-truncated nil)
  (setq org-agenda-span 14)
  (setq org-capture-templates '(("j" "Journal" plain
				 (file+olp+datetree "~/Dropbox/notes/journal/personal-journal.org")
				 "%i%?")
				("n" "Note" entry
				 (file+headline "~/Dropbox/notes/planning/plan.org" "Inbox")
				 "* %?\n %i")))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  ;; dot
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t) (dot . t)))
  )

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/notes/org-roam")
  :config
  (org-roam-db-autosync-mode))

(use-package magit
  :init
  (setq magit-display-buffer-function
	(lambda (buffer)
	  (display-buffer
	   buffer
	   (cond ((and (derived-mode-p 'magit-mode)
		       (eq (with-current-buffer buffer major-mode)
			   'magit-status-mode))
		  nil)
		 ((memq (with-current-buffer buffer major-mode)
			'(magit-process-mode
			  magit-revision-mode
			  magit-diff-mode
			  magit-stash-mode)) 
		  nil)
		 (t '(display-buffer-same-window)))))))

(use-package ag)
(use-package buffer-move)
(use-package cmake-mode)
(use-package yaml-mode)
(use-package ace-window)

(use-package eglot
  :init
  (setq eglot-server-programs '((python-mode . ("pyright-langserver" "--stdio"))))
  (setq eglot-ignored-server-capabilities '(:hoverProvider :signatureHelpProvider)))



(use-package helm
  :init
  (setq helm-echo-input-in-header-line t)
  (setq helm-split-window-in-side-p t)
  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
  	(overlay-put ov 'window (selected-window))
  	(overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
  				`(:background ,bg-color :foreground ,bg-color)))
  	(setq-local cursor-type nil))))
  :config
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe))

(use-package flycheck)

(use-package vterm
  :config
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package company)

(use-package beacon
  :config
  (beacon-mode 1))

(use-package tramp
  :init
  (setq vc-handled-backends nil)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-verbose 6))

(use-package origami
  :config
  (add-hook 'prog-mode-hook 'origami-mode))

(use-package standard-themes
  :init
  (setq custom-safe-themes t)
  :config
  (load-theme 'standard-light)
  (define-key global-map (kbd "<f9>") #'standard-themes-toggle))

(use-package dimmer
  :config
  (setq dimmer-fraction 0.5)
  (dimmer-mode 1))

(use-package darkroom)

(use-package bash-completion
  :config
  (bash-completion-setup))

(use-package free-keys)

(use-package nano-theme)

;; My own packages
(add-to-list 'load-path (concat user-emacs-directory "pp"))
(require 'pp-cpp-utils)
(require 'pp-python-utils)
(require 'pp-misc-options)
(require 'pp-avy)  
(require 'pp-keybindings)

;; Show home by default
(dired "~")
