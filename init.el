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

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(use-package evil
  :init
  ;; (setq evil-want-integration t)
  ;; ;; http://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working
  ;; (setq evil-want-C-i-jump nil)
  ;; See https://github.com/emacs-evil/evil-collection/issues/60 for more details.
  (setq evil-want-keybinding nil)
  :config
  (add-to-list 'evil-insert-state-modes 'shell-mode)
  (evil-mode 1))

(use-package vterm
	     :after evil
	     :commands vterm
	     :config
	     (define-key vterm-mode-map (kbd "<C-backspace>")
			 (lambda () (interactive (vterm-send-key (kbd "C-w")))))
	     (define-key vterm-mode-map (kbd "<M-backspace>")
			 (lambda () (interactive (vterm-send-key (kbd "C-w")))))
	     (define-key vterm-mode-map (kbd "<f12>") 'pp-avy-switch)
	     (evil-set-initial-state 'vterm-mode 'emacs))


(straight-use-package 'project)

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection--supported-modes (delete 'vterm evil-collection--supported-modes))
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
  (setq org-refile-targets '((nil :maxlevel . 3)))
  (setq org-agenda-span 14)
  (setq org-confirm-shell-link-function nil)
  (setq org-capture-templates '(("j" "Journal" plain
				 (file+olp+datetree "~/notes/journal/personal-journal.org")
				 "%i%?")
				("n" "Note" entry
				 (file+headline "~/notes/planning/plan.org" "Inbox")
				 "* %?\n %i")))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  ;; dot
  :config
  (org-add-link-type "bookmark" 'org-bookmark-open)
  (defun org-bookmark-open (bookmark)
    "Open the Emacs bookmark with the given name."
    (bookmark-jump bookmark))
  (org-add-link-type "rshell" 'org-rshell-open)
  (defun org-rshell-open (remote)
    (pp-remote-shell remote))
  )

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
  (setq eglot-server-programs '(
				(c++-mode . ("~/tools/pp_clangd"))
				(python-mode . ("pyright-langserver" "--stdio"))))
  (setq eglot-ignored-server-capabilities '(:hoverProvider :signatureHelpProvider)))

(use-package eglot-booster
        :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
	:after eglot
	:config	(eglot-booster-mode))

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
(use-package solarized-theme)
(use-package doom-themes)
(use-package doom-themes)
(use-package modus-themes)
(use-package denote
	     :config
	     (setq denote-directory "~/notes"))
(use-package protobuf-mode)
(use-package org-modern
	     :config
	     (add-hook 'org-mode-hook #'org-modern-mode)
	     (add-hook 'org-agenda-finalize #'org-modern-agenda))
(use-package ef-themes)
(use-package markdown-mode)
(use-package orgit)
(use-package ob-mermaid
	     :config
	     (setq ob-mermaid-cli-path "~/tools/mermaid"))

	(org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t) 
     (C . t)
     (plantuml . t)	
     (sql . t)
     (mermaid . t)
     (dot . t)))
;; My own packages
(add-to-list 'load-path (concat user-emacs-directory "pp"))
(require 'pp-cpp-utils)
(require 'pp-python-utils)
(require 'pp-misc-options)
(require 'pp-avy)  
(require 'pp-keybindings)
(require 'pp-pulse)

;; Show home by default
(dired "~")
