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

(use-package evil
  :init
  (setq evil-want-integration t)
  ;; http://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working
  (setq evil-want-C-i-jump nil)
  ;; See https://github.com/emacs-evil/evil-collection/issues/60 for more details.
  (setq evil-want-keybinding nil)
  :config (evil-mode 1))

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
  (org-roam-directory "~/org-roam")
  :config
  (org-roam-db-autosync-mode))

(use-package magit)
(use-package ag)
(use-package buffer-move)
(use-package cmake-mode)
(use-package yaml-mode)
(use-package ace-window)

(use-package solarized-theme
  :init
  (setq solarized-scale-org-headlines nil)
  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0))


(use-package lsp-mode :ensure t
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq treemacs-space-between-root-nodes nil)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq lsp-idle-delay 0.0)
  (setq lsp-file-watch-threshold 20000)
  (add-hook 'c++-mode-hook 'lsp)
  )

;; (use-package eldoc
;;   :init
;;   (setq eldoc-echo-area-use-multiline-p nil))

;; (use-package eglot
;;   :init
;;   (add-hook 'c++-mode-hook
;; 	    #'(lambda ()
;; 		(eglot-ensure)
;; 		(eldoc-mode 0))))


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
(use-package vterm)
(use-package company)

;; My own packages
(add-to-list 'load-path (concat user-emacs-directory "pp"))

(require 'pp-cpp-utils)
(require 'pp-misc-options)
(require 'pp-avy)  
(require 'pp-keybindings)

;; Show home by default
(dired "~")
