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

;; My own packages
(add-to-list 'load-path (concat user-emacs-directory "pp"))
(require 'pp-misc-options)
(require 'pp-avy)  
(require 'pp-keybindings)

  
