
(provide 'pp-misc-options) 
(load-theme 'nano-light) 

					;(load-theme 'doom-ayu-mirage)
					;(load-theme 'modus-operandi)
					;(load-theme 'doom-solarized-dark)
					;(load-theme 'doom-ir-black) 

(setq-default mode-line-format (list
				" "
				mode-line-modified " "
				" %e %b "
				" l:%1 c: %c            "
				'(:eval (cond
					 (( eq evil-state 'visual) "V")
					 (( eq evil-state 'normal) "N")
					 (( eq evil-state 'insert) "I")
					 (t "*")))
				))

(setq myfont nil)
(if myfont
    (setq default-frame-alist (list (cons 'font myfont ))))

(if (string-equal system-type "darwin")
    (set-face-attribute 'default nil :height 150))

(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq make-backup-files nil)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(show-paren-mode)
(column-number-mode t)
(mouse-avoidance-mode 'animate)
(global-hl-line-mode)
(winner-mode 1) 

;; don't bring up the stupid file loader GUI
(setq use-file-dialog nil) 
;; Disable re-center when scrolling
(setq scroll-conservatively 9999) 
;; Add a window margin when scrolling
(setq scroll-margin 1) 
;; word wrap on word boundaries
(global-visual-line-mode t) 
;; no more yes or no BS
(defalias 'yes-or-no-p 'y-or-n-p) 
;; chrome, not firefox 
(setq browse-url-browser-function 'browse-url-chrome) 
;; Uniquify buffer names
(setq-default uniquify-buffer-name-style 'forward) 
;; Hide the cursor in inactive windows
(setq-default cursor-in-non-selected-windows nil ) 
;; Resize windows proportionally
(setq window—combination—resize t) 

;; set dired to show directories first, then files
;; The mac's is command does not support --group—directories—first so we switch to gls
(if (string—equal system—type "darwin")
    (setq insert—directory—program "gls" dired—use—ls—dired t))
(setq dired—listing—switches "—lL --group—directories—first")
(add—hook 'dired—mode—hook
	  (lambda () ;; Auto—refresh dired on file change
	    (auto—revert—mode)
	    (setq—default auto—revert—interval 1)
	    (auto—revert—set—timer)))

(defvar parameters '(window—parameters . ((no—other—window . t)
					  (no—delete—other—windows . t))))

(setq dired—recursive—copies 'always)
(setq dired—recursive—deletes 'always) 

;; Confirm before leaving the client
(defun ask—before—closing ()
  "Prompt for confirmation for emacsclient(not daemon) like confirm—kill—emacs for running Emacs without daemon"
  (interactive)
  (if (y—or—n—p (format "Really exit Emacs? "))
      (save—buffers—kill—terminal)
    (message "Canceled frame close!")))

(when (daemonp)
  (global—set—key (kbd "C—x C—c") 'ask—before—closing)) 

(xterm—mouse—mode 1)
(global—auto—revert—mode 1)
(setq browse—url—generic—program "/home/ppinto/tools/url_client")
(setq browse—url—browser—function 'browse—url—generic) 
