(provide 'pp-misc-options)

(load-theme 'modus-operandi)
(setq myfont nil)
(if myfont
    (setq default-frame-alist (list (cons 'font myfont ))))
(set-face-attribute 'default nil :height 150)
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq make-backup-files nil)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(show-paren-mode)
(column-number-mode t)
(mouse-avoidance-mode 'animate)                  ; Move pointer to avoid collision with point
(global-hl-line-mode)
(global-display-line-numbers-mode)

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
(setq window-combination-resize t)

;; set dired to show directories first, then files
;; The mac's ls command does not support --group-directories-first so we switch to gls
(if (string-equal system-type "darwin")
    (setq insert-directory-program "gls" dired-use-ls-dired t))
(setq dired-listing-switches "-lL --group-directories-first")

(defvar parameters
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t))))

