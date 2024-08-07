(provide 'pp-cpp-utils)
(require 'pp-utils)

(defun find-pp-proj (from-dir)
  "Locate last pp-proj up the current directory"
  (interactive)
  (message (format "Find pp-proj.el from: %s " default-directory))
  (let ((d (locate-last-dominating-file from-dir ".pp-proj.el")))
    (if d (concat (expand-file-name d) ".pp-proj.e1"))))

(defun find-cmakelist-dir (from-dir)
  "Locate last CMakeLists.txt up the current directory"
  (interactive)
  (message (format "Find CMakeLists.txt from: %s " default-directory))
  (let ((d (locate-last-dominating-file from-dir "CMakeLists.txt")))
    (if d
	(expand-file-name d)
      nil))) 

(defvar pp-build-dir nil)
(defvar pp-build-cmd nil)
(defvar pp-release-build-cmd nil)

(defun pp-cpp-build(release)
  "Find projet root and start compilation from there"
  (interactive)
  (let ((pp-proj (find-pp-proj default-directory)))
    (if pp-proj
	(progn
	  (message (format "found: %s" pp-proj))
	  (load pp-proj)
	  (setq pp-build-dir (file-name-directory pp-proj)))))
  (message "--------------------------------------------------------------------------------")
  (message (format "pp-build-dir: %s" pp-build-dir))
  (message (format "pp-build-cmd: %s" pp-build-cmd))
  (message (format "pp-release-build-cmd: %s" pp-release-build-cmd))
  (message (format "Default directory is: %s " default-directory)) 

  (setq build-cmd (if release pp-release-build-cmd pp-build-cmd))

  (if (and pp-build-dir build-cmd)
      (let ((cmd (format "cd %s ; %s" pp-build-dir build-cmd)))
	(message (format "compile command: %s" cmd))
	(my-compile cmd ))
    (message "Cannot compile from here")))

(defun pp-compile-finish (buffer outstr)
  (if (string-match "finished" outstr)
      (delete-windows-on buffer)))

(add-hook 'compilation-finish-functions 'pp-compile-finish)

(defadvice compilation-start
    (around inhibit-display
	    (command &optional mode name-function highlight-regexp))
  (if (not (string-match "^\\(find\\|grep\\|ag\\)" command))
      (flet ((display-buffer)
	     (set-window-point)
	     (goto-char))
	(fset 'display-buffer 'ignore)
	(fset 'goto-char 'ignore)
	(fset 'set-window-point 'ignore)
	(save-window-excursion
	  ad-do-it))
    ad-do-it)) 

(defun my-compile (cmd)
  (ad-activate 'compilation-start)
  (compile cmd)
  (if (not (get-buffer-window "*compilation*"))
      (set-window-buffer (split-window (frame-root-window) -15) "*compilation*"))
  (ad-deactivate 'compilation-start))

(defun my-next-error () 
  "Move point to next error and highlight it"
  (interactive)
  (next-error)
  (with-current-buffer "*compilation*"
    (message (thing-at-point 'line t))))

(defun my-previous-error () 
  "Move point to previous error and highlight it"
  (interactive)
  (previous-error)
  (with-current-buffer "*compilation*"
    (message (thing-at-point 'line t))))

(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

(defun my-compilation-mode-hook ()
  (setq compilation-scroll-output 'first-error) 
  (setq compilation-ask-about-save nil)
  (setq compilation-always-kill t)
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil))

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

;; .h, .cc should be handled in C++ mode
(setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cc\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ipp\\'" . c++-mode) auto-mode-alist))


(defun pp-next-cpp-extension (extension)
  (cond
   ((string= extension "h") "H")
   ((string= extension "H") "cc")
   ((string= extension "cc") "cpp")
   ((string= extension "cpp") "C")
   ((string= extension "C") "h")))

(defun pp-cpp-extension (cpp-path)
  (let ((stem-end (string-match "\\.\\(cpp\\|C\\|\\H|cc\\|h\\)$" cpp-path)))
    (if stem-end
	(substring cpp-path (+ 1 stem-end)))))

(defun pp-cpp-stem (cpp-path)
  (let ((extension (pp-cpp-extension cpp-path)))
    (if extension
	(substring cpp-path 0
		   (- (length cpp-path) (length extension) 1)))))

(defun pp-find-first-related-cpp-path (cpp-path &optional last-extension-tried)
  (let* ((extension (pp-cpp-extension cpp-path))
	 (stem (pp-cpp-stem cpp-path))
	 (extension-to-try (pp-next-cpp-extension (if last-extension-tried
						      last-extension-tried
						    extension))))
    (message extension)
    (message stem)
    (message extension-to-try)
    
    (if (and extension-to-try
	     (not (string= extension-to-try extension)))
	(let ((candidate (concat stem "." extension-to-try)))
	  (if (or (get-file-buffer candidate)
		  (file-exists-p candidate))
	      candidate
	    (pp-find-first-related-cpp-path cpp-path extension-to-try))))))

(defun pp-cpp-goto-related-file()
  "Cycle between .h,.H,cc,cpp,C files with the same stem in their names"
  (interactive)
  (message "--------------------------------------------------------------------------------")
  (let ((related-file (pp-find-first-related-cpp-path buffer-file-name)))
    (if related-file
	(if (get-file-buffer related-file)
	    (switch-to-buffer (get-file-buffer related-file))
	  (find-file related-file)))))

(defun pp-cpp-insert-file-name-stem ()
  "Insert in the current buffer the result of c++-get-file-name-stem"
  (interactive)
  (if (pp-cpp-stem (buffer-file-name))
      (insert (file-name-nondirectory (pp-cpp-stem (buffer-file-name))))
    (message "Not a C++ file")))

(defun my-c++-setup ()
  "changes to the default C++ setup"
  (interactive)
  (c-set-style "stroustrup")
  (modify-syntax-entry ?_ "w")
  (setq c-basic-offset 2) ; tab size = 2
  (c-set-offset 'innamespace 2) ; Namespace indentation is 0
  (c-set-offset 'inline-open 0 nil)
  (setq indent-tabs-mode nil) ; Use spaces not tabs when indenting
  (eglot-ensure)
  (display-line-numbers-mode)
  (company-mode 1)
  (font-lock-add-keywords 'c++-mode
			  '(("foreach" . font-lock-keyword-face)
			    ("nullptr" . font-lock-keyword-face)
			    ("co_await" . font-lock-keyword-face)
			    ("co_resume" . font-lock-keyword-face)
			    ("co_return" . font-lock-keyword-face)
			    ("co_yeld" . font-lock-keyword-face)
			    ("constexpr" . font-lock-keyword-face)
			    ("override" . font-lock-keyword-face)
			    ))
  )

(add-hook 'c++-mode-hook (lambda () (my-c++-setup)))


