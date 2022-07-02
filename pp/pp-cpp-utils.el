(provide 'pp-cpp-utils)
(require 'pp-utils)

(defun find-cmakelist-dir (from-dir)
  "Locate first CMakeLists.txt up the current directory"
  (interactive)
  (message (format "Find CMakeLists.txt from:  %s " default-directory))
  (let ((d (locate-last-dominating-file from-dir "CMakeLists.txt")))
    (if d
	(expand-file-name d)
      nil)))

(defun pp-cpp-build(release)
  "Find projet root and start compilation from there"
  (interactive)
  (message "--------------------------------------------------------------------------------")
  (message (format "pp-custom-build-location: %s" pp-custom-build-location))
  (message (format "pp-custom-build-cmd: %s" pp-custom-build-cmd))
  (message (format "pp-custom-release-build-cmd: %s" pp-custom-release-build-cmd))
  (message (format "Default directory is: %s " default-directory))
  (let ((cm-dir (find-cmakelist-dir default-directory)))
    (message (format "cm-dir is %s" cm-dir))
    (if (and cm-dir (boundp 'pp-custom-build-location))
	(let ((cmd (format "cd %s/%s ; %s"
			   cm-dir
			   pp-custom-build-location
			   (if release
			       pp-custom-release-build-cmd
			     pp-custom-build-cmd))))
	  (message "pp-cpp-build command: %s" cmd)
	  (my-compile cmd ))
      (cmake-ide-compile))))

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

(defun c++-get-file-name-extension ()
  "Extract the extension (.cpp in X.cpp) from the current buffers file name if possible"
  (interactive)
  (let (stem-end file-name)
    (setq file-name (file-name-nondirectory  buffer-file-name))
    (setq stem-end (string-match "\\.\\(cpp\\|C\\|\\H|cc\\|h\\)$" file-name ))
    (if stem-end
	(substring file-name stem-end))))

(defun c++-get-file-name-stem ()
  "Extract the stem (X in X.cpp, X.h, X.cc) from the file name if possible"
  (interactive)
  (let (stem-end file-name)
    (setq file-name (file-name-nondirectory  buffer-file-name))
    (setq stem-end (string-match "\\.\\(cpp\\|cc\\|h\\)$" file-name ))
    (if stem-end
	(substring file-name 0 stem-end))))

(defun c++-cycle-to (extension)
  (let (directory file-name)
    (setq directory (file-name-directory  buffer-file-name))
    (setq file-name (concat directory (c++-get-file-name-stem) extension))
    (if (get-file-buffer file-name)
	(progn
	  (switch-to-buffer (get-file-buffer file-name))
	  (pulse-window))
      (if (file-exists-p file-name)
	  (progn
	    (find-file file-name)
	    (pulse-window))))))

(defun c++-cycle-through-related-files ()
  "Cycle from .h to .cc to .cpp files"
  (interactive)
  (let (extension)
    (setq extension (c++-get-file-name-extension))
    (if (string=(downcase extension) ".h") 

	(or (c++-cycle-to ".cc") 
	    (c++-cycle-to ".cpp"))
      (if (string=(downcase extension) ".cc")
	  (or (c++-cycle-to ".cpp") 
	      (c++-cycle-to ".h"))
	(if (string=(downcase extension) ".cpp")
	    (or (c++-cycle-to ".h") 
		(c++-cycle-to ".cc"))
	  (message "File name does not end in .h, .cc or .cpp"))))))

(defun c++-get-file-name-stem ()
  "Extract the stem (X in X.cpp, X.h, X.cc) from the file name if possible"
  (interactive)
  (let (stem-end file-name)
    (setq file-name (file-name-nondirectory  buffer-file-name))
    (setq stem-end (string-match "\\.\\(cpp\\|cc\\|h\\)$" file-name ))
    (if stem-end
	(substring file-name 0 stem-end))))

(defun c++-insert-file-name-stem ()
  "Insert in the current buffer the result of c++-get-file-name-stem"
  (interactive)
  (if (c++-get-file-name-stem)
      (insert (c++-get-file-name-stem))
    (message "File name does not end in .h, .cc or .cpp")))

(defun my-c++-setup ()
  "changes to the default C++ setup"
  (interactive)
  (c-set-style "stroustrup")
  (modify-syntax-entry ?_ "w")
  (setq c-basic-offset 3) ; tab size = 3
  (c-set-offset 'innamespace 0) ; Namespace indentation is 0
  (c-set-offset 'inline-open 0 nil)
  (setq indent-tabs-mode nil) ; Use spaces not tabs when indenting
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

(defun clang-format()
  "Run clang-format on current file"
  (interactive)
  (if (string-match "\\.\\(cpp\\|cc\\|h\\)$" buffer-file-name )
      (progn
	(save-buffer)
	(shell-command (format "clang-format -style=file -i %s" buffer-file-name))
	(revert-buffer 1 1))))


(defvar pp-custom-build-location nil)
(defvar pp-custom-build-cmd nil)
(put 'pp-custom-build-location 'safe-local-variable (lambda (x) t))
(put 'pp-custom-build-cmd 'safe-local-variable (lambda (x) t))
(put 'pp-custom-release-build-cmd 'safe-local-variable (lambda (x) t))
