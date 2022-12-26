(provide 'pp-utils)
(require 'bookmark)
(require 'pulse)
(require 'ansi-color)
(require 'cl)

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string))) 

;; string utils
(defun find-match (str rx-list)
  (if (null rx-list)
      nil
    (if (string-match (car rx-list) str)
	(car rx-list)
      (find-match str (cdr rx-list)))))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING. White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\'[ \t\n]*" ""
			    (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun string/starts-with (s arg)
  "returns non-nil if string S starts with ARG. Else nil."
  (cond ((>= (length s) (length arg))
	 (string-equal (substring s 0 (length arg)) arg))
	(t nil)))

(defun string/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (if (> (length ending) (length s))
      nil
    (let ((elength (length ending)))
      (string= (substring s (- 0 elength)) ending))))

(defun string/drop-prefix (s prefix)
  (if (string/starts-with s prefix)
      (substring s (length prefix))))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(defun shell-command-on-buffer (command)
  (interactive "sShell command on buffer: ")
  (shell-command-on-region (point-min) (point-max) command t))

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
	(progn
	  (set (make-local-variable 'dired-dotfiles-show-p) nil)
	  (message "h")
	  (dired-mark-files-regexp "^\\\.")
	  (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
	     (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defun indent-whole-buffer()
  "Indent whole buffer (using indent-region)."
  (interactive)
  (save-excursion
    (normal-mode)
    (mark-whole-buffer)
    (call-interactively 'indent-region))
  (message "%s" "Indented whole buffer."))

;; pulse target window when switching
(setq pulse-iterations 2)
(setq pulse-delay .05)
(defun pulse-window ()
  (pulse-momentary-highlight-region (point-min) (point-max)))

(defun pp-toggle-window-dedicated ()
  "Toggle whether or not the current window is dedicated to its buffer"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
	 (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Marking window '%s' as dedicated"
     "Un-dedicating window '%s'")
   (current-buffer)))

(defun ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (setq buffer-read-only nil) ;; Not need to edit the content, just read mode (added)
  (local-set-key "q" 'bury-buffer) ;; Nice to have the option to bury the buffer (added)
  (setq lower32 '("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel"
		  "bs" "ht" "nl" "vt" "np" "cr" "so" "si"
		  "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
		  "can" "em" "sub" "esc" "fs" "gs" "rs" "us"
		  ))
  (save-excursion (let ((i -1))
		    (insert "ASCII characters 0 thru 127.\n\n")
		    (insert " Hex Dec Char' Hex Dec Charl Hex Dec Charl Hex Dec Char\n")
		    (while (< i 31)
		      (insert (format "%4x %4d %4s 1 %4x %4d %4s 1 %4x %4d %4s 1 %4x %4d %4s\n"
				      (setq i (+ 1 i)) i (elt lower32 i)
				      (setq i (+ 32 i)) i (single-key-description i)
				      (setq i (+ 32 i)) i (single-key-description i)
				      (setq i (+ 32 i)) i (single-key-description i)))
		      (setq i (- i 96)))))) 

(defun locate-last-dominating-file (from-dir file)
  (message (format "%s %s" from-dir file))
  (if (string= (file-truename from-dir) "/")
      nil
    (let ((d (locate-dominating-file from-dir file)))
      (if (and d (not (locate-dominating-file (concat d "/..") file)))
	  (locate-last-dominating-file (concat d "/..") file)))))

(defun pp-show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name)) (message (buffer-file-name))) 

(defun pp-copy-file-name-as-org-link-to-clipboard ()
  "Copy the current buffer file name to the clipboard "
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (let* ((clean-filename (file-name-nondirectory filename))
	     (org-link (format "[[file+emacs:%s][%s]]"
			       filename (file-name-sans-extension clean-filename))))
	(kill-new org-link)
	(message "Copied buffer file name 196s1 to the clipboard." clean-filename)))))


(defun pp-copy-region-to-pasteboard-file (beg end &optional type register yank-handler)
  (write-region beg end "~/.pp-pasteboard" t)
  (write-region "\n{{{ END_OF_PASTE_BLOCK }}}\n" nil "~/.pp-pasteboard" 'append))

(advice-add 'evil-yank :after 'pp-copy-region-to-pasteboard-file) 
