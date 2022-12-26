(require 'ivy)
(require 'avy)
(require 'pp-utils)
(require 'cl)

(provide 'pp-avy)

(defvar last-repo nil)

(defun repo-path (for-file)
  (let ((rr (locate-dominating-file for-file ".git")))
    (if rr
	(progn
	  (message (format "pp: found repo: %s" (expand-file-name rr)))
	  (setq last-repo (expand-file-name rr)))
      (message (format "pp: no repo here. defaulting to: %s" last-repo)))
    last-repo))

(defun repo-name (for-file)
  (let ((rp (repo-path for-file)))
    (if rp
	(file-name-nondirectory (directory-file-name rp))
      nil)))

(defun get-repo-version (rp)
  (chomp (shell-command-to-string
	  (if (file-exists-p (concat rp "/.git"))
	      (format "cd %s ; git log -n 1 | head -n 1" rp)
	    nil))))

(defun get-repo-manifest-from-disk (repo)
  (if (file-exists-p (concat repo "/.git"))
      (let ((d default-directory) (r '()))
	(setq default-directory repo)
	(with-temp-buffer
	  (process-file "git" nil t nil "ls-files")
	  (setq r (split-string (buffer-string))))
	(setq default-directory d)
	r)))

(setq repo-cache (make-hash-table :test 'equal))

(defun get-repo-manifest (repo)
  (let ((rv (get-repo-version repo)))
    (let ((cached (gethash repo repo-cache nil)))
      (if (and cached (string= (car cached) rv))
	  (cdr cached)
	(let ((col (get-repo-manifest-from-disk repo)))
	  (puthash repo (cons rv col) repo-cache)
	  col)))))

(defun current-repo ()
  (repo-path default-directory))

(defun get-includes (repo-root)
  (split-string 
   (shell-command-to-string
    (format "get_includes" repo-root))))

(defun get-good-bms()
  (let ((bad-bms '("\\RTags_")))
    (cl-remove-if (lambda (bn)
		    (find-match bn bad-bms))
		  (bookmark-all-names))))

(defun get-good-buffers()
  (let ((bad-buffers '("\\` " 
		       "\\*helm"
		       "\\*"
		       "\\*Debug helm" 
		       "\\*fsm-debug" 
		       "\\*helm-mode" 
		       "\\*Echo Area" 
		       "\\*Completions"
		       "\\*Minibuf")))
    (mapcar (lambda (b)
	      (cons (buffer-name b) b))
	    (cl-remove-if (lambda (b)
			    (find-match (buffer-name b)
					bad-buffers))
			  (buffer-list)))))

(defun format-repo-candidate (path)
  (let ((file (file-name-nondirectory path))
	(dir (file-name-directory path)))
    (format "%-40s %s" file (if dir (substring dir 0 -1) "." ))))

(defun repo-candidates ()
  (let ((repo (current-repo)))
    (if repo
	(mapcar (lambda (f)
		  (cons (format-repo-candidate f)
			(concat repo f)))
		(get-repo-manifest repo)))))

(defun buffer-candidates()
  (get-good-buffers))

(defun bm-candidates()
  (mapcar (lambda (b)
	    (cons b 'bookmark))
	  (get-good-bms)))


(defun merge-candidates (buffer-candidates bm-candidates repo-candidates)
  (let ((tree (avl-tree-create 'string<))
	(result '()))
    (dolist (bp buffer-candidates)
      (let ((buffer (cdr bp)))
	(if (buffer-file-name buffer)
	    (avl-tree-enter tree (buffer-file-name buffer)))
	(push bp result)))

    (dolist (bmp bm-candidates)
      (let ((bm (car bmp)))
	(if (and (bookmark-get-filename bm)
		 (not (avl-tree-member tree (bookmark-get-filename bm))))
	    (progn
	      (avl-tree-enter tree (bookmark-get-filename bm))
	      (push bmp result)))))

    (dolist (repop repo-candidates)
      (let ((path (cdr repop)))
	(if (not (avl-tree-member tree path))
	    (progn
	      (avl-tree-enter tree path)
	      (push repop result)))))

    (sort result
	  #'(lambda (e1 e2)
	      (let ((cdr1 (cdr e1))
		    (cdr2 (cdr e2)))
		(cond
		 ;; bookmark
		 ((and (equal cdr1 'bookmark) (equal cdr2 'bookmark)) (string< (car e1) (car e2)))
		 ((and (equal cdr1 'bookmark) (bufferp cdr2)) t)
		 ((and (equal cdr1 'bookmark) (stringp cdr2)) t)
		 ;; buffer
		 ((and (bufferp cdr1) (equal cdr2 'bookmark)) nil)
		 ((and (bufferp cdr1) (bufferp cdr2)) (string< (car e1) (car e2)))
		 ((and (bufferp cdr1) (stringp cdr2)) t)
		 ;; git
		 ((and (stringp cdr1) (stringp cdr2) (string< cdr1 cdr2)))
		 ((and (stringp cdr1) (equal cdr2 'bookmark)) nil)
		 ((and (stringp cdr1) (bufferp cdr2)) nil)))))))


(defun pp-avy-switch()
  (interactive)
  (ivy-read ": "
	    (merge-candidates (buffer-candidates) (bm-candidates) (repo-candidates))
	    :action (lambda (c)
		      (let ((target (cdr c)))
			(if (bufferp target)
			    (switch-to-buffer target)
			  (if (equal target 'bookmark)
			      (bookmark-jump (car c))
			    (find-file target)))))))

(defun get-pretty-includes()
  (get-include (current-repo)))

(defun pp-avy-include()
  (interactive)
  (ivy-read "Include: "
	    (get-pretty-includes)
	    :action (lambda (file)
		      (insert (format "#include <%s>\n" (cdr file))))))

