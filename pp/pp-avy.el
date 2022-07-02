(require 'ivy)
(require 'avy)
(require 'pp-utils)
(require 'cl)

(provide 'pp-avy)

(defun repo-path (for-file)
  (let ((rr (locate-dominating-file for-file ".git")))
    (if rr
	(expand-file-name rr)
      nil)))

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
  (let ((d default-directory))
    (setq default-directory repo)
    (setq r '())
    (if (file-exists-p (concat repo "/.git"))
	(setq r (process-lines "git" "ls-files")))
    (setq default-directory d)
    r))

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
    (format "%s/tools/get_includes.sh" repo-root))))

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

(defun repo-candidates ()
  (let ((repo (current-repo)))
    (if repo
	(mapcar (lambda (f)
		  (cons (file-name-nondirectory f)
			(format "%s/%s" repo f)))
		(get-repo-manifest repo)))))

(defun buffer-candidates()
  (get-good-buffers))

(defun bm-candidates()
  (mapcar (lambda (b)
	    (cons b 'bookmark))
	  (get-good-bms)))

(defun get-all-choices ()
  (let ((tree
	 (avl-tree-create '(lambda (a b) (string< (car a) (car b))))))
    (mapc (lambda (f) (avl-tree-enter tree f)) (buffer-candidates))
    (mapc (lambda (f) (avl-tree-enter tree f)) (bm-candidates))
    (mapc (lambda (f) (avl-tree-enter tree f)) (repo-candidates))
    (avl-tree-flatten tree)))


(defun pp-avy-switch()
  (interactive)
  (ivy-read ": "
	    (get-all-choices)
	    :action (lambda (c)
		      (let ((target (cdr c)))
			(if (bufferp target)
			    (switch-to-buffer target)
			  (if (equal target 'bookmark)
			      (bookmark-jump (car c))
			    (find-file target)))))))

(defun get-pretty-includes()
  (mapcar (lambda (f)
	    (cons (substring f 1 -1) f))
	  (get-includes (current-repo))))

(defun pp-avy-include()
  (interactive)
  (ivy-read "Include: "
	    (get-pretty-includes)
	    :action (lambda (file)
		      (insert (format "#include %s\n" (cdr file))))))

