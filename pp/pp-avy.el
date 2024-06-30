(require 'ivy)
(require 'avy)
(require 'cl)
(require 'avl-tree)
(require 'denote)

(require 'pp-utils)
(require 'pp-pulse)

(provide 'pp-avy)

(defvar-local last-visited-time nil "The time when this buffer was last visited")

(defun update-last-visited-time ()
  "Update the last visited time of a buffer"
  (setq last-visited-time (current-time)))

(add-hook 'buffer-list-update-hook 'update-last-visited-time)

(defun get-last-visited-time (buffer-name)
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
	(if last-visited-time
	    last-visited-time
	  nil))))

(defun compare-buffers (buffer1 buffer2)
  (if (and (get-last-visited-time buffer1)
	   (get-last-visited-time buffer2))
      (time-less-p (get-last-visited-time buffer2)
		   (get-last-visited-time buffer1))
    (if (get-last-visited-time buffer1)
	1
      (if (get-last-visited-time buffer22)
	  0
	(string< (buffer-name buffer1)
		 (buffer-name buffer2))))))
	   

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
			    (or (eq (current-buffer) b)
				(find-match (buffer-name b)
					    bad-buffers)))
			  (buffer-list)))))

(defun format-repo-candidate (path)
  (let ((file (file-name-nondirectory path))
	(dir (file-name-directory path)))
    (format "%-40s %s" file (if dir (substring dir 0 -1) "."))))

(defun repo-candidates ()
  (let ((repo (current-repo)))
    (if repo
	(mapcar (lambda (f)
		  (cons (format-repo-candidate f)
			(concat repo f)))
		(get-repo-manifest repo)))))

(defun denote-candidates()
  (mapcar (lambda (denote-file)
	    (cons (format "Note: %s" (denote-retrieve-filename-title denote-file))
		  denote-file))
	  (denote-directory-text-only-files)))

(defun buffer-candidates()
  (get-good-buffers))

(defun bm-candidates()
  (mapcar (lambda (b)
	    (cons b 'bookmark))
	  (get-good-bms)))


(defun merge-candidates (buffer-candidates bm-candidates repo-candidates denote-candidates)
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
		 ((and (bufferp cdr1) (bufferp cdr2)) (compare-buffers cdr1 cdr2 ))
		 ((and (bufferp cdr1) (stringp cdr2)) t)
		 ;; git
		 ((and (stringp cdr1) (stringp cdr2) (string< cdr1 cdr2)))
		 ((and (stringp cdr1) (equal cdr2 'bookmark)) nil)
		 ((and (stringp cdr1) (bufferp cdr2)) nil)))))
  (append result denote-candidates)
  ))


(defun pp-avy-switch()
  (interactive)
  (unless (eq last-command 'pp-avy-switch)
    (ivy-read ": "
	      (merge-candidates (buffer-candidates) (bm-candidates) (repo-candidates) (denote-candidates))
	      :action (lambda (c)
			(let ((target (cdr c)))
			  (if (bufferp target)
			      (switch-to-buffer target)
			    (if (equal target 'bookmark)
				(bookmark-jump (car c))
			      (find-file target))))))))

