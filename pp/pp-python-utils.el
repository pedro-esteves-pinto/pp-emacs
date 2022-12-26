(provide 'pp-python-utils)


(setq python-indent-guess-indent-offset-verbose nil) 
(add-hook 'python-mode-hook
	  (lambda ()
	    (eglot-ensure)
	    (company-mode)
	    (display-line-numbers-mode)))
