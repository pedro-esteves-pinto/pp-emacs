(require 'pulse)
(provide 'pp-pulse)

(defun pulse-window (iterations)
  "Pulse the entire current window with a specified number of iterations."
  (let ((pulse-iterations iterations)
	(start (window-start))
	(end (window-end nil t)))
    (pulse-momentary-highlight-region start end)))

(setq pulse-delay 0.015)

(add-hook 'window-selection-change-functions
	  (lambda (_)
	    (pulse-window 20)))
