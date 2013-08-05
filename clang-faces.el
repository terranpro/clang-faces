;;; clang-faces.el --- 
;;
;; Copyright (C) 2013 Brian Fransioli
;;
;; Author: Brian Fransioli <assem@terranpro.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Fantastic syntax highlighting directly from the horses mouth (clang).
;; 
;;; Usage:
;; `clang-faces-mode' after setting `clang-faces-client-exec' and Enjoy!
;;
;;; TODO: 
;; Buggy =(
;;
;;; Code:

(defvar clang-faces-client-exec 
  "~/code/clang-faces/build/emacs-clang-syntaxhl")

(defvar-local clang-faces-process nil
  "Process object to the clang-faces executable")

(defvar-local clang-faces-fontification-data nil)
(defvar-local clang-faces-fontification-data-incoming nil)

(defvar-local clang-faces-parsed-data nil)

(defvar-local clang-faces-status 'idle
  "Marking whether busy or idle.")

(defvar-local clang-faces-fontify-timer nil)
(defvar-local clang-faces-reparse-timer nil)

(defvar-local clang-faces-fontify-region-queue nil)
(defvar-local clang-faces-hilight-request-queue nil)

(defvar-local clang-faces-last-tick nil)

(defvar-local clang-faces-delta-beg nil)
(defvar-local clang-faces-delta-end nil)
(defvar-local clang-faces-valid-pt 0)
(defvar-local clang-faces-current-change nil)

(defconst clang-faces-output-finished-marker "!!!!$$$$!!!!")

;(defface )
(defface clang-faces-keyword-face
  '((t :inherit font-lock-keyword-face))
  "")

(defface clang-faces-literal-face
  '((t :inherit font-lock-string-face))
  "")

(defface clang-faces-type-face
  '((t :inherit font-lock-type-face))
  "")

(defface clang-faces-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "")

(defface clang-faces-comment-face
  '((t :inherit font-lock-comment-face))
  "")

(defface clang-faces-function-name-face
  '((t :inherit font-lock-function-name-face))
  "")
(set-face-attribute 'clang-faces-function-name-face nil 
		    :inherit font-lock-function-name-face)

(defface clang-faces-namespace-face
  '((t :inherit warning))
  "")
(set-face-attribute 'clang-faces-namespace-face nil 
		    :inherit font-lock-type-face
		    :bold t)

(defvar clang-faces-type-face-alist nil)
(setq clang-faces-type-face-alist
      '(("Punctuation" . default)
	("Identifier" . clang-faces-type-face)
	("Variable" . clang-faces-variable-name-face)
	("Literal" . clang-faces-literal-face)
	("Comment" . clang-faces-comment-face)
	("Keyword" . clang-faces-keyword-face)
	("Function" . clang-faces-function-name-face)
	("Namespace" . clang-faces-namespace-face)))

(defvar clang-faces-output-regex nil)
(setq clang-faces-output-regex
      (concat (rx bol
		  (group (one-or-more digit))
		  ":"
		  (group (one-or-more digit))
		  " @ ")
	      "\\(" 
	      (regexp-opt (mapcar 'car clang-faces-type-face-alist))
	      "\\)"))

(defun clang-faces-color-region (buf type start end)
  (let ((type-face (or (cdr (assoc type clang-faces-type-face-alist))
		       'default)))
    (put-text-property start end 'font-lock-face type-face buf)
;    (put-text-property start end 'fontified nil buf)
    ))

(defun clang-faces-fontify-region-worker (start end buf &optional hardstoppt)
  (message (format "Fontifying in %s" buf))
  (with-silent-modifications
    (mapcar
     #'(lambda (e)
	 (message (format "Element: %d %d %s" (car e) (cadr e) (caddr e)))
	 (clang-faces-color-region buf
				   (caddr e)
				   (car e)
				   (cadr e)))
     (loop for elem in clang-faces-parsed-data
	   for s = (car elem)
	   for e = (cadr elem)
	   with max = (or hardstoppt (1+ (buffer-size buf)))
	   with win-end = (window-end (get-buffer-window buf))
	   if (and (>= s start) (<= s end) (<= s max) (<= e max))
	   ;;if (and (>= s start) (<= s win-end))
	   collect elem))))

(defun clang-faces-fontify-queue ()
  (unwind-protect
      (mapcar #'(lambda (e) 
		  (message (format "Fontifying %d -> %d" (car e) (cadr e)))
		  (clang-faces-fontify-region-worker (car e) (cadr e)
						     (caddr e)
						     clang-faces-min-point))
	      clang-faces-fontify-region-queue)
    (setq clang-faces-fontify-region-queue nil)
    (setq clang-faces-min-point 1000000)))

(defun clang-faces-fontify-region (&optional start end verbose)
  "Puts all fontification requests into a queue to be fontified
later when clang-faces process finishes and returns new info."
  ;; (message (format "Fontify Region! %d %d" start end))

  ;; (setq clang-faces-fontify-region-queue 
  ;; 	(append 
  ;; 	 clang-faces-fontify-region-queue
  ;; 	 (list (list start end (current-buffer)))))
  (font-lock-default-fontify-region start end verbose))

(defun clang-faces-fontify-buffer (&optional buf)
  (interactive)
  (clang-faces-fontify-region-worker (point-min) (point-max) 
				     (or buf (current-buffer))))

(defun clang-faces-on-hilight-returns (proc)
  "To be called by the process filter when it has collected all
process output and parsed the incoming data.  Pops the first
element off the hilight queue and attempts to fontify the delta
region."
  (let ((buf (process-buffer proc)) elt beg end)
    (setq elt (pop clang-faces-hilight-request-queue))
    (when elt
      (save-excursion
	(set-buffer buf)
	(setq beg (progn (goto-char (car elt))
			 (point-at-bol)))
	(setq end (progn (goto-char (cadr elt))
			 (point))))
      (message (format "Highlighting %d to %d" beg end))
      (clang-faces-fontify-region-worker beg end buf)
      (setq clang-faces-valid-pt end))))

(defun clang-faces-parse-incoming-data ()
  (let ((data clang-faces-fontification-data))
    (setq 
     clang-faces-parsed-data
     (with-temp-buffer
       (insert data)
       (goto-char (point-min))
       (let ((regex clang-faces-output-regex)
	     pdata)
	 (while (search-forward-regexp regex (point-max) t)
	   (setq pdata
		 (append
		  pdata
		  (list
		   (list
		    (1+ (string-to-number (match-string 1)))
		    (1+ (string-to-number (match-string 2)))
		    (match-string 3))))))
	 pdata))))
  (setq clang-faces-fontification-data nil))

(defun clang-faces-proc-send-reparse (proc bufstr)
  (process-send-string proc "\nREPARSE\n")
  (process-send-string 
   proc
   (concat bufstr clang-faces-output-finished-marker "\n")))

(defun clang-faces-request-hilight-worker (proc)
  (with-current-buffer (process-buffer proc)
    (when (and (eq clang-faces-status 'idle)
	       clang-faces-hilight-request-queue)
      (message "Reparse request!")
      (setq clang-faces-status 'busy)
      ;;(setq clang-faces-last-tick (buffer-modified-tick))
      (clang-faces-proc-send-reparse 
       proc (buffer-substring-no-properties (point-min) 
					    (point-max))))))

(defmacro safe1- (n)
  `(if ,n (1- ,n) nil))

(defun clang-faces-process-filter (process output)
  ;;(message "Inside Process Filter!")
  (let ((off (string-match 
	      (concat "^" (regexp-quote
			   clang-faces-output-finished-marker))
	      output)))
    (setq clang-faces-fontification-data-incoming
	  (concat clang-faces-fontification-data-incoming
		  (substring output 0 (safe1- off))))

    (if off (progn
	      (message "End of Output Detected!")
	      (setq clang-faces-status 'idle
		    clang-faces-fontification-data 
		    clang-faces-fontification-data-incoming
		    clang-faces-fontification-data-incoming nil)
	      (clang-faces-parse-incoming-data)
	      
	      (clang-faces-on-hilight-returns process)

	      (if clang-faces-hilight-request-queue
		  (clang-faces-request-hilight-worker process))

	      ;(clang-faces-fontify-queue)
	      ))
    t))

(defun clang-faces-kill-process ()
  (when clang-faces-process
    (process-kill-without-query clang-faces-process)
    (setq clang-faces-process nil)))

(defun clang-faces-relaunch-process ()
  (interactive)
  (clang-faces-kill-process)
  (clang-faces-launch-process))

(defun clang-faces-launch-process ()
  (interactive)
  (let* ((buf (current-buffer))
	 (file (or (buffer-file-name)
		   (concat default-directory (buffer-name buf))))
	 (outbuf buf)
	 (cmd (concat clang-faces-client-exec 
		      " "
		      file)))
    (setq clang-faces-process
	  (start-process "clang-faces" outbuf clang-faces-client-exec
			 file))
    (set-process-filter clang-faces-process 
			(function clang-faces-process-filter))
    (message "Clang Faces Process Launched!")))

(defun clang-faces-post-command ()
  ;; (pp last-command)
  ;; (pp this-command)
  ;; (pp (first buffer-undo-list))
  ;; (setq clang-faces-point-delta (+ clang-faces-point-delta
  ;; 				   (- (point) 
  ;; 				      clang-faces-last-point)))
  ;; (message "Last P: %d This P: %d Running Delta: %d"
  ;; 	   clang-faces-last-point (point) clang-faces-point-delta)
  ;; (when (eq last-command 'self-insert-command)
  ;;   (setq clang-faces-last-point (point))
  ;;   (if (memq last-input-event '(return kp-enter))
  ;; 	(setq clang-faces-min-point (point))
  ;;     (setq clang-faces-min-point (min clang-faces-last-point
  ;; 				       clang-faces-min-point)))
  ;;   (message (format "Point = %d Min = %d" (point) clang-faces-min-point))
  ;;   )
  ;(setq clang-faces-parsed-data (clang-faces-adjust-delta))
  )

(defun clang-faces-request-hilight ()
  (let* ((beg clang-faces-delta-beg)
	 (end clang-faces-delta-end)
	 (entry (list beg end))
	 (proc clang-faces-process))
    (setq clang-faces-hilight-request-queue
	  (append clang-faces-hilight-request-queue
		  (list entry)))
    (setq clang-faces-delta-beg nil)
    (clang-faces-request-hilight-worker proc)))

(defun clang-faces-before-change (beg end)
  (if (eq (- end beg) 0)
      (setq clang-faces-current-change 'insertion)
    (setq clang-faces-current-change 'deletion))
  ;; (message 
  ;;  (format "trigger: %s %d %d before: %s"
  ;; 	   this-command
  ;; 	   beg end (buffer-substring-no-properties beg end)))

)

(defmacro mems (elt str)
  `(let ((r nil)
	 (n 0)
	 (len (length ,str)))
     (while (and (null r)
		 (< n len))
       (if (eq (aref ,str n) ,elt)
	   (setq r t)
	 (setq n (1+ n))))
     r))

(defmacro memls (elts str)
  `(let ((er nil)
	 (en 0)
	 (elen (length ,elts)))
     (while (and (null er)
		 (< en elen))
       (if (mems (aref ,elts en) ,str)
	   (setq er t)
	 (setq en (1+ en))))
     er))

(defun clang-faces-after-change (beg end oldlen)
  (when (eq clang-faces-current-change 'insertion)
    (message (format
	      "Inserted text: %s" 
	      (buffer-substring-no-properties beg end)))
    (if clang-faces-delta-beg
	;; Delta Start Region already exists... check for stop char
	(when (and clang-faces-delta-beg
		   (not (null this-command))
		   (memls "(){};<>:"
			  (buffer-substring-no-properties beg end)))
	  ;; Stop character found, mark end of delta region
	  (setq clang-faces-delta-end end)
	  (setq clang-faces-valid-pt (min clang-faces-delta-beg
				    clang-faces-valid-pt))
	  (clang-faces-request-hilight))
      ;; Delta Start nil - mark this as the start of delta reg
      (setq clang-faces-delta-beg beg)))

  (when (eq clang-faces-current-change 'deletion)
    (message "Deletion!")

))

;; (defadvice ac-handle-post-command (around inhibit-mods () activate)
;;   ""
;;   (let ((inhibit-modification-hooks t))
;;     ad-do-it))
;; (ad-deactivate 'ac-handle-post-command)

;; (defadvice ac-update-greedy (around inhibit-mods (&optional force) activate)
;;   ""
;;   (let ((inhibit-modification-hooks t))
;;     ad-do-it))
;; (ad-deactivate 'ac-update-greedy)

(defun clang-faces-mode-default-hook ()
  ;; (if clang-faces-reparse-timer
  ;;     (cancel-timer clang-faces-reparse-timer))
  ;; (if clang-faces-fontify-timer
  ;;     (cancel-timer clang-faces-fontify-timer))

  (setq clang-faces-delta-beg nil)
  (setq clang-faces-delta-end nil)
  (setq clang-faces-valid-pt nil)
  (setq clang-faces-hilight-request-queue nil)

  (setq clang-faces-point-delta 0)
  (setq clang-faces-last-point 0)
  (setq clang-faces-fontify-region-queue nil)
  (setq clang-faces-status 'idle)
  (setq clang-faces-fontification-data-incoming nil)
  (setq clang-faces-fontification-data nil))

(defun clang-faces-mode-disable ()
  (clang-faces-kill-process)
  ;; (setq font-lock-fontify-buffer-function
  ;; 	(function font-lock-default-fontify-buffer))
  ;; (setq font-lock-fontify-region-function
  ;; 	(function font-lock-default-fontify-region))
  (setq clang-faces-mode nil)
  
  ;;(remove-hook 'post-command-hook 'clang-faces-post-command t)
  (remove-hook 'before-change-functions 'clang-faces-before-change)
  (remove-hook 'after-change-functions 'clang-faces-after-change)

  ;; (font-lock-mode -1)
  ;; (setq font-lock-defaults nil)
  ;; (font-lock-mode 1)
  (message "Clang Faces Disabled"))

(defun clang-faces-mode-enable ()
  (clang-faces-relaunch-process)

  (font-lock-mode -1)
  ;; (setq font-lock-defaults
  ;; 	`(("auto")
  ;; 	  nil
  ;; 	  nil
  ;; 	  ,c-identifier-syntax-modifications
  ;; 	  c-beginning-of-syntax
  ;; 	  (font-lock-fontify-region-function . clang-faces-fontify-region)
  ;; 	  (font-lock-fontify-buffer-function . clang-faces-fontify-buffer)))

  ;; (setq font-lock-fontify-buffer-function
  ;; 	(function clang-faces-fontify-buffer))
  ;; (setq font-lock-fontify-region-function
  ;; 	(function clang-faces-fontify-region))

  (font-lock-mode 1)
  ;; (setq clang-faces-fontify-timer
  ;; 	(run-at-time 5 5 (function clang-faces-fontify-buffer)
  ;; 		     (current-buffer)))
  ;; (setq clang-faces-reparse-timer
  ;; 	(run-with-idle-timer 5 2 (function 
  ;; 				  clang-faces-process-reparse-request)
  ;; 			     (current-buffer)))

  ;;(jit-lock-register (function clang-faces-fontify-region))
  (setq clang-faces-mode t)
  ;;(add-hook 'post-command-hook 'clang-faces-post-command t t)
  (add-hook 'before-change-functions 'clang-faces-before-change nil t)
  (add-hook 'after-change-functions 'clang-faces-after-change nil t)
  (setq clang-faces-delta-beg (point-min))
  (setq clang-faces-delta-end (point-max))
  (setq clang-faces-valid-pt (point-max))
  (clang-faces-request-hilight))

(defvar clang-faces-mode nil)

(defun clang-faces-mode (&optional arg)
  (interactive)
  ;(font-lock-mode -1)
;  (setq clang-faces-mode arg)

  (clang-faces-mode-default-hook)
  
  (cond ((or (and (null clang-faces-mode)
		  (null arg)
		  (called-interactively-p 'any))
	     (not (called-interactively-p 'any))
	     arg)
	 (clang-faces-mode-enable))
	(t
	 (clang-faces-mode-disable))))

(provide 'clang-faces)

;;; clang-faces.el ends here
