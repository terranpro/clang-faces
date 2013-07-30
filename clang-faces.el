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
  "~/code/emacs-clang-syntaxhighlighting/build/emacs-clang-syntaxhl")

(defvar clang-faces-process nil
  "Process object to the clang-faces executable")

(defvar clang-faces-fontification-data nil)
(defvar clang-faces-fontification-data-incoming nil)

(defvar clang-faces-status 'idle
  "Marking whether busy or idle.")

(defvar clang-faces-reparse-timer nil)

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

(defvar clang-faces-type-face-alist nil)
(setq clang-faces-type-face-alist
      '(("Punctuation" . default)
	("Identifier" . clang-faces-type-face)
	("Variable" . clang-faces-variable-name-face)
	("Literal" . clang-faces-literal-face)
	("Comment" . clang-faces-comment-face)
	("Keyword" . clang-faces-keyword-face)
	("Function" . clang-faces-function-name-face)))

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
    (put-text-property start end 'face type-face buf)
    (put-text-property start end 'fontified t buf)
    ))

(defvar clang-faces-fontify-region-queue nil)

(defun clang-faces-fontify-region-worker (start end buf)
  (message (format "Fontifying in %s" buf))
  (with-silent-modifications
    (mapcar
     #'(lambda (e)
	 ;(message (format "Element: %d %d" (car e) (cadr e)))
	 (clang-faces-color-region buf
				   (caddr e)
				   (car e)
				   (cadr e)))
     (loop for elem in clang-faces-parsed-data
	   for s = (car elem)
	   if (and (>= s start) (<= s end))
	   collect elem))))

(defun clang-faces-fontify-queue ()
  (unwind-protect
      (mapcar #'(lambda (e) 
		  (clang-faces-fontify-region-worker (car e) (cadr e)
						     (caddr e)))
	      clang-faces-fontify-region-queue)
    (setq clang-faces-fontify-region-queue nil)))

(defun clang-faces-fontify-region (&optional start end verbose)
  (message (format "Fontify Region! %d %d" start end))

  (setq clang-faces-fontify-region-queue 
	(append 
	 clang-faces-fontify-region-queue
	 (list (list start end (current-buffer))))))

(defun clang-faces-fontify-buffer (&optional buf)
  (interactive)
  (clang-faces-fontify-region-worker (point-min) (point-max) 
				     (or buf (current-buffer))))

(defvar clang-faces-parsed-data nil)

(defun clang-faces-parse-incoming-data ()
  (with-temp-buffer
    (insert clang-faces-fontification-data)
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
      (setq clang-faces-parsed-data pdata))))

(defun clang-faces-process-reparse-request (&optional buf)
  (interactive)
  (clang-faces-fontify-queue)
  (message "Reparse request!")
  (with-current-buffer (or buf (current-buffer))
   (when (and clang-faces-process
	      (eq clang-faces-status 'idle))
     (setq clang-faces-status 'busy)
     ;; (process-send-string clang-faces-process 
     ;; 			 (concat "\n"
     ;; 				 clang-faces-output-finished-marker))

     (process-send-string clang-faces-process "\nREPARSE\n")
     (process-send-string clang-faces-process
			  (concat
			   (buffer-substring-no-properties (point-min)
							   (point-max))
			   "\n"
			   clang-faces-output-finished-marker
			   "\n")))))

(defmacro safe1- (n)
  `(if ,n (1- ,n) nil))

(defun clang-faces-process-filter (process output)
  ;;(message "Inside Process Filter!")
  (let ((off (string-match 
	      (rx bol (eval clang-faces-output-finished-marker))
	      output)))
    (setq clang-faces-fontification-data-incoming
	  (concat clang-faces-fontification-data-incoming
		  (substring output 0 (safe1- off))))

    (if off (progn
	      ;(message "End of Output Detected!")
	      (setq clang-faces-status 'idle
		    clang-faces-fontification-data 
		    clang-faces-fontification-data-incoming
		    clang-faces-fontification-data-incoming nil)
	      (clang-faces-parse-incoming-data)
	      (clang-faces-fontify-queue)))
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
	 (outbuf "clang-faces")
	 (cmd (concat clang-faces-client-exec 
		      " "
		      file)))
    (setq clang-faces-process
	  (start-process "clang-faces" outbuf clang-faces-client-exec
			 file))
    (set-process-filter clang-faces-process 
			(function clang-faces-process-filter))
    (message "Clang Faces Process Launched!")))

(defun clang-faces-mode-default-hook ()
  (if clang-faces-reparse-timer
      (cancel-timer clang-faces-reparse-timer))
  (setq clang-faces-fontify-region-queue nil)
  (setq clang-faces-status 'idle)
  (setq clang-faces-fontification-data-incoming nil)
  (setq clang-faces-fontification-data nil))

(defun clang-faces-mode-disable ()
  (clang-faces-kill-process)
  (setq font-lock-fontify-buffer-function
	(function font-lock-default-fontify-buffer))
  (setq font-lock-fontify-region-function
	(function font-lock-default-unfontify-region))
  (setq clang-faces-mode nil))

(defun clang-faces-mode-enable ()
  (clang-faces-relaunch-process)

  (setq clang-faces-reparse-timer
	(run-at-time 5 5 (function clang-faces-process-reparse-request)
		     (current-buffer)))

  ;;(jit-lock-register (function clang-faces-fontify-region))
  (setq font-lock-fontify-buffer-function
	(function clang-faces-fontify-region))
  (setq font-lock-fontify-region-function
	(function clang-faces-fontify-region))
  (setq clang-faces-mode t))

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

(setq clang-faces-temp clang-faces-fontification-data
      )

(let ((out))
  t)
;;; clang-faces.el ends here
