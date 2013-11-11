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
;; `clang-faces-mode' after setting `clang-faces-exec' 
;;
;; Enjoy!
;;
;;; TODO: 
;; Buggy =(
;;
;;; Code:

(defvar clang-faces-exec 
  "~/code/clang-faces/build/clang-faces")

(defvar-local clang-faces-process nil
  "Process object to the clang-faces executable")

(defvar-local clang-faces-parsed-data nil)

(defvar-local clang-faces-current-buffer nil)

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

(defvar-local clang-faces-dirty-id 0)
(defvar-local clang-faces-dirty-current 0)

(defconst clang-faces-output-finished-marker "!!!!$$$$!!!!")
(defconst clang-faces-output-finished-regexp 
  (concat "^" (regexp-quote clang-faces-output-finished-marker)))

;(defface )
(defface clang-faces-keyword-face
  '((t :inherit font-lock-keyword-face))
  ""
  :group 'clang-faces)

(defface clang-faces-literal-face
  '((t :inherit font-lock-string-face))
  ""
  :group 'clang-faces)

(defface clang-faces-type-face
  '((t :inherit font-lock-type-face))
  ""
  :group 'clang-faces)

(defface clang-faces-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  ""
  :group 'clang-faces)

(defface clang-faces-comment-face
  '((t :inherit font-lock-comment-face))
  ""
  :group 'clang-faces)

(defface clang-faces-function-name-face
  '((t :inherit font-lock-function-name-face))
  ""
  :group 'clang-faces)
(set-face-attribute 'clang-faces-function-name-face nil 
		    :inherit font-lock-function-name-face)

(defface clang-faces-namespace-face
  '((t :inherit warning))
  ""
  :group 'clang-faces)
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
    (remove-text-properties start (1+ end) '(dirty) buf)
;    (put-text-property start end 'fontified nil buf)
    ))

(defun clang-faces-fontify-region-worker (start end buf &optional hardstoppt)
  ;(message (format "Fontifying in %s" buf))
  (with-silent-modifications
    (mapcar
     #'(lambda (e)
	 ;(message (format "Element: %d %d %s" (car e) (cadr e) (caddr e)))
	 (clang-faces-color-region buf
				   (caddr e)
				   (car e)
				   (cadr e)))
     (loop for elem in clang-faces-parsed-data
	   for s = (car elem)
	   for e = (cadr elem)
	   with max = (or hardstoppt (1+ (buffer-size buf)))
	   with win-end = (window-end (get-buffer-window buf))
	   if (and (>= s start) (<= s end) (<= s max) (<= e max)
		   (get-text-property s 'dirty buf) 
		   (get-text-property e 'dirty buf))
	   ;;if (and (>= s start) (<= s win-end))
	   collect elem))))

(defun clang-faces-fontify-buffer (&optional buf)
  (interactive)
  (clang-faces-fontify-region-worker (point-min) (point-max) 
				     (or buf (current-buffer))))


(defun clang-faces-next-dirty-region ()
  (save-excursion
    (let ((beg (next-single-property-change (point) 'dirty))
	  (end (next-single-property-change (point) 'dirty))
	  this-id this-dirty-pt res)
      (while (and beg end (not res))
	(setq this-id (get-text-property beg 'dirty))
	(setq this-dirty-pt (get-text-property beg 'dirtypt))
	(setq end (next-single-property-change beg 'dirty))
	(if this-id 
	    (setq res (list this-id this-dirty-pt beg (or end (point-max))))
	  (setq beg end)))
      res)))

(defun clang-faces-reset-dirty-id-after (pos oldid newid)
  (save-excursion
    (let (reg) 
      (while (setq reg (clang-faces-next-dirty-region))
	(let ((thisid (car reg))
	      (beg (cadr reg))
	      (end (caddr reg)))
	  (when (= oldid thisid)
	    (set-text-properties beg end (list 'dirty newid)))
	  (goto-char end))))))

;(defun clang-faces-)
(defun clang-faces-update-table-entry (tbl dirtypt parsed-data)
  (let ((res nil))
    (loop for entry in parsed-data
	 for beg = (car entry)
	 for end = (cadr entry)
	 for this-res = (aget clang-faces-type-face-alist (caddr entry))
	 ;do (message (format "%s Comparing %d > %d >= %d" this-res end dirtypt beg))
	 if (and (>= dirtypt beg) (< dirtypt end))
	 do (setq res this-res)
	 until res)
    
    (when res
      (puthash dirtypt res tbl))))

;; ;; Test clang-faces-update-table-entry
;; (let ((parsed-data (list (list 1 10 "Punctuation")
;; 			 (list 10 20 "Function")
;; 			 (list 20 30 "Namespace")
;; 			 (list 30 40 "Keyword")))
;;       (tbl (make-hash-table))
;;       (key 39))
;;   (puthash key "Punctuation" tbl)
;;   (clang-faces-update-table-entry tbl key parsed-data)
;;   (maphash #'(lambda (key val) (message (format "%d => %s" key val))) tbl)
;;   ;(gethash key tbl)
;;   )

;; ;; General testing of maphash for sortedness (result: not sorted)
;; (let ((tbl (make-hash-table)))
;;   (loop for x from 15 downto 1
;; 	do
;; 	(puthash x "Puncutation" tbl))

;;   (maphash #'(lambda (key val) (message (format "%d => %d" key val))) tbl))


;;(aget clang-faces-type-face-alist "Punctuation")

(defun clang-faces-update-table (tbl)
  (maphash #'(lambda (key val)
	       (clang-faces-update-table-entry tbl key clang-faces-parsed-data))))

(defun clang-faces-update-dirty-entries-for-id (dirty-id dirty-pt-table)
  (save-excursion
    (goto-char (point-min))
    (let (dreg this-did this-beg this-end)
     (while (setq dreg (clang-faces-next-dirty-region))
       (setq this-did (car dreg))
       (setq this-dpt (cadr dreg))
       (setq this-beg (caddr dreg))
       (setq this-end (cadddr dreg))

       (while (< this-beg this-end)
	 (goto-char this-beg)
	 (when (= dirty-id this-did)
	   (set-text-properties this-beg (1+ this-beg) (list 'font-lock-face (gethash this-dpt tbl))))
	 (setq this-beg (1+ this-beg)))
;; TODO: finish this amazing shit~! and make test harness!
)))
  )

(defun clang-faces-on-hilight-returns (proc)
  "To be called by the process filter when it has collected all
process output and parsed the incoming data.  Pops the first
element off the hilight queue and attempts to fontify the delta
region."
  (let ((buf (with-current-buffer (process-buffer proc)
	       clang-faces-current-buffer))
	;; Experimental: without this, we might make font-lock overwork?
	(inhibit-point-motion-hooks t)
	elt beg end dirty-id dirty-pt-table)
    (with-current-buffer buf
      (setq elt (pop clang-faces-hilight-request-queue))
      (message (format "dirty-id = %d dirty-current = %d"
		       (car elt) clang-faces-dirty-current))
      (when (and elt
		 (setq dirty-id (car elt))
		 (setq dirty-pt-table (cadr elt))
		 ;(= clang-faces-dirty-current dirty-id)
		 )
	(message (format "Highlighting for dirty id: %d" dirty-id))
	(clang-faces-update-table dirty-pt-table)
	(clang-faces-update-dirty-entries-for-id dirty-id dirty-pt-table)

	;; ;; TODO: should we change this?!
	;; (save-excursion
	;;   (clang-faces-fontify-region-worker (point-min) (point-max) buf)
	;;   ;(font-lock-fontify-region beg end)
	;;   )

))))

(defun clang-faces-parse-incoming-data (proc)
  (setq 
   clang-faces-parsed-data
   (with-current-buffer (process-buffer proc)
     (goto-char (point-min))
     (let ((regex clang-faces-output-regex)
	   pdata)
       (while (search-forward-regexp regex (point-max) t)
	 
	 
	 (setq pdata
	       ;; (append
	       ;; 	pdata
	       ;; 	(list
	       ;; 	 (list
	       ;; 	  (1+ (string-to-number (match-string 1)))
	       ;; 	  (1+ (string-to-number (match-string 2)))
	       ;; 	  (match-string 3))))

	       ;; ;; I think this is more efficient+faster with what
	       ;; ;; little I could ascertain from `profile-report'.
	       (nconc pdata
	       	      (list
	       	       (list
	       		(1+ (string-to-number (match-string 1)))
	       		(1+ (string-to-number (match-string 2)))
	       		(match-string 3))))
))
       (erase-buffer)
       pdata))))

(defun clang-faces-proc-send-reparse (proc bufstr)
  (process-send-string proc "\nREPARSE\n")
  (process-send-string 
   proc
   (concat bufstr "\n" clang-faces-output-finished-marker "\n")))

(defun clang-faces-request-hilight-worker (proc)
  (let ((srcbuf (with-current-buffer (process-buffer proc)
		  clang-faces-current-buffer)))
    (with-current-buffer srcbuf
      (when (and (eq clang-faces-status 'idle)
		 clang-faces-hilight-request-queue)
					;(message "Reparse request!")
	(setq clang-faces-status 'busy)
	;;(setq clang-faces-last-tick (buffer-modified-tick))
	(clang-faces-proc-send-reparse 
	 proc (buffer-substring-no-properties (point-min) 
					      (point-max)))))))

;; (defmacro safe1- (n)
;;   `(if ,n (1- ,n) nil))

(defun clang-faces-process-filter (process output)
					;(message "Inside Process Filter!")
  (with-current-buffer (process-buffer process)
    (let ((srcbuf clang-faces-current-buffer)
	  (off (progn
		(goto-char (point-max))
		(insert output)
		(goto-char (point-min))
		(search-forward-regexp
		 clang-faces-output-finished-regexp (point-max) t))))

     (if off (with-current-buffer srcbuf
	       (setq clang-faces-status 'idle)
	       (clang-faces-parse-incoming-data process)
	      
	       (clang-faces-on-hilight-returns process)

	       (if clang-faces-hilight-request-queue
		   (progn
		     (clang-faces-request-hilight-worker process))
		 (save-excursion
		   (message 
		    (format "Current buffer: %s\nFormatting buffer: %s"  
				    
				    (or (current-buffer) "") 
				    (or srcbuf "")))
		   (font-lock-fontify-region (point-min)
					     (point-min))))))
     t)))

(defun clang-faces-get-process-parent-buffer (proc)
  (when proc
    (process-buffer proc)))

(defun clang-faces-kill-process ()
  (when clang-faces-process
    (message "Sending kill-process to clang-faces")
    (ignore-errors (kill-buffer (process-buffer clang-faces-process))
		   (kill-process clang-faces-process)
		   (setq clang-faces-process nil))))

(defun clang-faces-relaunch-process ()
  (interactive)
  (clang-faces-kill-process)
  (clang-faces-launch-process))

(defun clang-faces-launch-process ()
  (interactive)
  (let* ((buf (current-buffer))
	 (file (or (buffer-file-name)
		   (concat default-directory (buffer-name buf))))
	 (outbuf (generate-new-buffer-name "clang-faces"))
	 (cmdargs (append ac-clang-cflags
			  (list file))))
    (message (format "Launching clang-faces with args: %s" 
		     (mapconcat 'identity cmdargs " ")))
    (setq clang-faces-process
	  (apply 'start-process
		 "clang-faces"
		 outbuf clang-faces-exec
		 cmdargs))
    (set-process-filter clang-faces-process 
			(function clang-faces-process-filter))
    (set-process-query-on-exit-flag clang-faces-process nil)
    (with-current-buffer outbuf
      (setq clang-faces-current-buffer buf))
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

(defvar-local clang-faces-dirty-pt-table (make-hash-table))

(defun clang-faces-request-hilight ()
  (let* (;(entry (list beg end))
	 (entry (list clang-faces-dirty-id clang-faces-dirty-pt-table))
	 (proc clang-faces-process))
    (setq clang-faces-hilight-request-queue
	  (append clang-faces-hilight-request-queue
		  (list entry)))
    (message (format "Requesting Hilight in %s w/id %d !"
		     (current-buffer) clang-faces-dirty-id))
    ;; reset hash table
    (setq clang-faces-dirty-pt-table (make-hash-table))

    (clang-faces-request-hilight-worker proc)))

(defun clang-faces-idle-request-hilight (buffer)
  (with-current-buffer buffer
    (let ((tick (buffer-chars-modified-tick)))
      (unless (eq tick clang-faces-last-tick)
	(setq clang-faces-last-tick tick)
	(clang-faces-request-hilight)))))

(defun clang-faces-force-request-hilight ()
  (interactive)
  (let ((clang-faces-delta-beg (point-min))
	(clang-faces-delta-end (point-max)))
    (clang-faces-request-hilight)))

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
  "Return TRUE if the character ELT is present in the string STR."
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
  "Return TRUE if the string ELTS contains any character in the string STR."
  `(let ((er nil)
	 (en 0)
	 (elen (length ,elts)))
     (while (and (null er)
		 (< en elen))
       (if (mems (aref ,elts en) ,str)
	   (setq er t)
	 (setq en (1+ en))))
     er))

(defun clang-faces-reset-dirty-id ()
  (setq clang-faces-dirty-id (1+ clang-faces-dirty-id)))

(defun clang-faces-after-change (beg end oldlen)
  (ignore-errors
    (when (eq clang-faces-current-change 'insertion)
      (setq clang-faces-dirty-current clang-faces-dirty-id)
      (message (format
		"Point %d Beg: %d End: %d Inserted text: %s" 
		(point) beg end (buffer-substring-no-properties beg end)))

      (set-text-properties beg end (list 'dirty clang-faces-dirty-id 'dirty-pt beg))
      (puthash beg 'default clang-faces-dirty-pt-table)

      ;; Delta Start Region already exists... check for stop char
      (when (and (memls "(){};<>:"
			(buffer-substring-no-properties beg end)))
	;; Stop character found, mark end of delta region
	(clang-faces-request-hilight)
	(clang-faces-reset-dirty-id))
      ;; Delta Start nil - mark this as the start of delta reg
      ))

  (when (eq clang-faces-current-change 'deletion)
    (message "Deletion!")
;    (setq clang-faces-dirty-current clang-faces-dirty-id)
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

(defun clang-faces-kill-buffer ()
  (message "Killing Clang-Faces...")
  (let ((buf (current-buffer)))
    (message "Current buffer: %s" buf)
    (clang-faces-mode-disable)))

(defun clang-faces-mode-default-hook ()
  ;; (if clang-faces-reparse-timer
  ;;     (cancel-timer clang-faces-reparse-timer))
  
  (set-text-properties (point-min) (point-max) (list 'dirty
						     clang-faces-dirty-id))
  (setq clang-faces-delta-beg nil)
  (setq clang-faces-delta-end nil)
  (setq clang-faces-valid-pt nil)
  (setq clang-faces-hilight-request-queue nil)

  (setq clang-faces-point-delta 0)
  (setq clang-faces-last-point 0)
  (setq clang-faces-fontify-region-queue nil)
  (setq clang-faces-status 'idle))

(defun clang-faces-mode-disable ()
  ;; (setq font-lock-fontify-buffer-function
  ;; 	(function font-lock-default-fontify-buffer))
  ;; (setq font-lock-fontify-region-function
  ;; 	(function font-lock-default-fontify-region))
  (setq clang-faces-mode nil)
  (when clang-faces-fontify-timer
    (message "Cancelling Clang Faces Fontify Timer")
    (cancel-timer clang-faces-fontify-timer))
  
  ;;(remove-hook 'post-command-hook 'clang-faces-post-command t)
  (remove-hook 'before-change-functions 'clang-faces-before-change)
  (remove-hook 'after-change-functions 'clang-faces-after-change)
  (clang-faces-kill-process)
  ;; (font-lock-mode -1)
  ;; (setq font-lock-defaults nil)
  ;; (font-lock-mode 1)
  (message "Clang Faces Disabled"))

(defun clang-faces-mode-enable ()
  (message "Enabling Clang Faces...")
  (clang-faces-relaunch-process)

  ;;(font-lock-mode -1)
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

  ;;(font-lock-mode 1)
  (setq clang-faces-current-buffer (current-buffer))
  (and (not clang-faces-fontify-timer)

       ;; (setq clang-faces-fontify-timer
       ;; 	     ;; (run-with-idle-timer 5 5 (function clang-faces-fontify-buffer)
       ;; 	     ;; 		     (current-buffer))
       ;; 	     (run-with-idle-timer 5 5
       ;; 				  (function clang-faces-idle-request-hilight)
       ;; 				  clang-faces-current-buffer))

       ;; (message "Refontify Timer Started")
)
  ;; (setq clang-faces-reparse-timer
  ;; 	(run-with-idle-timer 5 2 (function 
  ;; 				  clang-faces-process-reparse-request)
  ;; 			     (current-buffer)))

  ;;(jit-lock-register (function clang-faces-fontify-region))
  (setq clang-faces-mode t)
  ;;(add-hook 'post-command-hook 'clang-faces-post-command t t)
  (add-hook 'before-change-functions 'clang-faces-before-change nil t)
  (add-hook 'after-change-functions 'clang-faces-after-change nil t)
  (add-hook 'kill-buffer-hook 'clang-faces-kill-buffer nil t)
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
