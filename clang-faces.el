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

(require 'cl)

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
    (remove-text-properties start (1+ end) (list 'dirty 'ignore) buf)
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


(defun clang-faces-this-dirty-region ()
  (save-excursion
    (let* ((beg (point))
	   (end (next-single-property-change (point) 'dirty))
	   (this-id (get-text-property beg 'dirty)))
      (cond 
       ((and this-id (null end))
	(setq end (point-max))
	(setq res (list this-id beg end)))

       ((null this-id)
	(setq res nil))

       (t
	(setq res (list this-id beg (or end (point-max))))))
      res)))

(defun clang-faces-next-dirty-region (&optional dirty-id)
  (save-excursion
    (let ((beg (next-single-property-change (point) 'dirty))
	  (end (next-single-property-change (point) 'dirty))
	  this-id res)
      (while (and beg end (not res))
	(setq this-id (get-text-property beg 'dirty))
	(setq end (next-single-property-change beg 'dirty))
	(if this-id 
	    (setq res (list this-id beg (or end (point-max))))
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
	    (put-text-property beg end 'dirty newid))
	  (goto-char end))))))

(defun clang-faces-get-face-by-point (pt parsed-data)
  (let ((res nil))
    (loop for entry in parsed-data
	  for beg = (car entry)
	  for end = (cadr entry)
	  for this-res = (aget clang-faces-type-face-alist (caddr entry))
	  ;; do (message (format "%s Comparing %d > %d >= %d" this-res end pt beg))
	  if (and (>= pt beg) (< pt end))
	  do (setq res this-res)
	  until res)
    
    res))

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


(defun clang-faces-update-table (tbl parsed-data)
  (maphash #'(lambda (key val)
	       (clang-faces-update-table-entry tbl key parsed-data))
	   tbl)
  (maphash #'(lambda (key val)
	       (message (format "%d => %s" key val)))
	   tbl))

;; (puthash 692 'default clang-faces-dirty-pt-table)
;; (clang-faces-update-table 
;;  clang-faces-dirty-pt-table
;; '((1 2 "Punctuation")
;;  (2 9 "Identifier")
;;  (10 11 "Punctuation")
;;  (11 19 "Identifier")
;;  (19 20 "Punctuation")
;;  (22 23 "Punctuation")
;;  (23 30 "Identifier")
;;  (31 32 "Punctuation")
;;  (32 36 "Identifier")
;;  (36 37 "Punctuation")
;;  (37 38 "Identifier")
;;  (38 39 "Punctuation")
;;  (41 49 "Identifier")
;;  (50 58 "Function")
;;  (58 59 "Punctuation")
;;  (60 68 "Identifier")
;;  (69 78 "Variable")
;;  (79 80 "Punctuation")
;;  (81 82 "Punctuation")
;;  (85 94 "Identifier")
;;  (95 96 "Punctuation")
;;  (96 100 "Variable")
;;  (101 102 "Punctuation")
;;  (103 104 "Punctuation")
;;  (104 113 "Identifier")
;;  (114 115 "Punctuation")
;;  (115 116 "Punctuation")
;;  (116 125 "Variable")
;;  (125 126 "Punctuation")
;;  (129 136 "Function")
;;  (136 137 "Punctuation")
;;  (138 152 "Literal")
;;  (153 154 "Punctuation")
;;  (154 155 "Punctuation")
;;  (159 175 "Function")
;;  (175 176 "Punctuation")
;;  (177 181 "Variable")
;;  (182 183 "Punctuation")
;;  (183 184 "Punctuation")
;;  (188 194 "Keyword")
;;  (195 200 "Identifier")
;;  (200 201 "Punctuation")
;;  (202 203 "Punctuation")
;;  (205 209 "Identifier")
;;  (210 226 "Function")
;;  (226 227 "Punctuation")
;;  (228 240 "Identifier")
;;  (241 242 "Punctuation")
;;  (242 243 "Variable")
;;  (243 244 "Punctuation")
;;  (245 254 "Identifier")
;;  (255 256 "Punctuation")
;;  (256 257 "Variable")
;;  (258 259 "Punctuation")
;;  (260 261 "Punctuation")
;;  (264 271 "Identifier")
;;  (272 273 "Punctuation")
;;  (273 274 "Variable")
;;  (275 276 "Punctuation")
;;  (277 297 "Function")
;;  (297 298 "Punctuation")
;;  (299 302 "Literal")
;;  (303 304 "Punctuation")
;;  (304 305 "Punctuation")
;;  (308 329 "Function")
;;  (329 330 "Punctuation")
;;  (331 332 "Variable")
;;  (332 333 "Punctuation")
;;  (334 342 "Function")
;;  (342 343 "Punctuation")
;;  (344 345 "Punctuation")
;;  (345 353 "Identifier")
;;  (353 354 "Punctuation")
;;  (354 355 "Variable")
;;  (355 356 "Punctuation")
;;  (357 361 "Function")
;;  (362 363 "Punctuation")
;;  (363 364 "Punctuation")
;;  (367 382 "Function")
;;  (382 383 "Punctuation")
;;  (384 385 "Variable")
;;  (385 386 "Punctuation")
;;  (387 388 "Variable")
;;  (389 390 "Punctuation")
;;  (390 391 "Punctuation")
;;  (394 408 "Function")
;;  (408 409 "Punctuation")
;;  (410 411 "Variable")
;;  (412 413 "Punctuation")
;;  (413 414 "Punctuation")
;;  (415 416 "Punctuation")
;;  (418 421 "Identifier")
;;  (422 426 "Function")
;;  (426 427 "Punctuation")
;;  (427 430 "Identifier")
;;  (431 435 "Variable")
;;  (435 436 "Punctuation")
;;  (437 441 "Identifier")
;;  (442 443 "Punctuation")
;;  (443 447 "Variable")
;;  (447 448 "Punctuation")
;;  (448 449 "Punctuation")
;;  (449 450 "Punctuation")
;;  (451 452 "Punctuation")
;;  (455 467 "Identifier")
;;  (468 469 "Punctuation")
;;  (469 476 "Variable")
;;  (477 478 "Punctuation")
;;  (479 497 "Function")
;;  (497 498 "Punctuation")
;;  (498 499 "Punctuation")
;;  (499 500 "Punctuation")
;;  (503 512 "Identifier")
;;  (513 514 "Punctuation")
;;  (514 518 "Variable")
;;  (519 520 "Punctuation")
;;  (521 536 "Function")
;;  (536 537 "Punctuation")
;;  (538 545 "Variable")
;;  (545 546 "Punctuation")
;;  (547 552 "Function")
;;  (553 554 "Punctuation")
;;  (554 555 "Punctuation")
;;  (559 575 "Function")
;;  (575 576 "Punctuation")
;;  (577 584 "Variable")
;;  (584 585 "Punctuation")
;;  (586 590 "Variable")
;;  (591 592 "Punctuation")
;;  (592 593 "Punctuation")
;;  (597 612 "Function")
;;  (612 613 "Punctuation")
;;  (614 618 "Variable")
;;  (619 620 "Punctuation")
;;  (620 621 "Punctuation")
;;  (624 641 "Function")
;;  (641 642 "Punctuation")
;;  (643 647 "Variable")
;;  (648 649 "Punctuation")
;;  (649 650 "Punctuation")
;;  (653 668 "Function")
;;  (668 669 "Punctuation")
;;  (670 674 "Variable")
;;  (675 676 "Punctuation")
;;  (676 677 "Punctuation")
;;  (681 696 "Identifier")
;;  (696 697 "Punctuation")
;;  (700 704 "Identifier")
;;  (705 706 "Punctuation")
;;  (707 722 "Identifier")
;;  (722 723 "Punctuation")
;;  (724 731 "Identifier")
;;  (731 732 "Punctuation")
;;  (733 738 "Identifier")
;;  (739 740 "Punctuation")
;;  (740 741 "Punctuation")
;;  (745 761 "Function")
;;  (761 762 "Punctuation")
;;  (763 770 "Variable")
;;  (770 771 "Punctuation")
;;  (772 776 "Variable")
;;  (777 778 "Punctuation")
;;  (778 779 "Punctuation")
;;  (782 797 "Function")
;;  (797 798 "Punctuation")
;;  (799 803 "Variable")
;;  (804 805 "Punctuation")
;;  (805 806 "Punctuation")
;;  (809 826 "Function")
;;  (826 827 "Punctuation")
;;  (828 832 "Variable")
;;  (833 834 "Punctuation")
;;  (834 835 "Punctuation")
;;  (839 859 "Function")
;;  (859 860 "Punctuation")
;;  (861 868 "Variable")
;;  (869 870 "Punctuation")
;;  (870 871 "Punctuation")
;;  (875 881 "Keyword")
;;  (882 883 "Literal")
;;  (883 884 "Punctuation")
;;  (885 886 "Punctuation")
;;  (888 907 "Comment")
;;  (908 1042 "Comment")
;;  (1043 1129 "Comment")
;;  (1130 1137 "Comment")))

(defun clang-faces-update-dirty-entries-for-id (dirty-id dirty-pt-table)
  (message (format "dirty id: %d" dirty-id))
  (save-excursion
    (goto-char (point-min))
    (let (dreg this-did this-beg this-end cur-pt)
      (message (format "dirty table: %s" dirty-pt-table))
      (message (format "parsed data: %s" clang-faces-parsed-data))

      (while (setq dreg (or (clang-faces-next-dirty-region)
			    (clang-faces-this-dirty-region)))
	(setq this-did (car dreg))
	(setq this-beg (cadr dreg))
	(setq this-end (caddr dreg))
	(setq cur-pt this-beg)

	(while (< cur-pt this-end)
	  (goto-char cur-pt)
	  ;; TODO: loop optimization, pull when out in front of 2nd while
	  (message (format "cur-pt: %d this-end %d" cur-pt this-end))
	  (message (format "Comparing did %d this did %d" dirty-id this-did))
	  
	  (when (= dirty-id this-did)
	    (message (format "newface: %s" (or (gethash this-dpt dirty-pt-table)
					       (clang-faces-get-face-by-point this-dpt clang-faces-parsed-data))))
	    (let* ((this-dpt (or (get-text-property cur-pt 'dirtypt)
				 cur-pt))
		   (this-face (or (gethash this-dpt dirty-pt-table)
				  (clang-faces-get-face-by-point this-dpt clang-faces-parsed-data))))
	      (message (format "thisbeg %d this-dpt %d" cur-pt this-dpt))
	      (when (and (not (or (overlays-at cur-pt)))
			 this-face)
		(message (format "Adding face: %s" (gethash this-dpt dirty-pt-table)))

		(add-text-properties cur-pt
				     (1+ cur-pt)
				     (list 'font-lock-face 
					   this-face))
		(remove-text-properties cur-pt (1+ cur-pt)
					(list 'dirty 'ignore 
					      'dirty-pt 'ignore)))
	      ))
	  (setq cur-pt (1+ cur-pt)))

	;;(remove-text-properties this-beg this-end (list 'dirty 'ignore))
	(goto-char this-end)
	;; TODO: finish this amazing shit~! and make test harness!

	))))

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
      (cond 
       ((and elt
	     (setq dirty-id (car elt))
	     (setq dirty-pt-table (cadr elt))
	     ;; (= clang-faces-dirty-current dirty-id)
	     )

	(when (or (null dirty-pt-table) 
		  (eq (hash-table-count dirty-pt-table) 0))
	  (message (format "Byung Sin!"))
	  ;; (clang-faces-fontify-buffer)
	  (loop for pt from (point-min) to (1- (point-max))
		;; if (not (string=
		;; 	 (spaces-string 1) (buffer-substring pt (1+ pt))))
		do (puthash pt 'default dirty-pt-table)))
	(message (format "Highlighting for dirty id: %d" dirty-id))
	(clang-faces-update-table dirty-pt-table clang-faces-parsed-data)
	(clang-faces-update-dirty-entries-for-id dirty-id dirty-pt-table))

       (t
	(message "Houston, we have a byung sin."))

       

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
       (when (null pdata) (error "Donkey!"))
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
  (let* ((entry (list clang-faces-dirty-id clang-faces-dirty-pt-table))
	 (proc clang-faces-process))
    (setq clang-faces-hilight-request-queue
	  (append clang-faces-hilight-request-queue
		  (list entry)))
    (message (format "Requesting Hilight in %s w/id %d !"
		     (current-buffer) clang-faces-dirty-id))
    ;; reset hash table

    (clang-faces-reset-dirty-id)
    (clang-faces-reset-dirty-pt-table)

    (clang-faces-request-hilight-worker proc)))

(defun clang-faces-reset-dirty-pt-table ()
  (setq clang-faces-dirty-pt-table (make-hash-table)))

(defun clang-faces-request-hilight-region (beg end)
  ;; (set-text-properties beg end
  ;; 		       (list 'dirty
  ;; 			     clang-faces-dirty-id))
  ;; This shit is *very* expensive
  (loop for pt from beg to (1- end)
  	do (add-text-properties 
	    pt (1+ pt) 
	    (list 'dirty clang-faces-dirty-id 'dirty-pt pt)))

  (clang-faces-reset-dirty-pt-table)
  (clang-faces-request-hilight))

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

  (cond 
   ((or (null this-command))
    (setq clang-faces-current-change 'ignore))
   ((eq (- end beg) 0)
    (setq clang-faces-current-change 'insertion))
   (t
    (setq clang-faces-current-change 'deletion)))
  (message 
   (format "lastcmd: %s thiscmd: %s clang-current-change %s %d %d before: %s"
  	   last-command
  	   this-command
	   clang-faces-current-change
  	   beg end (buffer-substring-no-properties beg end)))

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

(defun clang-faces-debug-dirty-pt-table ()
  (interactive)
  (maphash #'(lambda (key val)
	       (message (format "%d => %s" key val)))
	   clang-faces-dirty-pt-table))

(defun clang-faces-after-change (beg end oldlen)
  (ignore-errors
    (when (eq clang-faces-current-change 'insertion)
      (let ((ins-txt (buffer-substring-no-properties beg end)))
	(setq clang-faces-dirty-current clang-faces-dirty-id)
	(message (format
		  "Point %d Beg: %d End: %d Inserted text: %s" 
		  (point) beg end ins-txt))

	(when (not (or (string= (spaces-string (- end beg)) ins-txt)
		       (string= "\n" ins-txt)))
	  (add-text-properties beg end 
			      (list 'dirty clang-faces-dirty-id 
				    'dirty-pt beg))
	  (puthash beg 'default clang-faces-dirty-pt-table))

	;; Delta Start Region already exists... check for stop char
	(when (and (memls "){}<>:" ins-txt))
	  ;; Stop character found, mark end of delta region
	  (clang-faces-request-hilight))

	(when (memls ";}" ins-txt)
	  (clang-faces-request-hilight-region (save-excursion
						(c-beginning-of-defun)
						(point))
					      end)))
      

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
  
  (setq clang-faces-delta-beg nil)
  (setq clang-faces-delta-end nil)
  (setq clang-faces-valid-pt nil)
  (setq clang-faces-hilight-request-queue nil)

  (setq clang-faces-dirty-id 0)
  (setq clang-faces-dirty-pt-table (makehash))
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

  (clang-faces-request-hilight-region (point-min) (point-max)))

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
