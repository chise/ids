;;; ids-dump.el --- Dump utility of IDS-* files

;; Copyright (C) 2002 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <tomo@kanji.zinbun.kyoto-u.ac.jp>
;; Keywords: IDS, IDC, Ideographs, UCS, Unicode

;; This file is a part of IDS.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'ids)

(defvar ids-dump-file-specs-alist
  '((ucs-basic "IDS-UCS-Basic.txt"
	       ids-dump-insert-ccs-ranges
	       ucs "U+%04X\t%c\t%s\n"
	       (#x4E00 . #x9FA5))
    (ucs-ext-a "IDS-UCS-Ext-A.txt"
	       ids-dump-insert-ccs-ranges
	       ucs "U+%04X\t%c\t%s\n"
	       (#x3400 . #x4DB5) #xFA1F #xFA23)
    (ucs-compat "IDS-UCS-Compat.txt"
		ids-dump-insert-ccs-ranges
		ucs "U+%04X\t%c\t%s\n"
		(#xF900 . #xFA1E) (#xFA20 . #xFA22) (#xFA24 . #xFA2D))
    (ucs-ext-b-1 "IDS-UCS-Ext-B-1.txt"
		 ids-dump-insert-ccs-ranges
		 ucs "U-%08X\t%c\t%s\n"
		 (#x20000 . #x21FFF))
    (ucs-ext-b-2 "IDS-UCS-Ext-B-2.txt"
		 ids-dump-insert-ccs-ranges
		 ucs "U-%08X\t%c\t%s\n"
		 (#x22000 . #x23FFF))
    (ucs-ext-b-3 "IDS-UCS-Ext-B-3.txt"
		 ids-dump-insert-ccs-ranges
		 ucs "U-%08X\t%c\t%s\n"
		 (#x24000 . #x25FFF))
    (ucs-ext-b-4 "IDS-UCS-Ext-B-4.txt"
		 ids-dump-insert-ccs-ranges
		 ucs "U-%08X\t%c\t%s\n"
		 (#x26000 . #x27FFF))
    (ucs-ext-b-5 "IDS-UCS-Ext-B-5.txt"
		 ids-dump-insert-ccs-ranges
		 ucs "U-%08X\t%c\t%s\n"
		 (#x28000 . #x29FFF))
    (ucs-ext-b-6 "IDS-UCS-Ext-B-6.txt"
		 ids-dump-insert-ccs-ranges
		 ucs "U-%08X\t%c\t%s\n"
		 (#x2A000 . #x2A6D6))
    (ucs-compat-supplement "IDS-UCS-Compat-Supplement.txt"
			   ids-dump-insert-ccs-ranges
			   ucs "U-%08X\t%c\t%s\n"
			   (#x2F800 . #x2FA1D))
    (daikanwa-01 "IDS-Daikanwa-01.txt" ids-dump-insert-daikanwa 00001 01449)
    (daikanwa-02 "IDS-Daikanwa-02.txt" ids-dump-insert-daikanwa 01450 04674)
    (daikanwa-03 "IDS-Daikanwa-03.txt" ids-dump-insert-daikanwa 04675 07410)
    (daikanwa-04 "IDS-Daikanwa-04.txt" ids-dump-insert-daikanwa 07411 11529)
    (daikanwa-05 "IDS-Daikanwa-05.txt" ids-dump-insert-daikanwa 11530 14414)
    (daikanwa-06 "IDS-Daikanwa-06.txt" ids-dump-insert-daikanwa 14415 17574)
    (daikanwa-07 "IDS-Daikanwa-07.txt" ids-dump-insert-daikanwa 17575 22677)
    (daikanwa-08 "IDS-Daikanwa-08.txt" ids-dump-insert-daikanwa 22678 28107)
    (daikanwa-09 "IDS-Daikanwa-09.txt" ids-dump-insert-daikanwa 28108 32803)
    (daikanwa-10 "IDS-Daikanwa-10.txt" ids-dump-insert-daikanwa 32804 38699)
    (daikanwa-11 "IDS-Daikanwa-11.txt" ids-dump-insert-daikanwa 38700 42209)
    (daikanwa-12 "IDS-Daikanwa-12.txt" ids-dump-insert-daikanwa 42210 48902)
    (daikanwa-ho "IDS-Daikanwa-ho.txt" ids-dump-insert-daikanwa-hokan)
    (cbeta "IDS-CBETA.txt"
	   ids-dump-insert-ccs-ranges
	   ideograph-cbeta "CB%05d\t%c\t%s\n"
	   (1 . 13363))
    ))

(defun ids-dump-insert-line (ccs line-spec code)
  (let ((chr (decode-char ccs code))
	id-list)
    (when chr
      (setq id-list (get-char-attribute chr 'ideographic-structure))
      (insert (format line-spec
		      code (decode-builtin-char ccs code)
		      (if id-list
			  (ids-format-list id-list)
			(char-to-string chr)))))))

(defun ids-dump-insert-ccs-ranges (ccs line-spec &rest ranges)
  (let (range code max-code)
    (while ranges
      (setq range (car ranges))
      (cond ((consp range)
	     (setq code (car range)
		   max-code (cdr range))
	     (while (<= code max-code)
	       (ids-dump-insert-line ccs line-spec code)
	       (setq code (1+ code))))
	    ((integerp range)
	     (ids-dump-insert-line ccs line-spec range))
	    (t (error 'wrong-type-argument range)))
      (setq ranges (cdr ranges)))))

(defun ids-dump-ranges (range filename)
  (with-temp-buffer
    (let* ((coding-system-for-write 'utf-8)
	   (spec (assq range ids-dump-file-specs-alist))
	   (file (nth 1 spec))
	   (func (nth 2 spec))
	   (args (nthcdr 3 spec)))
      (if (file-directory-p filename)
	  (setq filename (expand-file-name file filename)))
      (insert ";; -*- coding: utf-8 -*-\n")
      (apply func args)
      (write-region (point-min)(point-max)
		    filename))))

(defun ids-dump-insert-daikanwa (start end)
  (let ((i start)
	mdh-alist
	chr sal)
    (map-char-attribute
     (lambda (key val)
       (when (= (length val) 2)
	 (set-alist 'mdh-alist
		    (car val)
		    (put-alist (nth 1 val)
			       key
			       (cdr (assq (car val) mdh-alist)))))
       nil)
     'morohashi-daikanwa)
    (while (<= i end)
      (when (setq chr (decode-char 'ideograph-daikanwa i))
	(insert
	 (format "M-%05d \t%c\t%s\n"
		 i (decode-builtin-char 'ideograph-daikanwa i)
		 (ids-format-list
		  (get-char-attribute chr 'ideographic-structure)))))
      (when (setq sal (assq i mdh-alist))
	(setq sal (cdr sal))
	(when (setq chr (assq 1 sal))
	  (setq chr (cdr chr))
	  (insert
	   (format "M-%05d'\t%c\t%s\n"
		   i chr
		   (ids-format-list
		    (get-char-attribute chr 'ideographic-structure)))))
	(when (setq chr (assq 2 sal))
	  (setq chr (cdr chr))
	  (insert
	   (format "M-%05d\"\t%c\t%s\n"
		   i chr
		   (ids-format-list
		    (get-char-attribute chr 'ideographic-structure)))))
	)
      (setq i (1+ i)))))

(defun ids-dump-insert-daikanwa-hokan ()
  (let (chr sal)
    (map-char-attribute
     (lambda (key val)
       (when (and (eq (car val) 'ho)
		  (null (nthcdr 2 val)))
	 (setq sal (cons (cons (nth 1 val) key) sal)))
       nil)
     'morohashi-daikanwa)
    (setq sal (sort sal (lambda (a b) (< (car a)(car b)))))
    (dolist (cell sal)
      (setq chr (cdr cell))
      (insert
       (format "MH-%04d \t%c\t%s\n"
	       (car cell)
	       chr
	       (ids-format-list
		(get-char-attribute chr 'ideographic-structure)))))))

(dolist (spec ids-dump-file-specs-alist)
  (eval `(defun ,(intern (concat "ids-dump-" (symbol-name (car spec))))
	   (filename)
	   (interactive ,(concat "Fdump "
				 (file-name-sans-extension (nth 1 spec))
				 " : "))
	   (ids-dump-ranges ',(car spec) filename))))


;;; @ End.
;;;

(provide 'ids-dump)

;;; ids-dump.el ends here
