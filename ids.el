;;; ids.el --- Parser and utility for Ideographic Description Sequence.

;; Copyright (C) 2001 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <tomo@kanji.zinbun.kyoto-u.ac.jp>
;; Keywords: IDS, IDC, Ideographs, UCS, Unicode

;; This file is a part of Tomoyo-Tools.

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

;;; Commentary:

;; Ideographic Description Sequence (IDS) is defined in ISO/IEC
;; 10646-1:2000 Annex F.

;;; Code:

(defun ids-parse-terminal (string)
  (if (>= (length string) 1)
      (let* ((chr (aref string 0))
	     (ucs (get-char-attribute chr 'ucs))
	     big5)
	(unless (and ucs (<= #x2FF0 ucs)(<= ucs #x2FFF))
	  (if (and ucs (<= #xE000 ucs)(<= ucs #xF8FF)
		   (setq big5 (get-char-attribute chr 'chinese-big5)))
	      (setq chr (decode-char 'chinese-big5-cdp big5)))
	  (cons chr
		(substring string 1))))))

(defun ids-parse-op-2 (string)
  (if (>= (length string) 1)
      (let* ((chr (aref string 0))
	     (ucs (get-char-attribute chr 'ucs)))
	(if (or (eq ucs #x2FF0)
		(eq ucs #x2FF1)
		(and (<= #x2FF4 ucs)(<= ucs #x2FFB)))
	    (cons chr
		  (substring string 1))))))

(defun ids-parse-op-3 (string)
  (if (>= (length string) 1)
      (let ((chr (aref string 0)))
	(if (memq chr '(?\u2FF2 ?\u2FF3))
	    (cons chr
		  (substring string 1))))))

(defun ids-parse-component (string)
  (let ((ret (ids-parse-element string))
	rret)
    (when ret
      (if (and (listp (car ret))
	       (setq rret (ideographic-structure-find-char
			   (cdr (assq 'ideographic-structure (car ret))))))
	  (cons rret (cdr ret))
	ret))))

(defun ids-parse-element (string)
  (let (ret op arg1 arg2 arg3)
    (cond ((ids-parse-terminal string))
	  ((setq ret (ids-parse-op-2 string))
	   (setq op (car ret))
	   (when (setq ret (ids-parse-component (cdr ret)))
	     (setq arg1 (car ret))
	     (when (setq ret (ids-parse-component (cdr ret)))
	       (setq arg2 (car ret))
	       (cons (list (list 'ideographic-structure op arg1 arg2))
		     (cdr ret)))))
	  ((setq ret (ids-parse-op-3 string))
	   (setq op (car ret))
	   (when (setq ret (ids-parse-component (cdr ret)))
	     (setq arg1 (car ret))
	     (when (setq ret (ids-parse-component (cdr ret)))
	       (setq arg2 (car ret))
	       (when (setq ret (ids-parse-component (cdr ret)))
		 (setq arg3 (car ret))
		 (cons (list (list 'ideographic-structure op arg1 arg2 arg3))
		       (cdr ret)))))))))

;;;###autoload
(defun ids-parse-string (string)
  (let ((ret (ids-parse-element string)))
    (if (= (length (cdr ret)) 0)
	(car ret))))


(require 'ids-util)

;;;###autoload
(defun ids-read-buffer (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (ucs
	  radical seq ret
	  char struct
	  morohashi m-chr)
      (while (re-search-forward
	      "^U\\+\\([0-9A-F]+\\)\t\\([0-9]+\\)\t[^\t]+\t\\([^\t\n]+\\)"
	      nil t)
	(setq ucs (string-to-int (match-string 1) 16)
	      radical (string-to-int (match-string 2))
	      seq (match-string 3))
	(setq ret (ids-parse-string seq))
	(when (and (consp ret)
		   (consp
		    (setq struct (cdr (assq 'ideographic-structure ret)))))
	  (setq char (decode-char 'ucs ucs))
	  (unless (get-char-attribute char 'ideograph-daikanwa)
	    (when (and (setq morohashi
			     (get-char-attribute char 'morohashi-daikanwa))
		       (>= (length morohashi) 3))
	      (setq m-chr
		    (if (= (nth 1 morohashi) 0)
			(decode-char 'ideograph-daikanwa
				     (setq morohashi (car morohashi)))
		      (setq morohashi (list (car morohashi)
					    (nth 1 morohashi)))
		      (map-char-attribute (lambda (char val)
					    (if (equal morohashi val)
						char))
					  'morohashi-daikanwa)))
	      (put-char-attribute
	       m-chr
	       'ideographic-structure
	       (ideographic-structure-convert-to-daikanwa struct))))
	  (put-char-attribute char 'ideographic-structure struct)
	  (dolist (ref (union
			(get-char-attribute char '->same-ideograph)
			(get-char-attribute char '->identical)))
	    (if (setq ret
		      (cond ((characterp ref) ref)
			    ((char-ref-p ref)
			     (find-char (plist-get ref :char)))
			    (t
			     (find-char ref))))
		(put-char-attribute ret 'ideographic-structure struct)))
	  )))))

;; (ids-read-buffer "IDDef1.txt")

;;; @ End.
;;;

(provide 'ids)

;;; ids.el ends here
