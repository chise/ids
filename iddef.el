;;; iddef.el --- Parser and utility for IDDef format files.

;; Copyright (C) 2001,2002 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <tomo@kanji.zinbun.kyoto-u.ac.jp>
;; Keywords: IDDef, IDS, IDC, Ideographs, UCS, Unicode

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

;; IDDef is a tab-separated format to describe some
;; character-attributes of each Ideographs mainly for Ideographic
;; structure.

;;; Code:

(require 'ids)
(require 'ids-util)

;;;###autoload
(defun iddef-read-buffer (buffer)
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
	      (when m-chr
		(unless (get-char-attribute m-chr 'ucs)
		  (put-char-attribute
		   m-chr
		   'ideographic-structure
		   (ideographic-structure-convert-to-daikanwa struct))))))
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
		(unless (get-char-attribute ret 'ucs)
		  (put-char-attribute ret 'ideographic-structure struct))))
	  )))))

;;;###autoload
(defun iddef-read-file (file)
  (interactive "fIDDef file : ")
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (iddef-read-buffer (current-buffer))))

;;;###autoload
(defun iddef-check-mapping-buffer (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (ucs radical plane code ccs chr ret)
      (while (re-search-forward "^U\\+\\([0-9A-F]+\\)\t\\([0-9]+\\)\t[^\t]*\t[^\t]*\t[^\t]*\t\\([0-9A-C]\\)-\\([0-9A-F][0-9A-F][0-9A-F][0-9A-F]\\)" nil t)
	(setq ucs (string-to-int (match-string 1) 16)
	      radical (string-to-int (match-string 2))
	      plane (string-to-int (match-string 3) 16)
	      code (string-to-int (match-string 4) 16))
	(setq ccs
	      (if (= plane 0)
		  (progn
		    (setq chr (decode-char 'chinese-big5 code))
		    (if (and (setq ret (get-char-attribute chr 'ucs))
			     (<= #xE000 ret)(<= ret #xF848))
			'chinese-big5-cdp))
		(intern (format "ideograph-hanziku-%d" plane))))
	(when ccs
	  (setq chr (decode-char ccs code))
	  (if (setq ret (or (get-char-attribute chr 'ucs)
			    (get-char-attribute chr '=>ucs)
			    (get-char-attribute chr '->ucs)))
	      (unless (= ret ucs)
		(put-char-attribute chr 'ucs-cdp ucs))
	    (if (eq (get-char-attribute chr ccs) code)
		(put-char-attribute chr 'ucs ucs)
	      (setq chr (define-char (list (cons 'ucs ucs)
					   (cons ccs code)))))
	    )
	  (unless (get-char-attribute chr 'ideographic-radical)
	    (put-char-attribute chr 'ideographic-radical radical))
	  )))))

;;;###autoload
(defun iddef-check-mapping-file (file)
  (interactive "fIDDef file : ")
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (iddef-check-mapping-buffer (current-buffer))))


;;; @ End.
;;;

(provide 'iddef)

;;; iddef.el ends here
