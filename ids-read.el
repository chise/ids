;;; ids-read.el --- Reader for IDS-* files

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

;;;###autoload
(defun ids-read-buffer (buffer)
  (interactive "bBuffer = ")
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let (line chs ids char structure)
      (while (not (eobp))
	(unless (looking-at ";")
	  (setq line
		(split-string
		 (buffer-substring (point-at-bol)(point-at-eol))
		 "\t"))
	  (setq chs (car line)
		ids (nth 2 line))
	  (setq char
		(cond
		 ((string-match "U[-+]\\([0-9A-F]+\\)" chs)
		  (decode-char 'ucs
			       (string-to-int (match-string 1 chs) 16)))
		 ((string-match "M-\\([0-9]+\\)" chs)
		  (decode-char 'ideograph-daikanwa
			       (string-to-int (match-string 1 chs))))
		 ))
	  (when (and char
		     (>= (length ids) 3)
		     (not (string-match "\\?" ids))
		     (consp (setq structure (ids-parse-string ids))))
	    (put-char-attribute char
				'ideographic-structure
				(cdr (car structure))))
	  )
	(forward-line)
	))))

;;;###autoload
(defun ids-read-file (file)
  (interactive "fIDS file = ")
  (with-temp-buffer
    (insert-file-contents file)
    (ids-read-buffer (current-buffer))))


;;; @ End.
;;;

(provide 'ids-read)

;;; ids-read.el ends here
