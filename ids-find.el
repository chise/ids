;;; ids-find.el --- search utility based on Ideographic-structures

;; Copyright (C) 2002 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <tomo@kanji.zinbun.kyoto-u.ac.jp>
;; Keywords: Kanji, Ideographs, search, IDS

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

;;; Code:

(defun ideographic-structure-char= (c1 c2)
  (or (eq c1 c2)
      (and c1 c2
	   (let ((m1 (char-ucs c1))
		 (m2 (char-ucs c2)))
	     (or (and m1 m2
		      (eq m1 m2))
		 (progn
		   (setq m1 (car (get-char-attribute c1 '<-radical))
			 m2 (car (get-char-attribute c2 '<-radical)))
		   (unless (characterp m1)
		     (setq m1 (or (find-char m1))))
		   (unless (characterp m2)
		     (setq m2 (find-char m2)))
		   (when (or m1 m2)
		     (ideographic-structure-char= m1 m2))))))))

(defun ideographic-structure-member-compare-parts (part s-part)
  (let (ret)
    (cond ((char-ref= part s-part #'ideographic-structure-char=))
	  ((listp s-part)
	   (if (setq ret (assq 'ideographic-structure s-part))
	       (ideographic-structure-member part (cdr ret))))
	  ((setq ret (get-char-attribute s-part 'ideographic-structure))
	   (ideographic-structure-member part ret)))))

(defun ideographic-structure-member (part structure)
  (or (progn
	(setq structure (cdr structure))
	(ideographic-structure-member-compare-parts part (car structure)))
      (progn
	(setq structure (cdr structure))
	(ideographic-structure-member-compare-parts part (car structure)))
      (progn
	(setq structure (cdr structure))
	(and (car structure)
	     (ideographic-structure-member-compare-parts
	      part (car structure))))))

;;;###autoload
(defun ideographic-structure-search-chars (parts)
  "Search Ideographs by PARTS."
  (interactive "sParts : ")
  (with-current-buffer (get-buffer-create " *ids-chars*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (map-char-attribute
     (lambda (c v)
       (when (every
	      (lambda (p)
		;; (member* p v :test #'char-ref=)
		(ideographic-structure-member p v))
	      parts)
	 (insert (format "%c\t%s\n"
			 c
			 (or (ideographic-structure-to-ids v)
			     v))))
       nil)
     'ideographic-structure)
    (goto-char (point-min)))
  (view-buffer " *ids-chars*"))


;;; @ End.
;;;

(provide 'ids-find)

;;; ids-find.el ends here
