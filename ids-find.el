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

(defun ideographic-structure-member-compare-components (component s-component)
  (let (ret)
    (cond ((char-ref= component s-component #'ideographic-structure-char=))
	  ((listp s-component)
	   (if (setq ret (assq 'ideographic-structure s-component))
	       (ideographic-structure-member component (cdr ret))))
	  ((setq ret (get-char-attribute s-component 'ideographic-structure))
	   (ideographic-structure-member component ret)))))

;;;###autoload
(defun ideographic-structure-member (component structure)
  "Return non-nil if COMPONENT is included in STRUCTURE."
  (or (progn
	(setq structure (cdr structure))
	(ideographic-structure-member-compare-components
	 component (car structure)))
      (progn
	(setq structure (cdr structure))
	(ideographic-structure-member-compare-components
	 component (car structure)))
      (progn
	(setq structure (cdr structure))
	(and (car structure)
	     (ideographic-structure-member-compare-components
	      component (car structure))))))


;;;###autoload
(defun ideographic-structure-repertoire-p (structure components)
  "Return non-nil if STRUCTURE can be constructed by a subset of COMPONENTS."
  (and structure
       (let (ret s-component)
	 (catch 'tag
	   (while (setq structure (cdr structure))
	     (setq s-component (car structure))
	     (unless (characterp s-component)
	       (if (setq ret (find-char s-component))
		   (setq s-component ret)))
	     (unless (cond
		      ((listp s-component)
		       (if (setq ret (assq 'ideographic-structure s-component))
			   (ideographic-structure-repertoire-p
			    (cdr ret) components)))
		      ((member* s-component components
				:test #'ideographic-structure-char=))
		      ((setq ret
			     (get-char-attribute s-component
						 'ideographic-structure))
		       (ideographic-structure-repertoire-p ret components)))
	       (throw 'tag nil)))
	   t))))


(defvar ids-find-result-buffer "*ids-chars*")

(defun ids-find-format-line (c v)
  (format "%c\t%s\t%s\n"
	  c
	  (or (let ((ucs (or (char-ucs c)
			     (encode-char c 'ucs))))
		(if ucs
		    (cond ((<= ucs #xFFFF)
			   (format "    U+%04X" ucs))
			  ((<= ucs #x10FFFF)
			   (format "U-%08X" ucs)))))
	      "          ")
	  (or (ideographic-structure-to-ids v)
	      v)))

;;;###autoload
(defun ids-find-chars-including-components (components)
  "Search Ideographs whose structures have COMPONENTS."
  (interactive "sComponents : ")
  (with-current-buffer (get-buffer-create ids-find-result-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (map-char-attribute
     (lambda (c v)
       (when (every (lambda (p)
		      (ideographic-structure-member p v))
		    components)
         (insert (ids-find-format-line c v)))
       nil)
     'ideographic-structure)
    (goto-char (point-min)))
  (view-buffer ids-find-result-buffer))

;;;###autoload
(define-obsolete-function-alias 'ideographic-structure-search-chars
  'ids-find-chars-including-components)

;;;###autoload
(defun ids-find-chars-covered-by-components (components)
  "Search Ideographs which structures are consisted by subsets of COMPONENTS."
  (interactive "sComponents: ")
  (if (stringp components)
      (setq components (string-to-char-list components)))
  (with-current-buffer (get-buffer-create ids-find-result-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let (ucs jis)
      (map-char-attribute
       (lambda (c v)
	 (when (ideographic-structure-repertoire-p v components)
	   (insert
	    (ids-find-format-line c v)
            ;; (format "%c\t%s\t%s\n"
            ;;         c
            ;;         (or
            ;;          (when (setq ucs (char-ucs c))
            ;;            (or
            ;;             (when (setq jis
            ;;                         (encode-char
            ;;                          (decode-char
            ;;                           'ucs-jis (char-ucs c))
            ;;                          'japanese-jisx0208-1990))
            ;;               (format "J0-%04X" jis))))
            ;;          "")
            ;;         (or (ideographic-structure-to-ids v)
            ;;             v))
	    )))
       'ideographic-structure))
    (goto-char (point-min)))
  (view-buffer ids-find-result-buffer))


;;; @ End.
;;;

(provide 'ids-find)

;;; ids-find.el ends here
