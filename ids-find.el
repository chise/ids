;;; ids-find.el --- search utility based on Ideographic-structures

;; Copyright (C) 2002,2003,2005,2006,2007,2017 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <tomo@kanji.zinbun.kyoto-u.ac.jp>
;; Keywords: Kanji, Ideographs, search, IDS, CHISE, UCS, Unicode

;; This file is a part of CHISE IDS.

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

(defun ids-index-store-char (product component)
  (let ((ret (get-char-attribute component 'ideographic-products)))
    (unless (memq product ret)
      (put-char-attribute component 'ideographic-products
			  (cons product ret))
      (when (setq ret (char-feature component 'ideographic-structure))
	(ids-index-store-structure product ret)))
    ))

(defun ids-index-store-structure (product structure)
  (let (ret)
    (dolist (cell (cdr structure))
      (if (char-ref-p cell)
	  (setq cell (plist-get cell :char)))
      (cond ((characterp cell)
	     (ids-index-store-char product cell))
	    ((setq ret (assq 'ideographic-structure cell))
	     (ids-index-store-structure product (cdr ret)))
	    ((setq ret (find-char cell))
	     (ids-index-store-char product ret))
	    ))))

;;;###autoload
(defun ids-update-index ()
  (interactive)
  (map-char-attribute
   (lambda (c v)
     (ids-index-store-structure c v)
     nil)
   'ideographic-structure)
  (save-char-attribute-table 'ideographic-products))


(mount-char-attribute-table 'ideographic-products)

;;;###autoload
(defun ids-find-all-products (char)
  (let (dest)
    (dolist (cell (char-feature char 'ideographic-products))
      (unless (memq cell dest)
	(setq dest (cons cell dest)))
      (setq dest (union dest (ids-find-all-products cell))))
    dest))

(defun of-component-features ()
  (let (dest)
    (dolist (feature (char-attribute-list))
      (when (string-match "^<-.*[@/]component\\(/[^*/]+\\)*$"
			  (symbol-name feature))
	(push feature dest)))
    (cons '<-mistakable
	  dest)))

(defun to-component-features ()
  (let (dest)
    (dolist (feature (char-attribute-list))
      (when (string-match "^->.*[@/]component\\(/[^*/]+\\)*$"
			  (symbol-name feature))
	(push feature dest)))
    (cons '->formed
	  dest)))

;;;###autoload
(defun char-component-variants (char)
  (let ((dest (list char))
	ret uchr)
    (dolist (feature (to-component-features))
      (if (setq ret (get-char-attribute char feature))
	  (dolist (c ret)
	    (setq dest (union dest (char-component-variants c))))))
    (cond
     ;; ((setq ret (some (lambda (feature)
     ;;                    (get-char-attribute char feature))
     ;;                  (to-component-features)))
     ;;  (dolist (c ret)
     ;;    (setq dest (union dest (char-component-variants c))))
     ;;  )
     ((setq ret (get-char-attribute char '->ucs-unified))
      (setq dest (cons char ret))
      (dolist (c dest)
	(setq dest (union dest
                          (some (lambda (feature)
				  (get-char-attribute c feature))
				(of-component-features))
			  )))
      )
     ((and (setq ret (get-char-attribute char '=>ucs))
	   (setq uchr (decode-char '=ucs ret)))
      (setq dest (cons uchr (char-variants uchr)))
      (dolist (c dest)
	(setq dest (union dest
                          (some (lambda (feature)
				  (get-char-attribute c feature))
				(of-component-features))
			  )))
      )
     (t
      (map-char-family
       (lambda (c)
	 (unless (memq c dest)
	   (setq dest (cons c dest)))
	 (setq dest
	       (union dest
		      (some (lambda (feature)
			      (char-feature c feature))
			    (of-component-features))
		      ))
	 nil)
       char)
      ))
    dest))

;;;###autoload
(defun ideographic-products-find (&rest components)
  (if (stringp (car components))
      (setq components (string-to-char-list (car components))))
  (let (dest products)
    (dolist (variant (char-component-variants (car components)))
      (setq products
	    (union products
		   (get-char-attribute variant 'ideographic-products))))
    (setq dest products)
    (while (and dest
		(setq components (cdr components)))
      (setq products nil)
      (dolist (variant (char-component-variants (car components)))
	(setq products
	      (union products
		     (get-char-attribute variant 'ideographic-products))))
      (setq dest (intersection dest products)))
    dest))

(defun ideograph-find-products-with-variants (components &optional ignored-chars)
  (if (stringp components)
      (setq components (string-to-char-list components)))
  (let (dest products)
    (dolist (variant (char-component-variants (car components)))
      (setq products
	    (union products
		   (set-difference
		    (get-char-attribute variant 'ideographic-products)
		    ignored-chars))))
    (setq dest products)
    (while (and dest
		(setq components (cdr components)))
      (setq products nil)
      (dolist (variant (char-component-variants (car components)))
	(setq products
	      (union products
		     (set-difference
		      (get-char-attribute variant 'ideographic-products)
		      ignored-chars))))
      (setq dest (intersection dest products)))
    dest))

(defun ideograph-find-products (components &optional ignored-chars)
  (if (stringp components)
      (setq components (string-to-char-list components)))
  (let (dest products)
    ;; (dolist (variant (char-component-variants (car components)))
    ;;   (setq products
    ;;         (union products
    ;;                (get-char-attribute variant 'ideographic-products))))
    ;; (setq dest products)
    (setq dest (get-char-attribute (car components) 'ideographic-products))
    (while (and dest
		(setq components (cdr components)))
      ;; (setq products nil)
      ;; (dolist (variant (char-component-variants (car components)))
      ;;   (setq products
      ;;         (union products
      ;;                (get-char-attribute variant 'ideographic-products))))
      (setq products (get-char-attribute (car components) 'ideographic-products))
      (setq dest (intersection dest products)))
    dest))


(defun ideographic-structure-char= (c1 c2)
  (or (eq c1 c2)
      (and c1 c2
	   (let ((m1 (char-ucs c1))
		 (m2 (char-ucs c2)))
	     (or (and m1 m2
		      (eq m1 m2))
		 (memq c1 (char-component-variants c2)))))))

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
  (or (memq component structure)
      (progn
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

(defun ids-insert-chars-including-components* (components
					       &optional level ignored-chars)
  (unless level
    (setq level 0))
  (let (is i as bs)
    (dolist (c (sort (copy-tree (ideograph-find-products components
							 ignored-chars))
		     (lambda (a b)
		       (if (setq as (char-total-strokes a))
			   (if (setq bs (char-total-strokes b))
			       (if (= as bs)
				   (ideograph-char< a b)
				 (< as bs))
			     t)
			 (ideograph-char< a b)))))
      (unless (memq c ignored-chars)
	(setq is (char-feature c 'ideographic-structure))
	(setq i 0)
	(while (< i level)
	  (insert "\t")
	  (setq i (1+ i)))
	(insert (ids-find-format-line c is))
	(setq ignored-chars
	      (ids-insert-chars-including-components*
	       (char-to-string c) (1+ level)
	       (cons c ignored-chars))))
      )
    )
  ignored-chars)

(defun ids-insert-chars-including-components (components
					      &optional level ignored-chars)
  (unless level
    (setq level 0))
  (setq ignored-chars
	(nreverse
	 (ids-insert-chars-including-components* components
						 level ignored-chars)))
  (let (is i as bs)
    (dolist (c ignored-chars)
      (dolist (vc (char-component-variants c))
	(unless (memq vc ignored-chars)
	  (when (setq is (get-char-attribute vc 'ideographic-structure))
	    (setq i 0)
	    (while (< i level)
	      (insert "\t")
	      (setq i (1+ i)))
	    (insert (ids-find-format-line vc is))
	    (setq ignored-chars
		  (ids-insert-chars-including-components*
		   (char-to-string vc) (1+ level)
		   (cons vc ignored-chars)))))))
    (dolist (c (sort (copy-tree (ideograph-find-products-with-variants
				 components ignored-chars))
		     (lambda (a b)
		       (if (setq as (char-total-strokes a))
			   (if (setq bs (char-total-strokes b))
			       (if (= as bs)
				   (ideograph-char< a b)
				 (< as bs))
			     t)
			 (ideograph-char< a b)))))
      (unless (memq c ignored-chars)
	(setq is (get-char-attribute c 'ideographic-structure))
	(setq i 0)
	(while (< i level)
	  (insert "\t")
	  (setq i (1+ i)))
	(insert (ids-find-format-line c is))
	(setq ignored-chars
	      (ids-insert-chars-including-components*
	       (char-to-string c) (1+ level)
	       (cons c ignored-chars))))
      )
    )
  ignored-chars)

;;;###autoload
(defun ids-find-chars-including-components (components)
  "Search Ideographs whose structures have COMPONENTS."
  (interactive "sComponents : ")
  (with-current-buffer (get-buffer-create ids-find-result-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (ids-insert-chars-including-components components 0 nil)
    ;; (let ((ignored-chars
    ;;        (nreverse
    ;;         (ids-insert-chars-including-components components 0 nil
    ;;                                                #'ideograph-find-products)))
    ;;       rest)
    ;;   (setq rest ignored-chars)
    ;;   ;; (dolist (c rest)
    ;;   ;;   (setq ignored-chars
    ;;   ;;         (union ignored-chars
    ;;   ;;                (ids-insert-chars-including-components
    ;;   ;;                 (list c) 0 ignored-chars
    ;;   ;;                 #'ideograph-find-products-with-variants))))
    ;;   (ids-insert-chars-including-components components 0 ignored-chars
    ;;                                          #'ideograph-find-products-with-variants))
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
    (map-char-attribute
     (lambda (c v)
       (when (ideographic-structure-repertoire-p v components)
	 (insert (ids-find-format-line c v))))
     'ideographic-structure)
    (goto-char (point-min)))
  (view-buffer ids-find-result-buffer))


;;; @ End.
;;;

(provide 'ids-find)

;;; ids-find.el ends here
