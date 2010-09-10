;;; install-ids.el --- installer of IDS files.

;;; Code:

(setq load-ids-simplify nil)

(defun install-ids-read-file (file simplify soft)
  (princ "Loading ")
  (princ file)
  (princ "...")
  (ids-read-file file simplify soft)
  (princ "done.\n"))


;; (mount-char-attribute-table 'ideographic-products)
;; (map-char-attribute
;;  (lambda (c v)
;;    (remove-char-attribute c 'ideographic-products)
;;    nil)
;;  'ideographic-products)


(install-ids-read-file "IDS-JIS-X0208-1990.txt" load-ids-simplify t)

(install-ids-read-file "IDS-UCS-Basic.txt" load-ids-simplify t)

(install-ids-read-file "IDS-UCS-Ext-A.txt" load-ids-simplify t)

(let ((i 1))
  (while (<= i 6)
    (install-ids-read-file (format "IDS-UCS-Ext-B-%d.txt" i)
			   load-ids-simplify t)
    (setq i (1+ i))))

(install-ids-read-file "IDS-UCS-Ext-C.txt" load-ids-simplify t)

(let ((i 1))
  (while (<= i 3)
    (install-ids-read-file (format "IDS-CNS-%d.txt" i)
			   load-ids-simplify t)
    (setq i (1+ i))))

(let ((i 1))
  (while (<= i 12)
    (install-ids-read-file (format "IDS-Daikanwa-%02d.txt" i)
			   load-ids-simplify t)
    (setq i (1+ i))))

(install-ids-read-file "IDS-Daikanwa-dx.txt" load-ids-simplify t)

(install-ids-read-file "IDS-Daikanwa-ho.txt" load-ids-simplify t)

(install-ids-read-file "IDS-CBETA.txt" load-ids-simplify t)

(install-ids-read-file "IDS-CDP.txt" load-ids-simplify t)
(let ((i 1))
  (while (<= i 12)
    (install-ids-read-file (format "IDS-HZK%02d.txt" i)
			   load-ids-simplify t)
    (setq i (1+ i))))

(princ "Updating char-feature `ideographic-structure'...")
(save-char-attribute-table 'ideographic-structure)
(princ "done.\n")

(princ "Updating char-feature `ideographic-products'...")
(let* ((feature-dir
	(expand-file-name
	 "feature"
	 (expand-file-name
	  "character" chise-system-db-directory)))
       (p-file
	(expand-file-name "ideographic-products" feature-dir))
       old-p-file)
  (when (file-exists-p p-file)
    (setq old-p-file (make-temp-name p-file))
    (rename-file p-file old-p-file))
  (ids-update-index)
  (when old-p-file
    (delete-file old-p-file)))
(princ "done.\n")

;;; install-ids.el ends hear
