;;; install-ids.el --- installer of IDS files.

;;; Code:

(setq load-ids-simplify nil)

(defun install-ids-read-file (file simplify soft)
  (princ "Loading ")
  (princ file)
  (princ "...")
  (ids-read-file file simplify soft)
  (princ "done.\n"))

(install-ids-read-file "IDS-JIS-X0208-1990.txt" load-ids-simplify t)

(install-ids-read-file "IDS-UCS-Basic.txt" load-ids-simplify t)

(install-ids-read-file "IDS-UCS-Ext-A.txt" load-ids-simplify t)
(let ((i 1))
  (while (<= i 6)
    (install-ids-read-file (format "IDS-UCS-Ext-B-%d.txt" i)
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
;; (install-ids-read-file "IDS-HZK01.txt" load-ids-simplify t)
;; (install-ids-read-file "IDS-HZK02.txt" load-ids-simplify t)
;; (install-ids-read-file "IDS-HZK03.txt" load-ids-simplify t)

(princ "Updating char-feature `ideographic-structure'...")
(save-char-attribute-table 'ideographic-structure)
(princ "done.\n")

;;; install-ids.el ends hear
