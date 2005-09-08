(require 'ids-find)

(defun decode-url-string (string &optional coding-system)
  (if (> (length string) 0)
      (let ((i 0)
	    dest)
	(while (string-match "%\\([0-9A-F][0-9A-F]\\)" string i)
	  (setq dest (concat dest
			     (substring string i (match-beginning 0))
			     (char-to-string
			      (int-char
			       (string-to-int (match-string 1 string) 16))))
		i (match-end 0)))
	(decode-coding-string
	 (concat dest (substring string i))
	 coding-system))))

(defvar www-ids-find-tang-chars-file-name
  "~tomo/projects/chise/ids/www/tang-chars.udd")

(defun www-ids-find-format-line (c is)
  (let ((str (encode-coding-string (format "%c" c) 'utf-8-er))
	plane code ucs)
    (princ
     (with-temp-buffer
       (cond
	((string-match "&CB\\([0-9]+\\);" str)
	 (setq code (string-to-int (match-string 1 str)))
	 (insert "<a href=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/char-desc?char=")
	 (insert str)
	 (insert (format "\"><img alt=\"CB%05d\" src=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/glyphs/cb-gaiji/%02d/CB%05d.gif\">\n"
			 code (/ code 1000) code))
	 (insert (format "CB%05d</a>" code))
	 )
	((string-match "&JC3-\\([0-9A-F]+\\);" str)
	 (setq code (string-to-int (match-string 1 str) 16))
	 (insert "<a href=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/char-desc?char=")
	 (insert str)
	 (insert (format "\"><img alt=\"JC3-%04X\" src=\"http://kanji.zinbun.kyoto-u.ac.jp/db/CHINA3/Gaiji/%04x.gif\">\n"
			 code code))
	 (insert (format "JC3-%04X</a>" code))
	 )
	((string-match "&J\\(78\\|83\\|90\\|SP\\)-\\([0-9A-F]+\\);" str)
	 (setq plane (match-string 1 str)
	       code (string-to-int (match-string 2 str) 16))
	 (insert "<a href=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/char-desc?char=")
	 (insert str)
	 (insert (format "\"><img alt=\"J%s-%04X\" src=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/glyphs/JIS-%s/%02d-%02d.gif\">\n"
			 plane code plane
			 (- (lsh code -8) 32)
			 (- (logand code 255) 32)))
	 (insert (format "J%s-%04X</a>" plane code))
	 )
	((string-match "&G\\([01]\\)-\\([0-9A-F]+\\);" str)
	 (setq plane (string-to-int (match-string 1 str))
	       code (string-to-int (match-string 2 str) 16))
	 (insert "<a href=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/char-desc?char=")
	 (insert str)
	 (insert (format "\"><img alt=\"G%d-%04X\" src=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/glyphs/GB%d/%02d-%02d.gif\">\n"
			 plane code plane
			 (- (lsh code -8) 32)
			 (- (logand code 255) 32)))
	 (insert (format "G%d-%04X</a>" plane code))
	 )
	((string-match "&C\\([1-7]\\)-\\([0-9A-F]+\\);" str)
	 (setq plane (string-to-int (match-string 1 str))
	       code (string-to-int (match-string 2 str) 16))
	 (insert "<a href=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/char-desc?char=")
	 (insert str)
	 (insert (format "\"><img alt=\"C%d-%04X\" src=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/glyphs/CNS%d/%04X.gif\">\n"
			 plane code plane code))
	 (insert (format "C%d-%04X</a>" plane code))
	 )
	((string-match "&ZOB-\\([0-9]+\\);" str)
	 (setq code (string-to-int (match-string 1 str)))
	 (insert "<a href=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/char-desc?char=")
	 (insert str)
	 (insert (format "\"><img alt=\"ZOB-%04d\" src=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/glyphs/ZOB-1968/%04d.png\">\n"
			 code code))
	 (insert (format "ZOB-%04d</a>" code))
	 )
	(t
	 (insert "<a href=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/char-desc?char=")
         ;; (insert str)
	 (insert
	  (mapconcat (lambda (c)
		       (if (<= (char-int c) #x7F)
			   (char-to-string c)
			 (format "%%%02X" c)))
		     str ""))
	 (insert "\">")
	 (insert str)
	 (insert "</a>")
	 ))
       (goto-char (point-min))
       (while (search-forward "&" nil t)
	 (replace-match "&amp;" t 'literal))
       (buffer-string)
       ))
    (princ
     (or (if (setq ucs (or (char-ucs c)
			   (encode-char c 'ucs)))
	     (format " <a href=\"http://www.unicode.org/cgi-bin/GetUnihanData.pl?codepoint=%X\">%s</a>"
		     ucs
		     (cond ((<= ucs #xFFFF)
			    (format "U+%04X" ucs))
			   ((<= ucs #x10FFFF)
			    (format "U-%08X" ucs))))
	   "          ")))
    (princ " ")
    (when is
      (princ
       (with-temp-buffer
	 (insert
	  (encode-coding-string
	   (ideographic-structure-to-ids is)
	   'utf-8-jp-er))
	 (goto-char (point-min))
	 (while (re-search-forward "&CB\\([0-9]+\\);" nil t)
	   (setq code (string-to-int (match-string 1)))
	   (replace-match
	    (format "<img alt=\"CB%05d\" src=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/glyphs/cb-gaiji/%02d/CB%05d.gif\">"
		    code (/ code 1000) code)
	    t 'literal))
	 (buffer-string))))
    (when (and ucs
	       (with-current-buffer
		   (find-file-noselect
		    www-ids-find-tang-chars-file-name)
		 (goto-char (point-min))
		 (re-search-forward (format "^%d$" ucs) nil t)))
      (princ
       (format " <a href=\"http://coe21.zinbun.kyoto-u.ac.jp/djvuchar?query=%s\">"
	       (mapconcat
		(lambda (c)
		  (format "%%%02X" (char-int c)))
		(encode-coding-string (char-to-string c)
				      'utf-8-jp)
		"")))
      (princ (encode-coding-string "⇒[唐代拓本]</a>" 'utf-8-jp-er)))
    (princ "<br>\n")))

(defun www-ids-insert-chars-including-components (components)
  (let (is)
    (dolist (c (ideographic-products-find components))
      (setq is (char-feature c 'ideographic-structure))
      ;; to avoid problems caused by wrong indexes
      (when (every (lambda (cc)
		     (ideographic-structure-member cc is))
		   components)
	(princ "<li>")
	(www-ids-find-format-line c is)
	(princ "<ul>\n")
	(www-ids-insert-chars-including-components (char-to-string c))
	(princ "</ul>\n")
	)
      )))

(defun www-batch-ids-find ()
  (let ((components (car command-line-args-left))
	(coded-charset-entity-reference-alist
	 (list*
	  '(=cns11643-1		"C1-" 4 X)
	  '(=cns11643-2		"C2-" 4 X)
	  '(=cns11643-3		"C3-" 4 X)
	  '(=cns11643-4		"C4-" 4 X)
	  '(=cns11643-5		"C5-" 4 X)
	  '(=cns11643-6		"C6-" 4 X)
	  '(=cns11643-7		"C7-" 4 X)
	  '(=gb2312		"G0-" 4 X)
	  '(=gb12345		"G1-" 4 X)
	  '(=jis-x0208@1990	"J90-" 4 X)
	  '(=jis-x0212		"JSP-" 4 X)
	  '(=cbeta		"CB" 5 d)
	  '(=jef-china3		"JC3-" 4 X)
	  '(=jis-x0208@1978	"J78-" 4 X)
	  '(=jis-x0208@1983	"J83-" 4 X)
	  '(=daikanwa		"M-" 5 d)
	  coded-charset-entity-reference-alist))
	)
    (setq command-line-args-left (cdr command-line-args-left))
    (cond
     ((stringp components)
      (if (string-match "^components=" components)
	  (setq components (substring components (match-end 0))))
      (setq components
	    (if (> (length components) 0)
		(decode-url-string components 'utf-8-jp-er)
	      nil))
      )
     (t
      (setq components nil)
      ))
    (princ "Content-Type: text/html; charset=UTF-8

<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
            \"http://www.w3.org/TR/html4/loose.dtd\">
<html lang=\"ja\">
<head>
<title>CHISE IDS Find</title>
</head>

<body>

<h1>")
    (princ (encode-coding-string "CHISE IDS 漢字検索" 'utf-8-jp-er))
    (princ "</h1>
<p>
<form action=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/ids-find\" method=\"GET\">
")
    (princ (encode-coding-string "部品文字列" 'utf-8-jp-er))
    (princ " <input type=\"text\" name=\"components\" size=\"30\" maxlength=\"30\" value=\"")
    (if (> (length components) 0)
	(princ (encode-coding-string components 'utf-8-jp-er)))
    (princ "\">
<input type=\"submit\" value=\"")
    (princ (encode-coding-string "検索開始" 'utf-8-jp-er))
    (princ "\">
</form>

")
    (cond
     (components
      ;; (map-char-attribute
      ;;  (lambda (c v)
      ;;    (when (every (lambda (p)
      ;;                   (ideographic-structure-member p v))
      ;;                 components)
      ;;      (princ (encode-coding-string
      ;;              (ids-find-format-line c v)
      ;;              'utf-8-jp-er))
      ;;      (princ "<br>\n")
      ;;      )
      ;;    nil)
      ;;  'ideographic-structure)
      (when (= (length components) 1)
	(www-ids-find-format-line (aref components 0)
				  (char-feature (aref components 0)
						'ideographic-structure)))
      ;; (dolist (c (ideographic-products-find components))
      ;;   (setq is (char-feature c 'ideographic-structure))
      ;;   ;; to avoid problems caused by wrong indexes
      ;;   (when (every (lambda (c)
      ;;                  (ideographic-structure-member c is))
      ;;                components)
      ;;     (www-ids-find-format-line c is)))
      (princ "<ul>\n")
      (www-ids-insert-chars-including-components components)
      (princ "</ul>\n")
      )
     (t
      (princ (encode-coding-string "<hr>
<p>
指定した部品を全て含む漢字の一覧を表示します。
<p>
CHISE で用いられる実態参照形式（例：&amp;M-00256;）で部品を指定する事もできます。" 'utf-8-jp-er))
      ))
    (princ "<hr>")
    (princ
     (format
      "Powered by <a
href=\"http://kanji.zinbun.kyoto-u.ac.jp/projects/chise/xemacs/\"
>XEmacs CHISE</a> %s."
      xemacs-chise-version))
    (princ "
</body>
</html>
")))
