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
	code ucs)
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
	(t
	 (insert "<a href=\"http://mousai.kanji.zinbun.kyoto-u.ac.jp/char-desc?char=")
	 (insert str)
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

(defun www-batch-ids-find ()
  (let ((components (car command-line-args-left))
	(coded-charset-entity-reference-alist
	 (list*
	  '(=cbeta      "CB" 5 d)
	  '(=jef-china3 "JC3-" 4 X)
	  coded-charset-entity-reference-alist))
	is)
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
    (princ "Content-Type: text/html; charset=\"UTF-8\"

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
      (dolist (c (ideographic-products-find components))
	(setq is (char-feature c 'ideographic-structure))
	;; to avoid problems caused by wrong indexes
	(when (every (lambda (c)
		       (ideographic-structure-member c is))
		     components)
	  (www-ids-find-format-line c is)))
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
