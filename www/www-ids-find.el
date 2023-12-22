(require 'ids-find)
(require 'cwiki-common)

(defun www-format-encode-string (string &optional without-tags as-body)
  (with-temp-buffer
    (insert string)
    (let (plane code subcode start end char variants ret rret)
      (when as-body
	(goto-char (point-min))
	(while (search-forward "&" nil t)
	  (replace-match "&amp;" nil t)))
      (goto-char (point-min))
      (while (search-forward "<" nil t)
	(replace-match "&lt;" nil t))
      (goto-char (point-min))
      (while (search-forward ">" nil t)
	(replace-match "&gt;" nil t))
      (if without-tags
	  (encode-coding-region (point-min)(point-max) 'utf-8-mcs-er)
	(let ((coded-charset-entity-reference-alist
	       est-coded-charset-entity-reference-alist))
	  (encode-coding-region (point-min)(point-max) 'utf-8-mcs-er)

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(A-\\|G-\\|g2-\\|R-\\)?CB\\([0-9]+\\);" nil t)
	    (setq code (string-to-int (match-string 2)))
	    (replace-match
	     (format "<img alt=\"CB%05d\" src=\"%s/cb-gaiji/%02d/CB%05d.gif\"
style=\"%s\">"
		     code
		     chise-wiki-legacy-bitmap-glyphs-url
		     (/ code 1000) code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(o-\\|G-\\|g2-\\|R-\\)?J\\(78\\|83\\|90\\|97\\|SP\\)-\\([0-9A-F][0-9A-F][0-9A-F][0-9A-F]\\);" nil t)
	    (setq plane (match-string 2)
		  code (string-to-int (match-string 3) 16))
	    (replace-match
	     (format "<img alt=\"J%s-%04X\" src=\"%s/JIS-%s/%02d-%02d.gif\"
style=\"%s\">"
		     plane code
		     chise-wiki-legacy-bitmap-glyphs-url
		     plane
		     (- (lsh code -8) 32)
		     (- (logand code 255) 32)
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(o-\\|G-\\|g2-\\|R-\\)?J0-\\([0-9A-F][0-9A-F][0-9A-F][0-9A-F]\\);" nil t)
	    (setq code (string-to-int (match-string 2) 16))
	    (replace-match
	     (format "<img alt=\"J0-%04X\" src=\"%s/JIS-90/%02d-%02d.gif\"
style=\"%s\">"
		     code
		     chise-wiki-legacy-bitmap-glyphs-url
		     (- (lsh code -8) 32)
		     (- (logand code 255) 32)
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(o-\\|G-\\|g2-\\|R-\\)?HD-\\(JA\\|JB\\|JC\\|JD\\|FT\\|IA\\|IB\\|HG\\)-\\([0-9A-F][0-9A-F][0-9A-F][0-9A-F]\\);" nil t)
	    (setq plane (match-string 2)
		  code (string-to-int (match-string 3) 16))
	    (replace-match
	     (format "<img alt=\"HD-%s-%04X\" src=\"%s/IVD/HanyoDenshi/%s%02d%02d.png\"
style=\"%s\">"
		     plane code
		     chise-wiki-legacy-bitmap-glyphs-url
		     plane
		     (- (lsh code -8) 32)
		     (- (logand code 255) 32)
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(o-\\|G-\\|g2-\\|R-\\)?HD-\\(IP\\|JT\\)-\\([0-9A-F][0-9A-F][0-9A-F][0-9A-F]\\);" nil t)
	    (setq plane (match-string 2)
		  code (string-to-int (match-string 3) 16))
	    (replace-match
	     (format "<img alt=\"HD-%s-%04X\" src=\"%s/IVD/HanyoDenshi/%s%04X.png\"
style=\"%s\">"
		     plane code
		     chise-wiki-legacy-bitmap-glyphs-url
		     plane code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(o-\\|G-\\|g2-\\|R-\\)?HD-KS-\\([0-9]+\\);" nil t)
	    (setq code (string-to-int (match-string 2)))
	    (replace-match
	     (format "<img alt=\"HD-KS%06d\" src=\"%s/IVD/HanyoDenshi/KS%06d.png\"
style=\"vertical-align:middle\">"
		     code
		     chise-wiki-legacy-bitmap-glyphs-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(o-\\|G-\\|g2-\\|R-\\)?HD-TK-\\([0-9]+\\);" nil t)
	    (setq code (string-to-int (match-string 2)))
	    (replace-match
	     (format "<img alt=\"HD-KS%06d\" src=\"%s/IVD/HanyoDenshi/TK%08d.png\"
style=\"vertical-align:middle\">"
		     code
		     chise-wiki-legacy-bitmap-glyphs-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&G\\([01]\\)-\\([0-9A-F][0-9A-F][0-9A-F][0-9A-F]\\);" nil t)
	    (setq plane (string-to-int (match-string 1))
		  code (string-to-int (match-string 2) 16))
	    (replace-match
	     (format "<img alt=\"GB%d-%04X\" src=\"%s/GB%d/%02d-%02d.gif\"
style=\"%s\">"
		     plane code
		     chise-wiki-legacy-bitmap-glyphs-url
		     plane
		     (- (lsh code -8) 32)
		     (- (logand code 255) 32)
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(R-\\)?C\\([1-7]\\)-\\([0-9A-F][0-9A-F][0-9A-F][0-9A-F]\\);" nil t)
	    (setq plane (string-to-int (match-string 2))
		  code (string-to-int (match-string 3) 16))
	    (replace-match
	     (format "<img alt=\"CNS%d-%04X\" src=\"%s/CNS%d/%04X.gif\"
style=\"%s\">"
		     plane code
		     chise-wiki-legacy-bitmap-glyphs-url
		     plane code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(R-\\)?JC3-\\([0-9A-F][0-9A-F][0-9A-F][0-9A-F]\\);" nil t)
	    (setq code (string-to-int (match-string 2) 16))
	    (replace-match
	     (format "<img alt=\"JC3-%04X\" src=\"%s/JEF-CHINA3/%04X.png\">"
		     code chise-wiki-bitmap-glyph-image-url code)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(A-\\)?ZOB-\\([0-9]+\\);" nil t)
	    (setq code (string-to-int (match-string 2)))
	    (replace-match
	     (format "<img alt=\"ZOB-%04d\" src=\"%s/ZOB-1968/%04d.png\"
style=\"vertical-align:middle\">"
		     code
		     chise-wiki-legacy-bitmap-glyphs-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(A2-\\|g2-\\|R-\\)?DJT-\\([0-9]+\\);" nil t)
	    (setq code (string-to-int (match-string 2)))
	    (replace-match
	     (format "<img alt=\"DJT-%05d\" src=\"%s/%05d.png\"
style=\"vertical-align:middle; width: auto; max-height: 60px\">"
		     code
		     chise-wiki-daijiten-bitmap-glyphs-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&SW-JIGUGE\\([45]?\\)-\\([0-9]+\\);" nil t)
	    (setq subcode (match-string 1)
		  code (string-to-int (match-string 2)))
	    (setq plane
		  (if (string= subcode "")
		      "5"
		    subcode))
	    (replace-match
	     (format "<img alt=\"SW-JIGUGE%s-%05d\" src=\"%s/ShuoWen/Jiguge%s/%05d.png\"
style=\"vertical-align:middle; width: auto; max-height: 80px\">"
		     plane code
		     chise-wiki-legacy-bitmap-glyphs-url
		     plane code)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&HNG\\([0-9]+\\)-\\([0-9][0-9][0-9][0-9]\\)\\([0-9]\\);" nil t)
	    (setq plane (match-string 1)
		  code (string-to-int (match-string 2))
		  subcode (string-to-int (match-string 3)))
	    (setq subcode
		  (if (eq subcode 0)
		      ""
		    (char-to-string (decode-char 'ascii (+ 96 subcode)))))
	    (replace-match
	     (format
	      "<img alt=\"HNG%s-%04d%s\" src=\"%s/%s/%04d%s.png\" style=\"
vertical-align:middle; width: auto; max-height: 60px\">"
	      plane code subcode
	      chise-wiki-hng-bitmap-glyphs-url
	      plane code subcode
	      )
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(R-\\)?CHISE-HDIC-TSJ\\([0-9A-F]+\\);" nil t)
	    (setq code (string-to-int (match-string 2) 16))
	    (setq char (decode-char '===chise-hdic-tsj code))
	    (when (setq ret (get-char-attribute char '=hdic-tsj-glyph-id))
	      (replace-match
	       (format
		"<img alt=\"HDIC-TSJ-%s\" src=\"https://viewer.hdic.jp/img/tsj/%s.jpg\" style=\"
vertical-align:middle; width: auto; max-height: 60px\">"
		ret ret)
	       t 'literal)))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(R-\\)?CHISE-HDIC-SYP\\([0-9A-F]+\\);" nil t)
	    (setq code (string-to-int (match-string 2) 16))
	    (setq char (decode-char '===chise-hdic-syp code))
	    (when (setq ret (get-char-attribute char '=hdic-syp-entry-id))
	      (replace-match
	       (format
		"<img alt=\"HDIC-SYP-%s\" src=\"https://viewer.hdic.jp/img/syp/%s\" style=\"
vertical-align:middle; width: auto; max-height: 60px\">"
		ret ret)
	       t 'literal)))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(R-\\)?CHISE-HDIC-KTB\\([0-9A-F]+\\);" nil t)
	    (setq code (string-to-int (match-string 2) 16))
	    (setq char (decode-char '===chise-hdic-ktb code))
	    (when (setq ret (get-char-attribute char '=hdic-ktb-entry-id))
	      (replace-match
	       (format
		"<img alt=\"HDIC-KTB-%s\" src=\"https://hdic.chise.org/img/ktb/%s.jpg\" style=\"
vertical-align:middle; width: auto; max-height: 60px\">"
		ret ret)
	       t 'literal)))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(o-\\|G-\\|g2-\\|R-\\)?AJ1-\\([0-9]+\\);" nil t)
	    (setq code (string-to-int (match-string 2)))
	    (replace-match
	     (format "<img alt=\"AJ1-%05d\" src=\"%s/IVD/AdobeJapan1/CID+%d.png\"
style=\"vertical-align:middle\">"
		     code
		     chise-wiki-legacy-bitmap-glyphs-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(A-\\|o-\\|G-\\|g2-\\|R-\\)?MJ\\([0-9]+\\);" nil t)
	    (setq code (string-to-int (match-string 2)))
	    (replace-match
	     (format "<img alt=\"MJ%06d\" src=\"https://moji.or.jp/mojikibansearch/img/MJ/MJ%06d.png\"
style=\"vertical-align:middle; width: 48px; height: 48px\">"
		     code
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(o-\\|G-\\|g2-\\)?IU[+-]\\([0-9A-F]+\\);" nil t)
	    (setq code (string-to-int (match-string 2) 16))
	    (replace-match
	     (format "<img alt=\"u%04x\" src=\"%s/u%04x.svg\"
style=\"vertical-align:middle; width: 48px; height: 48px\">"
		     code
		     chise-wiki-glyphwiki-glyph-image-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(o-\\|G-\\|g2-\\|R-\\)?KU[+-]\\([0-9A-F]+\\);" nil t)
	    (setq code (string-to-int (match-string 2) 16))
	    (replace-match
	     (format "<img alt=\"u%04x-k\" src=\"%s/u%04x-k.svg\"
style=\"vertical-align:middle; width: 48px; height: 48px\">"
		     code
		     chise-wiki-glyphwiki-glyph-image-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&A-\\(comp\\|cgn\\)U[+-]\\([0-9A-F]+\\);" nil t)
	    (setq code (string-to-int (match-string 2) 16))
	    (replace-match
	     (format "<img alt=\"u%04x\" src=\"%s/u%04x.svg\"
style=\"vertical-align:middle; width: 48px; height: 48px\">"
		     code
		     chise-wiki-glyphwiki-glyph-image-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward
		  "&\\(A-\\|g2-\\)?U-i\\([0-9]+\\)\\+\\([0-9A-F]+\\);"
		  nil t)
	    (setq plane (string-to-int (match-string 2))
		  code (string-to-int (match-string 3) 16))
	    (replace-match
	     (format "<img alt=\"u%04x-itaiji-%03d\" src=\"%s/u%04x-itaiji-%03d.svg\"
style=\"vertical-align:middle; width: 48px; height: 48px\">"
		     code
		     plane
		     chise-wiki-glyphwiki-glyph-image-url
		     code
		     plane
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&A-IWDSU\\+\\([0-9A-F]+\\);" nil t)
	    (setq code (string-to-int (match-string 1) 16))
	    (replace-match
	     (format "<img alt=\"A-IWDSU+%04x\" src=\"%s/u%04x.svg\"
style=\"vertical-align:middle; width: 48px; height: 48px\">"
		     code
		     chise-wiki-glyphwiki-glyph-image-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward
		  "&\\(A-\\)?CDP-i\\([0-9]+\\)-\\([0-9A-F]+\\);"
		  nil t)
	    (setq plane (string-to-int (match-string 2))
		  code (string-to-int (match-string 3) 16))
	    (replace-match
	     (format "<img alt=\"cdp-%04x-itaiji-%03d\" src=\"%s/cdp-%04x-itaiji-%03d.svg\"
style=\"vertical-align:middle; width: 48px; height: 48px\">"
		     code
		     plane
		     chise-wiki-glyphwiki-glyph-image-url
		     code
		     plane
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward
		  "&\\(A-\\)?CDP-v\\([0-9]+\\)-\\([0-9A-F]+\\);"
		  nil t)
	    (setq plane (string-to-int (match-string 2))
		  code (string-to-int (match-string 3) 16))
	    (replace-match
	     (format "<img alt=\"cdp-%04x-var-%03d\" src=\"%s/cdp-%04x-var-%03d.svg\"
style=\"vertical-align:middle; width: 48px; height: 48px\">"
		     code
		     plane
		     chise-wiki-glyphwiki-glyph-image-url
		     code
		     plane
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward
		  "&\\(A-\\|G-\\|g2-\\|R-\\)?M-\\([0-9]+\\);"
		  nil t)
	    (setq code (string-to-int (match-string 2)))
	    (replace-match
	     (format "<img alt=\"dkw-%05d\" src=\"%s/dkw-%05d.svg\"
style=\"vertical-align:middle; width: 48px; height: 48px\">"
		     code
		     chise-wiki-glyphwiki-glyph-image-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(g2-\\)?U-v\\([0-9]+\\)\\+\\([0-9A-F]+\\);" nil t)
	    (setq plane (string-to-int (match-string 2))
		  code (string-to-int (match-string 3) 16))
	    (replace-match
	     (format "<img alt=\"u%04x-var-%03d\" src=\"%s/u%04x-var-%03d.svg\"
style=\"vertical-align:middle; width: 48px; height: 48px\">"
		     code
		     plane
		     chise-wiki-glyphwiki-glyph-image-url
		     code
		     plane
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(A-\\|G-\\|R-\\|g2-\\)?GT-\\([0-9]+\\);" nil t)
	    (setq code (string-to-int (match-string 2)))
	    (replace-match
	     (format "<img alt=\"GT-%05d\" src=\"%s?char=GT-%05d\"
style=\"%s\">"
		     code
		     chise-wiki-glyph-cgi-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(A-\\|G-\\|g2-\\)?GT-K\\([0-9]+\\);" nil t)
	    (setq code (string-to-int (match-string 2)))
	    (replace-match
	     (format "<img alt=\"GT-K%05d\" src=\"%s?char=GT-K%05d\"
style=\"%s\">"
		     code
		     chise-wiki-glyph-cgi-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&B-\\([0-9A-F]+\\);" nil t)
	    (setq code (string-to-int (match-string 1) 16))
	    (replace-match
	     (format "<img alt=\"B-%04X\" src=\"%s?char=B-%04X\"
style=\"%s\">"
		     code
		     chise-wiki-glyph-cgi-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward
		  "&\\(A-\\|G-\\|g2-\\|R-\\)?CDP-\\([0-9A-F]+\\);" nil t)
	    (setq code (string-to-int (match-string 2) 16))
	    (replace-match
	     (format "<img alt=\"CDP-%04X\" src=\"%s?char=CDP-%04X\"
style=\"%s\">"
		     code
		     chise-wiki-glyph-cgi-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward
		  "&\\(I-\\)?HZK\\(0[1-9]\\|1[0-2]\\)-\\([0-9A-F]+\\);" nil t)
	    (setq plane (match-string 2)
		  code (string-to-int (match-string 3) 16))
	    (replace-match
	     (format "<img alt=\"HZK%s-%04X\" src=\"%s?char=HZK%s-%04X\"
style=\"%s\">"
		     plane
		     code
		     chise-wiki-glyph-cgi-url
		     plane
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(A-\\|G-\\|g2-\\|R-\\)?RUI6-\\([0-9A-F]+\\);" nil t)
	    (setq code (string-to-int (match-string 2) 16))
	    (replace-match
	     (format "<img alt=\"RUI6-%04X\" src=\"%s?char=RUI6-%04X\"
style=\"vertical-align:middle\">"
		     code
		     chise-wiki-glyph-cgi-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&hanaJU\\+\\([0-9A-F]+\\);" nil t)
	    (setq code (string-to-int (match-string 1) 16))
	    (replace-match
	     (format "<img alt=\"hanaJU+%04X\" src=\"%s?char=hana-JU+%04X\"
style=\"vertical-align:middle\">"
		     code
		     chise-wiki-glyph-cgi-url
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&\\(A-\\|G-\\|g2-\\|R-\\)?\\(UU\\+\\|U-\\)\\([0-9A-F]+\\);" nil t)
	    (setq code (string-to-int (match-string 3) 16))
	    (replace-match
	     (format "<img alt=\"UU+%04X\" src=\"https://www.unicode.org/cgi-bin/refglyph?24-%04X\"
style=\"vertical-align:middle\">"
		     code
		     code
		     www-format-char-img-style)
	     t 'literal))

	  (goto-char (point-min))
	  (while (re-search-forward "&MCS-\\([0-9A-F]+\\);" nil t)
	    (setq code (string-to-int (match-string 1) 16))
	    (setq start (match-beginning 0)
		  end (match-end 0))
	    (setq char (decode-char 'system-char-id code))
	    (cond
	     ((and (setq variants
			 (or (www-get-feature-value char '->subsumptive)
			     (www-get-feature-value char '->denotational)))
		   (progn
		     (if (characterp variants)
			 (setq variants (list variants)))
    		     (while (and variants
				 (setq ret (www-format-encode-string
					    (char-to-string (car variants))))
				 (string-match "&MCS-\\([0-9A-F]+\\);" ret))
		       (setq variants (cdr variants)))
		     ret))
	      (unless (string-match "&MCS-\\([0-9A-F]+\\);" ret)
		(goto-char start)
		(delete-region start end)
		(insert ret))
	      )
	     ((setq ret (or (www-get-feature-value char 'ideographic-combination)
			    (www-get-feature-value char 'ideographic-structure)))
	      (setq ret
		    (mapconcat
		     (lambda (ch)
		       (if (listp ch)
			   (if (characterp (setq rret (find-char ch)))
			       (setq ch rret)))
		       (if (characterp ch)
			   (www-format-encode-string
			    (char-to-string ch) without-tags)
			 (www-format-encode-string
			  (format "%S" ch) without-tags)))
		     ret ""))
	      (when ret
		(goto-char start)
		(delete-region start end)
		(insert ret))
	      )))
	  ))
      ;; (goto-char (point-min))
      ;; (while (search-forward "&GT-" nil t)
      ;;   (replace-match "&amp;GT-" t 'literal))
      (buffer-string))))

(setq www-format-char-img-style "vertical-align:middle;")

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

(defconst www-ids-find-version "0.100.2")

(defvar www-ids-find-ideographic-products-file-name
  (expand-file-name "ideographic-products"
		    (expand-file-name
		     "feature"
		     (expand-file-name
		      "character"
		      chise-system-db-directory))))

(defvar www-ids-find-char-viewer-url
  "/est/view/character/")

(defvar www-ids-find-chise-link-map-url-prefix
  "http://fonts.jp/chise_linkmap/map.cgi?code=")

(defvar www-ids-find-tang-chars-file-name
  "~tomo/projects/chise/ids/www/tang-chars.udd")

(defun www-ids-find-format-char (c &optional code-desc)
  (let* ((ucs (encode-char c '=ucs)))
    (princ
     (format "<a href=\"%s%s\">%s</a>"
	     www-ids-find-char-viewer-url
	     (www-uri-encode-object c)
	     (if ucs
		 (format "<img alt=\"u%04x\" src=\"%s/u%04x.svg\" style=\"vertical-align:middle; width: 60px; height: 60px\"/>%s"
			 ucs
			 chise-wiki-glyphwiki-glyph-image-url
			 ucs
			 (if code-desc
			     (encode-coding-string (format " (%c)" c) 'utf-8-mcs-er)
			   ""))
	       (www-format-encode-string (char-to-string c)))))
    ))

(defun www-ids-find-format-ids (ids &optional code-desc)
  (let (len i ucs ret)
    (setq i 0
	  len (length ids))
    (while (< i len)
      (www-ids-find-format-char (aref ids i))
      (setq i (1+ i)))
    (when code-desc
      (princ
       (format " (%s)"
	       (mapconcat
		(lambda (c)
		  (setq ucs (or (char-ucs c)
				(encode-char c '=>ucs@iso)
				(encode-char c '=>ucs@unicode)
				(encode-char c '=>ucs@iwds-1)
				(encode-char c '=>ucs@iwds-1/normalized)
				(encode-char c '=>ucs@component)
				(encode-char c '=>ucs@cognate)))
		  (cond (ucs
			 (encode-coding-string
			  (char-to-string (decode-char '=ucs ucs))
			  'utf-8-mcs-er)
			 )
			(t
			 (setq ret (encode-coding-string 
				    (char-to-string c) 'utf-8-mcs-er))
			 (if (eq (aref ret 0) ?&)
			     (concat "&amp;" (substring ret 1)))
			 )))
		ids ""))))))

(defun www-ids-find-format-line (c is)
  (let (ucs ids)
    (princ "<span class=\"entry\">")
    (www-ids-find-format-char c 'code-desc)
    (princ "</span>")
    (princ
     (or (if (setq ucs (or (char-ucs c)
			   (encode-char c 'ucs)))
	     (format
	      " <a href=\"http://www.unicode.org/cgi-bin/GetUnihanData.pl?codepoint=%X\">%s</a>"
	      ucs
	      (cond ((<= ucs #xFFFF)
		     (format "U+%04X" ucs))
		    ((<= ucs #x10FFFF)
		     (format "U-%08X" ucs))))
	   "          ")))
    (when ucs
      (princ
       (format " <a href=\"%s%X\">(link map)</a>"
	       www-ids-find-chise-link-map-url-prefix ucs)))
    (princ " ")
    ;; (www-ids-find-format-ideographic-structure is 'code-desc)
    (when is
      (setq ids (ideographic-structure-to-ids is))
      ;; (setq i 0
      ;;       len (length ids))
      (princ "<span class=\"ids\">")      
      (www-ids-find-format-ids ids 'code-desc)
      ;; (while (< i len)
      ;;   (www-ids-find-format-char (aref ids i))
      ;;   (setq i (1+ i)))
      (princ "</span>"))
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

(defun www-ids-insert-chars-including-components* (components
						   &optional ignored-chars products)
  (unless products
    (setq products (ideograph-find-products components ignored-chars)))
  (let (is as bs len)
    (setq len (length products))
    (princ "<ul>\n")
    (dolist (c (cond
		((>= len 1024)
		 (sort (copy-list products)
		       (lambda (a b)
			 (< (char-int a)(char-int b))))
		 )
		((>= len 512)
		 (sort (copy-list products)
		       (lambda (a b)
			 (if (setq as (char-total-strokes a))
			     (if (setq bs (char-total-strokes b))
				 (if (= as bs)
				     (< (char-int a)(char-int b))
				   (< as bs))
			       t)
			   (< (char-int a)(char-int b)))))
		 )
		(t
		 (sort (copy-list products)
		       (lambda (a b)
			 (if (setq as (char-total-strokes a))
			     (if (setq bs (char-total-strokes b))
				 (if (= as bs)
				     (ideograph-char< a b)
				   (< as bs))
			       t)
			   (ideograph-char< a b))))
		 )))
      (unless (memq c ignored-chars)
	(setq is (char-feature c 'ideographic-structure))
	(princ "<li>")
	(www-ids-find-format-line c is)
	(setq ignored-chars
	      (www-ids-insert-chars-including-components*
	       (char-to-string c) (cons c ignored-chars)))
	)
      )
    (princ "</ul>\n")
    )
  ignored-chars)

(defun www-ids-insert-chars-including-components (components
						  &optional ignored-chars)
  (let ((products (ideograph-find-products components ignored-chars))
	is as bs len ignore-children)
    (setq len (length products))
    (when (>= len 1024)
      (setq ignore-children t)
      (princ
       (encode-coding-string
	"<p>結果が多すぎるため、再帰的検索を省略しました。</p>"
	'utf-8-jp-er)))
    (if (>= len 2048)
	(dolist (c products)
	  (www-ids-find-format-char c))
      (setq ignored-chars
	    (nreverse
	     (www-ids-insert-chars-including-components* components ignored-chars products)))
      (dolist (c ignored-chars)
	(dolist (vc (char-component-variants c))
	  (unless (memq vc ignored-chars)
	    (when (setq is (get-char-attribute vc 'ideographic-structure))
	      (princ "<li>")
	      (www-ids-find-format-line vc is)
	      (setq ignored-chars
		    (www-ids-insert-chars-including-components*
		     (char-to-string vc)
		     (cons vc ignored-chars)))))))
      (setq products (ideograph-find-products-with-variants components ignored-chars))
      (setq len (length products))
      (when (>= len 512)
	(setq ignore-children t)
	(princ
	 (encode-coding-string
	  "<p>結果が多すぎるため、関連字の再帰的検索を省略しました。</p>"
	  'utf-8-jp-er)))
      (if (>= len 1024)
	  (dolist (c products)
	    (www-ids-find-format-char c))
	(dolist (c (sort (copy-tree products)
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
	    (princ "<li>")
	    (www-ids-find-format-line c is)
	    (unless ignore-children
	      (setq ignored-chars
		    (www-ids-insert-chars-including-components*
		     (char-to-string c)
		     (cons c ignored-chars))))
	    ))
	))
    )
  ignored-chars)

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
		(decode-url-string components 'utf-8-er)
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
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<title>CHISE IDS Find</title>
<link href=\"/css/bootstrap-4.5.0.min.css\" rel=\"stylesheet\">
<style type=\"text/css\">
<!--
.entry { font-size: 36px; }
.entry a img { height: 36px; }
.ids { font-size: 24px; }
.ids a img { height: 24px; }
img { vertical-align:middle; }
a { text-decoration:none; }
ul { margin: 0 0; }
li { margin: 0 0 -0.2em; }
.tooltip {
    position: relative;
    display: inline-block;
}
.tooltip .tooltiptext {
    display: none;
}
-->
</style>
</head>

<body>

<div class=\"jumbotron jumbotron-fluid mb-0\">
<h1 class=\"display-4 text-center\">")
    (princ (encode-coding-string "CHISE IDS 漢字検索" 'utf-8-jp-er))
    (princ "</h1>")
    (princ "
<p class=\"text-center\">Version ")
    (princ www-ids-find-version)
    (princ (format-time-string
	    " (Last-modified: %Y-%m-%d %H:%M:%S)</p>"
	    (nth 5
		 (file-attributes
		  www-ids-find-ideographic-products-file-name))))
    (princ "
</div>
<div class=\"container mt-0 mw-100 d-inline-block align-top bg-dark\">
<p />
<div class=\"input-group mb-3 h3 my-4\">
<div class=\"input-group-prepend mw-75 ml-3\">
<form action=\"/ids-find\" method=\"GET\">
<span class=\"input-group-text\" id=\"basic-addon1\">
")
    (princ (encode-coding-string "部品文字列" 'utf-8-jp-er))
    (princ "</span>
</div>
<input type=\"text\" class=\"form-control\" aria-describedby=\"basic-addon1\" name=\"components\" size=\"30\" maxlength=\"256\" value=\"")
    (if (> (length components) 0)
	(princ (encode-coding-string components 'utf-8-er)))
    (princ "\">
<input class=\"mr-3\" type=\"submit\" value=\"")
    (princ (encode-coding-string "検索開始" 'utf-8-jp-er))
    (princ "\">
</form>
</div>
</div>

")
    (unless (file-newer-than-file-p
	     www-ids-find-ideographic-products-file-name
	     (locate-file (car command-line-args) exec-path))
      (princ (encode-coding-string "<hr>
<p>
現在、システムの更新作業中です。しばらくお待ちください。
<hr>
" 'utf-8-jp-er))
      ;; (setq components nil)
      )
    (cond
     (components
      (princ "<div class=\"container\">
")
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
      ;; (princ "<ul>\n")
      (www-ids-insert-chars-including-components components)
      ;; (princ "</ul>\n")
      (princ "</div>\n")
      )
     (t
      (princ (encode-coding-string "<div class=\"container mt-4\">
<div class=\"ml-3\">
<p>
指定した部品を全て含む漢字の一覧を表示します。
</p>
<p>
CHISE で用いられる実態参照形式（例：&amp;M-00256;）で部品を指定する事もできます。
</p>
</div>
" 'utf-8-jp-er))
      (princ (encode-coding-string "
<p  class=\"ml-0\">
\[Links\]
<ul>
<li><a href=\"https://www.chise.org/ipns/ids-find.chise.org/index.ja.html\"
>IPFS 版 CHISE IDS 漢字検索</a></li>
</ul>
<ul>
<li><a href=\"http://www.shuiren.org/chuden/toyoshi/syoseki/chise_ids.html\"
>「CHISE IDS FINDで漢字を検索」</a> ― 山田崇仁さん（<a
href=\"http://www.shuiren.org/\">睡人亭</a>）による解説
</ul>
<ul>
<li><a href=\"http://www.karitsu.org/tools/firefox_plugin.htm\"
>Firefox 用 plugin</a> by 秋山陽一郎さん（<a href=\"http://www.karitsu.org/\"
>過立齋</a>）
</ul>
<ul>
<li><a href=\"http://git.chise.org/gitweb/?p=chise/ids.git;a=blob;f=www/www-ids-find.el\"
>www-ids-find.el (source file (Emacs Lisp part))
<li><a href=\"http://www.chise.org/ids/\"
>「CHISE 漢字構造情報データベース」</a>
<li><a href=\"http://fonts.jp/chise_linkmap/\"
>「chise_linkmap : CHISE 漢字連環図」</a> by 上地宏一さん
<li><a href=\"http://www.chise.org/\"
>CHISE Project</a>
</ul>
<ul>
<li><a href=\"http://coe21.zinbun.kyoto-u.ac.jp/djvuchar\"
>「拓本文字データベース」</a> by
<a href=\"http://coe21.zinbun.kyoto-u.ac.jp/\"
>京都大学21世紀COE「東アジア世界の人文情報学研究教育拠点」</a>
<li><a href=\"http://www.unicode.org/\"
>Unicode</a>
</ul>
</p>
</div>
"
 'utf-8-jp-er))

      ))
    (princ "<hr>
<div class=\"container\">
")
    (princ "<div class=\"ml-0\">
Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2015, 2016, 2017, 2020, 2021, 2022, 2023 <a href=\"http://kanji.zinbun.kyoto-u.ac.jp/~tomo/\"
>MORIOKA Tomohiko</a></div>")
    (princ
     (format
      "<div>Powered by <a
href=\"http://www.chise.org/xemacs/\"
>XEmacs CHISE</a> %s.</div>"
      (encode-coding-string xemacs-chise-version 'utf-8-jp-er)))
    (princ "
</div>
</body>
</html>
")))
