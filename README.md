<!-- -*- coding: utf-8-jis-er -*- -->

This directory holds the CHISE-IDS package which contains data and
utilities about structures of Han Ideographs (漢字).


# How to install

Please install [CL-CHISE](https://gitlab.chise.org/CHISE/cl-chise).

Note that if you install CL-CHISE in SBCL, CL-CHISE installs this
package automatically.


You can also use this package with XEmacs CHISE.
If XEmacs CHISE is installed in your system, please type

    % make install

in the directory of the CHISE-IDS distribution.

[Note] If you don't have XEmacs CHISE, the CHISE-base package may be
useful.  It is an installer package to install CHISE functionalities
including libchise, XEmacs CHISE, the CHISE-IDS package, some other
Emacs Lisp utilities, some fonts, etc.  It is available at:

	http://www.chise.org/dist/base/


# IDS files

The following files contains the data about structures of Han
Ideographs (漢字).

## IDS files for abstract characters

    IDS-UCS-Basic.txt	CJK Unified Ideographs (U+4E00 〜 U+9FA5)
			of ISO/IEC 10646-1:2000
    IDS-UCS-Ext-A.txt	CJK Unified Ideographs Extension A
			(U+3400 〜 U+4DB5, U+FA1F and U+FA23)
			of ISO/IEC 10646-1:2000
    IDS-UCS-Ext-B-1.txt	CJK Unified Ideographs Extension B [part 1]
			(U-00020000 〜 U-00021FFF)
			of ISO/IEC 10646-2:2001
    IDS-UCS-Ext-B-2.txt	CJK Unified Ideographs Extension B [part 2]
			(U-00022000 〜 U-00023FFF)
			of ISO/IEC 10646-2:2001
    IDS-UCS-Ext-B-3.txt	CJK Unified Ideographs Extension B [part 3]
			(U-00024000 〜 U-00025FFF)
			of ISO/IEC 10646-2:2001
    IDS-UCS-Ext-B-4.txt	CJK Unified Ideographs Extension B [part 4]
			(U-00026000 〜 U-00027FFF)
			of ISO/IEC 10646-2:2001
    IDS-UCS-Ext-B-5.txt	CJK Unified Ideographs Extension B [part 5]
			(U-00028000 〜 U-00029FFF)
			of ISO/IEC 10646-2:2001
    IDS-UCS-Ext-B-6.txt	CJK Unified Ideographs Extension B [part 6]
			(U-0002A000 〜 U-0002A6D6)
			of ISO/IEC 10646-2:2001
    IDS-UCS-Ext-C.txt	CJK Unified Ideographs Extension C
			(U-0002A700 〜 U-0002B738)
    IDS-UCS-Ext-D.txt	CJK Unified Ideographs Extension D
			(U-0002B740 〜 U-0002B81D)
    IDS-UCS-Ext-E.txt	CJK Unified Ideographs Extension E
			(U-0002B820 〜 U-0002CEA1)
    IDS-UCS-Ext-F.txt	CJK Unified Ideographs Extension F
			(U-0002CEB0 〜 U-0002EBE0)
    IDS-UCS-Ext-G.txt	CJK Unified Ideographs Extension G
			(U-00030000 〜 U-0003134A)
    IDS-UCS-Ext-H.txt	CJK Unified Ideographs Extension H
			(U-00031350 〜 U-000323AF)
    IDS-UCS-Ext-I.txt	CJK Unified Ideographs Extension I
			(U-0002EBF0 〜 U-0002EE5D)

In these files, each line represents an abstract character, and each
component used in IDS should be abstract characters.

If a component is unified two or more CJK Unified Ideographs by UCV,
one CJK Unified Ideograph represents the abstract component
(semantically it indicates the abstract component unified by UCV, not
the abstract character itself).


## IDS files for glyphs

    IDS-UCS-Compat.txt	CJK Compatibility Ideographs
			(U+F900 〜 U+FA2D, except U+FA1F and U+FA23)
			of ISO/IEC 10646-1:2000

    IDS-UCS-Compat-Supplement.txt
			CJK Compatibility Ideographs Supplement
			(U-0002F800 〜 U-0002FA1D)
			of ISO/IEC 10646-2:2001

    IDS-JIS-X0208-1990.txt

    IDS-CNS-1.txt

    IDS-CNS-2.txt

    IDS-CNS-3.txt

    IDS-Daikanwa-01.txt

    IDS-Daikanwa-02.txt

    IDS-Daikanwa-03.txt

    IDS-Daikanwa-04.txt

    IDS-Daikanwa-05.txt

    IDS-Daikanwa-06.txt

    IDS-Daikanwa-07.txt

    IDS-Daikanwa-08.txt

    IDS-Daikanwa-09.txt

    IDS-Daikanwa-10.txt

    IDS-Daikanwa-11.txt

    IDS-Daikanwa-12.txt

    IDS-Daikanwa-dx.txt

    IDS-Daikanwa-ho.txt

    IDS-CDP.txt

    IDS-CBETA.txt

In these files, each line represents a glyph unified by the
corresponding CJK Unified Ideograph, not an abstract character.  So
each component should be a glyph.  If a CJK Unified Ideograph
indicates the unique and concrete glyph, it is used as a glyph
component.  But a CJK Unified Ideograph unifies two or more glyphs,
one glyph object of the CHISE character ontology is used.  In that
case, glyphs are represented by entity-reference format.  Note that
even if a component can be encoded by a CJK Unified Ideograph, it may
be encoded as an entity-reference to indicate the corresponding glyph
of the CHISE character ontology.  In that case, it is **not** bug!
Don't send Pull/Merge-Request or issues to convert an entity-reference
to the corresponding CJK Unified Ideographs!


## Format

These files are encoded by UTF-8.  The format of each line of the
files is:

    <CODEPOINT><TAB><CHARACTER><TAB><IDS>(<TAB>@apparent=<IDS>)

or

    ;; <COMMENTS>

Each element means

    <TAB>	<HORIZONTAL TABULATION> (U+0009)
    <CODEPOINT>	code point
		U+hhhh		Hex form of UCS code point
				(U+0000 〜 U+FFFF)
		U-hhhhhhhh	Hex form of UCS code point
				(U+00000000 〜 U+7FFFFFFF)
    <CHARACTER>	character corresponding with <CODEPOINT>
    <IDS>	Ideographic Description Sequence
		(based on ISO/IEC 10646-1:2000 F.3.1; however
		Compatibility Ideographs and non-UCS Ideographs are
		also allowed)
    <COMMENTS>	comment

`(<TAB>@apparent=<IDS>)` is an optional field.  `@apparent=<IDS>` means
that the `<IDS>` represents an apparent structure.  Not that the `<IDS>`
stored in the 3rd field may be regarded as functional structure.

In many cases, apparent structures are automatically converted from
functional structures by the following set of conversion rules:

- 111: ⿸⿰LRB -> ⿰L⿱RB
	e.g. 㫅:	⿸㫃亓 <-> ⿰方⿱&A-IWDSU+4EBA;亓

- 112: ⿸⿲亻丨RB -> ⿰&CDP-8B7A;⿱RB

- 121: ⿸⿱ATR -> ⿱A⿸TR if T is enc
	(T = 𠂇, 厂, 丆, 𠁣, &CDP-88E2;, &CDP-88AD;, <&A-CDP-8766;/&CDP-i001-8766;>, 尸, &U-i001+2E282;, ⿸XY, ...)
	e.g. 亷:	⿸产&CDP-8CDE; <-> ⿱&A-compU+6587;⿸厂&CDP-8CDE;

- 122: ⿸⿱ADR -> ⿱A⿰DR if D is non-enc
	e.g. 㝥:	⿸𪧇米 <-> ⿱宀⿰&A-IWDSU+723F;米

- 131: ⿸⿸ELR -> ⿸E⿰LR if L = 彳
	e.g. 屜:	⿸&U-i002+5C65;世 <-> ⿸尸⿰彳世

- 132: ⿸⿸EAB -> ⿸E⿱AB	if A = non 彳
	e.g. 㞙:	⿸尾水 <-> ⿸尸⿱毛水

- 141: ⿸⿹⿱AMLR -> ⿱A⿲LMR
	e.g. 𭐱:	⿱⿸⿹首止&A-compU+5DF1;&A-compU+5902; <-> ⿱⿱䒑⿲止自&A-compU+5DF1;&A-compU+5902;

- 210: ⿹⿰LRB -> ⿰⿱LBR
	e.g. 㕡:	⿹𣦻&A-IWDSU+8C37; <-> ⿰⿱𣦵&A-IWDSU+8C37;又

- 220: ⿹⿱ARL -> ⿱A⿰LR
	e.g. 命:	⿹⿱&IWDS1-064;卩口 -> ⿱&IWDS1-064;⿰口卩

- 310: ⿽⿺LRA -> ⿺⿱ALR
	e.g. 㽔:	⿽甤&CDP-85F0; <-> ⿺⿱&CDP-85F0;豕生

- 320: ⿽⿱ABL -> ⿱⿰LAB
	e.g. 㭍:	⿽𣎽&A-IWDSU+4E8C; <-> ⿱⿰&A-IWDSU+4E8C;&A-IWDSU+4E03;木

- 330: ⿽⿰LRA -> ⿰⿱ALR
	e.g. 𭤁:	⿽𰕆白 -> ⿰⿱白方攴

- 411: ⿴⿱AEM -> ⿱A⿴EM	if E=囗/彑/西/<&A-CDP-87A5;>
	e.g. 卤:	⿴占㐅 <-> ⿱⺊⿴囗㐅


- 412: ⿴⿱A凵M -> ⿱A⿶凵M

- 413: ⿴⿱<𰃦>BM -> ⿱⿵<𰃦>MB

- 414: ⿴⿱ABM -> ⿱A⿱MB

- 415: ⿴⿳AEBC -> ⿱⿱A⿴ECB if E=囗 (⿳AEB=&U-i002+675F;)

- 416: ⿴⿳AHBC -> ⿱⿱⿱AHCB if H=冖

- 417: ⿴⿳A𰀪BC -> ⿱A⿸⿱C𰀪B

- 421:  ⿴⿰AAC -> ⿲ACA

- 422:  ⿴⿰LRC -> ⿰L⿰CR

- 511: ⿶⿱⿰LRBA -> ⿱⿲LARB
	e.g. 䰞:	⿶𩰲者 <-> ⿱⿲弓者弓鬲

- 512: ⿶⿳⿰LRMBA -> ⿳⿲LARMB

- 530: ⿶⿴⿰LRCA -> ⿲L⿱ACR

- 601: ⿵⿱AKC -> ⿱A⿵KC	if K = <character surround from above>
	e.g. 𧖔:	⿵𨳌蟲 <-> ⿱𠄟⿵門蟲

- 611: ⿵⿱A⿰LRC -> ⿱A⿲LCR
	e.g. 嬴:	⿵&A-compU+23386;女 <-> ⿱吂⿲&A-compU+6708;女&A-IWDSU+51E1;

- 612: ⿵⿳AM⿰LRC -> ⿳AM⿲LCR

- 613: ⿵⿱A⿱M⿰LRC -> ⿳AM⿲LCR
	e.g. 𡫙: ⿵⿱宀⿱⿰工工⿰又又仌 <-> ⿳宀⿰工工⿲又仌又

- 614: ⿵⿱A⿲LMRC -> ⿱A⿲L⿱MCR
	e.g. &U-i002+8D0F;(𬖊):	⿵⿱𠫓⿲月口凡貝 <-> ⿱𠫓⿲月員凡

- 620: ⿵⿲AMRC -> ⿲A⿱MCR

- 640: ⿵⿵EML -> ⿵E⿱ML
	e.g. 開:	⿵閂廾 -> ⿵門开

- 710: ⿷⿺NAC -> ⿺N⿱AC

- 720: ⿷⿸TLC - > ⿸T⿺LC
	e.g. 𣅦:	⿷仄日 -> ⿸厂⿺人日

- 730: ⿷⿱⿱AMBC -> ⿳A⿰MCB
	e.g. 寶:	⿷𡪓&A-IWDSU+7F36; -> ⿳宀珤貝

- 811: ⿺⿱𠃊BC -> ⿱⿺𠃊CB

- 812: ⿺⿱木B丶 -> ⿱⿺木丶B

- 813: ⿺⿱LBC -> ⿱⿰LCB if B is 灬/日/曰/心/𡈼/壬/王/&A-compU+7389;/一/𱥯/大/犬 (B is not 豕/八/儿/龰/乂/又/&A-compU+590A;/⿺儿厶/夂/&CDP-8D46;/艮/&J97-5238;/巴/兀/&A-compU+51E0;/&CDP-8BCB;/元/兄/𫶧/电/电/⿰亻&CDP-89F3;/&A-IWDSU+4E03;/光/鬼/攴/𤴓/毛/龰/𠂡/&A-CDP-8661;/黽/乙/&CDP-85C0;/竜/&U-i004+592E;/風/更/𠃔/⿻尢丶/兒/𫀄/光/免/免)


- 1011: &U-i002+2FF1;⿰LR⿱AB -> ⿱⿲LARB

- 1141: &U-i001+2FF1;⿱AB⿰LR -> ⿱A⿲LBR
	e.g. 𭐱:	⿱&U-i001+2FF1;首⿰止&A-compU+5DF1;&A-compU+5902; <-> ⿱⿱䒑⿲止自&A-compU+5DF1;&A-compU+5902;



## Extended IDC

### Non-abstract IDC

&U-i001+2FF1; <img src="https://glyphwiki.org/glyph/u2ff1-itaiji-001.50px.png" alt="U+2FF1-itaiji-001" title="U+2FF1-itaiji-001">x⿰yz = ⿸⿹xyz

&U-i001+2FFB; <img src="https://glyphwiki.org/glyph/u2ffb-itaiji-001.50px.png" alt="U+2FFB-itaiji-001" title="U+2FFB-itaiji-001">x⿰yz = ⿷⿼xyz

&U-i002+2FF1; <img src="https://glyphwiki.org/glyph/u2ff1-itaiji-002.50px.png" alt="U+2FF1-itaiji-002" title="U+2FF1-itaiji-002">x⿰yz = ⿺⿽xyz


# License

This package is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This package is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this package; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.


# Acknowledgment

The developing of the package was supported by the “Exploratory
Software Project” of Information-technology Promotion Agency, Japan.
Some data in the IDS-UCS* files are derived and expanded from the CDP
database developped by C.C. Hsieh and his team at Academia Sinica in
Taipei, Taiwan.
