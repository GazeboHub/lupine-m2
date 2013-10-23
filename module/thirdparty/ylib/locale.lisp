;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10;  -*-
;;; Ystok Library - Internationalization/localization (i18n/l10n) and external formats
;;; Copyright (c) 2003-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ystok.library)

;; The character element-type we rely on in our code when no info available in advance.
;; Rationaly:
;;  Sometimes we get source modules which
;;  - supplied by developers that are unreachable or too independent;
;;  - we don't want to edit but use them "as is";
;;  - only works properly when lw:*default-character-element-type* is set to base-char.
;; OT1H, lw:*default-character-element-type* governs standard functionality.
;; OTOH, yl:*default-char-type* governs what is under our control.
;;
;; Used by: ystok.sql.sys::with-sql-string-stream
(defparameter *default-char-type* #+lispworks 'lw:simple-char #-lispworks 'base-char)

(defun ef-type (external-format)
 ;;; Lisp impelementaion-specific type corresponding to the extertnal-format designator
  #+lispworks
  (if (and external-format (neq external-format :default))
      (ef:external-format-type external-format)
      *default-char-type*)
  #-lispworks
  (decalre (ignore external-format))
  #-lispworks
  *default-char-type*)
 
(defvar *internal-format*)	; default converting arguments and/or values for OS APIs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  DEFNATIVE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Complile- and load-time translation of text.
;;; Additinally coalecse strings, either original or localized.

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defvar *localization-hash-table* (make-hash-table :test #'equal))
 (defparameter *warn-on-missing-localization* nil)

 (defmacro defnative (some &optional (native *unbound-value*) locale)
  "Register translation of SOME object into NATIVE object.
 Args: native - can even be NIL."
  (declare (ignore locale))			; use :self or the like as a default?
   ;; Do not put the entry if the translation is empty (for the language desired)
   (if (eq native *unbound-value*)
       (values)
       `(%ensure-native ,some ,native)))

 (defun %ensure-native (some native)
   ;(unless native (setq native some))
   (multiple-value-bind (old present-p) (gethash some *localization-hash-table*)
     (and present-p (not (equalp old native))
          (warn "Redefining localization of ~S from ~S to ~S." some old native)))
   (setf (gethash some *localization-hash-table*) native))

 (defun %read-localized (stream char number)
  (declare (ignore char number))
  (let ((some (read stream nil nil t)))
    (multiple-value-bind (native present-p) (gethash some *localization-hash-table*)
      (if present-p
          native
          (progn (when *warn-on-missing-localization*
                   (warn "No localization found for ~S." some))
            some)))))

 (set-dispatch-macro-character #\# #\L '%read-localized)
) ; eval-when

#+lispworks
(lw:define-action "Delivery Actions" "Shake localization"
                  (lambda () (setf *localization-hash-table* nil)))
                  ;(maphash (lambda (x y) (remhash x yl::*localization-hash-table*)))


;;; RFC3066 (obsoletes RFC1766) - Tags for the Identification of Languages
;;; The language tag is composed of 1 or more parts:
;;; a primary language subtag and a (possibly empty) series of subtags.
;;;	Language-Tag ::= Primary-subtag *( "-" Subtag )
;;;	Primary-subtag ::= 1*8ALPHA
;;;	Subtag ::= 1*8(ALPHA / DIGIT)
;;;
;;; Primary language subtag:
;;; - all 2-letter tags are interpreted according to
;;;   ISO 639, "Code for the representation of names of languages"
;;; Second subtag:
;;; - all 2-letter codes are interpreted as ISO 3166 alpha-2
;;;   country codes denoting the area in which the language is used.
;;; ISO 639/ISO 3166 recommendation (not enforced as the tags are case insensitive)
;;;   Language names are in lower case.
;;;   Country codes are in upper case.
;;; Recommendation:
;;;   Use the shortest (only primary) tag if possible
;;;
;;; Arg: lang is a language tag keyword, either primary or with subtags.

;; Association: language-tag -> primary-subtag
(defparameter *language-subtag-map*
  '((:en-US . :en)	; United States
    (:en-AU . :en)	; Australia
    (:en-CA . :en)	; Canada
    (:en-GB . :en)))	; Great Britain

(declaim (inline language-primary-subtag))
(defun language-primary-subtag (lang)
  (cdr-assoq lang *language-subtag-map*))

(defun language-primary-list (lang)
  (if-bind (language-primary-subtag (language-primary-subtag lang))
    (list lang language-primary-subtag)
    (list lang)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  LABELS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run-time translation of text: label -> string
;;; A label is one of the two kinds:
;;; - a string, so we use hash-table similar to cl-l18n,
;;; - a symbol (keyword) or a list, aka path.
;;;
;;; Labels files are
;;;  - placed into lang/ subdirectory of the product-pathname directory,
;;;  - named {bundle-name}-{language}.lab,
;;;  - loaded by calling the read function in loop.
;;;
;;; Label forest is map: language -> (external-format timestamp . tree),
;;; where tree is a kind of (binary tree), one tree per master language.
;;; ((:en :ascii nil
;;;   (:navigation (:toggle (:title . "..."))
;;;                (:sync   (:title . "...")))
;;;   (:prev . "...")
;;;   (:up   . "...")
;;;   (:next . "...")
;;;   ...))

;; Default language as a language tag keyword
(defvar *language* :en)

;; Bundle is a piece of software, ys::module inherits from it
;; The languages slot is the list of tuples:
;;   ((lang external-format timestamp . tree) ...)
;; where timestamp is one of:
;;   NIL = the labes file has not been loaded yet,
;;   file-write-date = the file modification time of the file loaded.
(defstruct (bundle (:copier nil))
  name				  	; used as file name prefix, defaults to "labels"
  (languages '((:en :ascii nil))) 	; list of tuples
  string-map)			  	; hash: string -> ((lang . string) ...)

(defmacro define-label-language (bundle lang &optional (external-format :ascii))
  `(setf (cdr-assoc ,lang (bundle-languages ,bundle)) (list ,external-format nil)))

;; If true, we check the file modified time in every gettext call
;(defvar *check-labels-file-update* nil)

(defun labels-pathname (bundle lang &optional (type "lab"))
 ;;; Args: bunlde Bundle instance or string designator (e.g. :stopwords).
  ;;       lang   Language tag keyword, primary or with subtags.
  ;; Value: Pathname with the name composed of the bundle name and language lowercased.
  (make-pathname :name (format nil "~(~a-~a~)"
                               (if (bundle-p bundle)
                                   (or (bundle-name bundle) "labels")
                                   bundle)
                               lang)
                 :type type 
                 :defaults (product-pathname "lang/")))

(defun unload-labels (bundle lang &optional (tuple (assoq lang (bundle-languages bundle))))
  (when tuple
    (rplacd (cddr tuple) ())				; cut the old tree
    (when (third tuple)					; clear string-amp
      (when-let (hash-table (bundle-string-map bundle))
        (loop for path being each hash-key in hash-table using (hash-value alist)
              do (setf (gethash path hash-table) (delete lang alist :key #'first))))
      (setf (third tuple) nil))))

(defun load-labels-file (bundle lang &key (pathname (labels-pathname bundle lang))
                                          external-format)
 ;;; Value: integer file-write-date = success
  (logg :info "Loading labels file ~a" pathname)
  (let ((*read-eval* nil)
        (*read-suppress* nil)
        (tuple (assoq lang (bundle-languages bundle)))
        (hash-table (bundle-string-map bundle))
        line tree)
    (unless external-format
      (setq external-format (second tuple)))
    (with-open-file (stream pathname :direction :input
                            :external-format (or external-format :default)
                            :element-type #+lispworks
                                          (if (and external-format
                                                   (neq external-format :default))
                                              (ef:external-format-type external-format)
                                              *default-char-type*)
                                          #-lispworks
                                          *default-char-type*)
      ;; To indicate that language has been loaded, we put the tree into the forest 
      ;; immediately but defer creation the string-map hash table.
      (if tuple
          (unload-labels bundle lang tuple)
          (push (setq tuple (list* lang external-format (setq tree (cons nil ()))))
                (bundle-languages bundle)))
      (setq tree (cddr tuple))
      (labels ((%put (self tree path value)
                 ;; Args: tree Cons whose cdr part is modified
                 #+debug (assert (consp tree))
                 (let ((alist (cdr tree))
                       (first (first-or-self path))
                       (rest (if (consp path) (rest path) nil)))
                   (if (atom alist)
                       (cond (path
                              (when (and self alist)	; replace atom with ((nil . atom))
                                (rplacd tree (setq alist (acons nil alist nil))))
                              (let ((tuple (list first)))
                                (rplacd tree (cons tuple alist))
                                (%put first tuple rest value)))
                             (alist
                              #1=(warn "In file ~a, duplicated label:~%~s"
                                       pathname line))
                             (self
                              (rplacd tree value)))
                       ;; (consp alist)
                       (let ((tuple (assoq first alist)))
                         (unless tuple
                           (rplacd tree (cons (setq tuple (list first)) alist)))
                         (%put first tuple rest value))))))
        (while (setq line (read stream nil nil))
          (let ((path (car line))
                (value (cdr line)))
            (cond ((stringp path)	; a la (add-resource bundle path value lang)
                   (unless hash-table
                     (setf hash-table (make-hash-table :test #'equal)
                           (bundle-string-map bundle) hash-table))
                   (if-bind (pair (assoq lang (gethash path hash-table)))
                     (progn #1# (rplacd pair value))
                     (push (cons lang value) (gethash path hash-table))))
                  ((or (symbolp path) (consp path)) 
                   (%put nil tree path value))
                  (t (cerror "Skip the line"
                             "In file ~a, invalid label:~%~s"
                             pathname line)))))))
    (setf (third tuple) (file-write-date pathname))))

(defun gettext (path bundle &optional (lang *language*))
 ;;; Args: lang Language tag
  ;; Value: String or NIL
  ;; Usage:
  ;;	(defmacro _ (text)
  ;;	  `(yl:gettext ,text (load-time-value yl:*current-bundle*)))
  (flet ((%not-found (path)
           ;; If there is no match, capitalize sequence starting from an unknown key
           (cond ((stringp path) path)
                 ((consp path) (format nil "~{~:(~A~)~^ ~}" path))
                 (t (format nil "~:(~A~)" path))))
         (%getl (lang)
           ;; Values: 1) result or NIL 2) Arg for %not-found, if null, use path
           (when-let (tuple (assoq lang (bundle-languages bundle)))
             (let ((file-write-date (third tuple)))
               (when (or file-write-date			; already loaded
                         ;(and (not (zerop file-write-date))	; not forced reload
                         ;     (or (not *check-labels-file-update*)
                         ;         (<= (file-write-date (labels-pathname bundle lang))
                         ;             file-write-date)))
                         (load-labels-file bundle lang :external-format (second tuple)))
                 (if (stringp path)
                     (when-let* ((hash-table (bundle-string-map bundle))
                                 (alist (gethash path hash-table)))
                       (cdr-assoq lang alist))
                     (when-let (tree (cddr tuple))		; (timestamp . alist)
                       (labels ((%get (self tree path)
                                  #+debug (assert (consp tree))
                                  (let ((alist (cdr tree)))
                                    (if (atom alist)
                                        (cond (path nil)
                                              (alist)		; success!
                                              (self (values nil self)))
                                        (let* ((first (first-or-self path))
                                               (rest (if (consp path) (rest path) nil))
                                               (tuple (assoq first alist)))
                                          (cond (tuple (%get first tuple rest))
                                                (rest nil)
                                                (first (values nil first))
                                                (self (values nil self))))))))
                         (%get nil tree path)))))))) )
    ;; We dive only one level into the language hierarchy
    (multiple-value-bind (result s) (%getl lang)
      (or result
          (when-let (language-primary-subtag (language-primary-subtag lang))
            (multiple-value-bind (result p) (%getl language-primary-subtag)
              (or result
                  (%not-found (or s p path)))))
          (%not-found (or s path))))))

(defvar *current-bundle*)

(defmacro in-bundle (arg)
  `(setq *current-bundle* ,arg))			; specify manually

(set-dispatch-macro-character #\# #\T
  (lambda (stream char arg)
    (declare (ignore char arg))
    `(gettext ,(read stream) (load-time-value *current-bundle*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  LOCALE-  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Character and string case conversion
;;; Win32 API uses the language driver for the current language selected
;;; by the user at setup or by using Control Panel.
;;; TODO: Implement on not-Windows platforms.

(defun locale-char-downcase (char)
 #+(and win32 lispworks) (code-char (|CharLower| (char-code char)))
 #-(and win32 lispworks) (char-downcase char))

(defun locale-char-upcase (char)
 #+(and win32 lispworks) (code-char (|CharUpper| (char-code char)))
 #-(and win32 lispworks) (char-upcase char))

;;; Args: arg - string (simple or not), character, or symbol (including NIL).
(defun locale-string-downcase (arg &key (start 0) end)
  #+(and win32 lispworks)
  (let ((string (string arg)))
    (if (lw:base-string-p string)
        (string-upcase string :start start :end end)
        (%wstring-upper-down-case string start end nil)))
  #-(and win32 lispworks)
  (string-downcase arg :start start :end end))

(defun locale-string-upcase (arg &key (start 0) end)
  #+(and win32 lispworks)
  (let ((string (string arg)))
    (if (lw:base-string-p string)
        (string-upcase string :start start :end end)
        (%wstring-upper-down-case string start end t)))
  #-(and win32 lispworks)
  (string-upcase arg :start start :end end))

(defun locale-string-capitalize (arg &key (start 0) end)
  #+(and win32 lispworks)
  (let ((string (string arg)))
    (if (lw:base-string-p string)
        (string-capitalize string :start start :end end)
        (progn (setq string (%wstring-upper-down-case string start end nil))
          (when (plusp (length string))
            (setf (schar string 0) (locale-char-upcase (schar string 0))))
          string)))
  #-(and win32 lispworks)
  (string-downcase arg :start start :end end))

;;; Must we have these generic with the lang parameter?

(defun decline-word (quantity body tail1 tail234 &optional (tail0 tail234))
 ;;; Change the case or the word depending on quanty (e.g. for plural)
  ;; Args: body      ¬озможна пуста€ строка +null-string+, но не NIL!
  ;;       quantity  Non-negative integer.
  ;;       tailNN    NIL or string.
  ;; NB: In some languages, e.g. English, 234 и 567890 coincide (plural).
  ;; ѕросклон€ть часть речи word по числам, добавив нужное окончание taili к корню body.
  (let ((mod (if (<= 11 quantity 19) 0 (mod quantity 10))))
    (string-append body (cond ((= mod 1) (or tail1 +null-string+))
                              ((<= 2 mod 4) (or tail234 +null-string+))
                              (tail0)
                              (t +null-string+)))))

(defun genderize-word (descriptor &optional (gender t))
 ;;; Select a word case depending of gender
  ;; Args: descriptor ::= (word-male word-female [word-neuter:=word-male])
  ;; NB: In some languages, e.g. English, there is no gender declination.
  ;; ¬ыбрать форму слова по полу gender
  (if (listp descriptor) 
      (case gender
        (:male (first descriptor))
        (:female (second descriptor))
        (otherwise (or (third descriptor) (first descriptor))))
      descriptor))

(defun ordinalize-tail (quantity &optional (gender t))
 ;;; Change the case of ordinal numeral
  (declare (ignore gender))
  (let ((string (format nil "~:R" quantity)))
    (subseq string (- (length string) 2))))

(defun transliterate-char (char)
 ;;; Value: character, string or nil
  ;; Q: Should we use Unicode names or Adobe glyph names?
  char)

(defun transliterate-string (string &key element-type)
  (let ((string (string string)))
    (with-output-to-string (stream nil :element-type (or element-type
                                                         (array-element-type string)))
      (dotimes (i (length string))
        (declare (fixnum i))
        (let ((result (transliterate-char (schar string i))))
          (cond ((characterp result)
                 (write-char result stream))
                (result
                 (write-string result stream))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Win32 GetLocaleInfo 

#+win32 
(progn
#|
' LOCALE_ILANGUAGE = &H1         '  language id
' LOCALE_SLANGUAGE = &H2         '  localized name of language
' LOCALE_SENGLANGUAGE = &H1001   '  English name of language
' LOCALE_SABBREVLANGNAME = &H3   '  abbreviated language name
' LOCALE_SNATIVELANGNAME = &H4   '  native name of language
' LOCALE_ICOUNTRY = &H5          '  country code
' LOCALE_SCOUNTRY = &H6          '  localized name of country
' LOCALE_SENGCOUNTRY = &H1002    '  English name of country
' LOCALE_SABBREVCTRYNAME = &H7   '  abbreviated country name
' LOCALE_SNATIVECTRYNAME = &H8   '  native name of country
' LOCALE_IDEFAULTLANGUAGE = &H9  '  default language id
' LOCALE_IDEFAULTCOUNTRY = &HA   '  default country code
' LOCALE_IDEFAULTCODEPAGE = &HB  '  default code page
 |#
(defconstant LOCALE_SLIST	#xC)		; list item separator
; LOCALE_IMEASURE = &HD				' 0 = metric, 1 = US
(defconstant LOCALE_SDECIMAL	#xE)		; decimal separator
(defconstant LOCALE_STHOUSAND	#xF)		; thousand separator
;(defconstant LOCALE_SGROUPING	#x10)		; digit grouping
(defconstant LOCALE_IDIGITS	#x11)		; number of fractional digits
; LOCALE_ILZERO	#x12)				; leading zeros for decimal
; LOCALE_SNATIVEDIGITS = &H13			; native ascii 0-9
(defconstant LOCALE_SCURRENCY	#x14)		; local monetary symbol
; LOCALE_SINTLSYMBOL = &H15			; intl monetary symbol
(defconstant LOCALE_SMONDECIMALSEP #x16)	; monetary decimal separator
(defconstant LOCALE_SMONTHOUSANDSEP #x17)	; monetary thousand separator
;(defconstant LOCALE_SMONGROUPING #x18)		; monetary grouping
(defconstant LOCALE_ICURRDIGITS	#x19)		; local monetary digits
; LOCALE_IINTLCURRDIGITS = &H1A			; intl monetary digits
(defconstant LOCALE_ICURRENCY	#x1B)		; positive currency mode: 0,1,2,3
; LOCALE_INEGCURR = &H1C			; negative currency mode
(defconstant LOCALE_SDATE	#x1D)		; date separator
(defconstant LOCALE_STIME	#x1E)		; time separator
(defconstant LOCALE_SSHORTDATE	#x1F)		; short date format string
; LOCALE_SLONGDATE = &H20			; long date format string
(defconstant LOCALE_STIMEFORMAT	#x1003)		; time format string
(defconstant LOCALE_IDATE	#x21) ; short date format ordering: 0=MDY, 1=DMY, 2=YMD 
; LOCALE_ILDATE = &H22				; long date format ordering
(defconstant LOCALE_ITIME	#x23) ; time format specifier: 0=AM/PM 12-hour,1=24-hour
(defconstant LOCALE_ICENTURY	#x24) ; century format specifier: 0=2-digit, 1=4-digit

#|LOCALE_ITLZERO = &H25          '  leading zeros in time field
' LOCALE_IDAYLZERO = &H26        '  leading zeros in day field
' LOCALE_IMONLZERO = &H27        '  leading zeros in month field
' LOCALE_S1159 = &H28            '  AM designator
' LOCALE_S2359 = &H29            '  PM designator |#
(defconstant LOCALE_SDAYNAME1 	#x2A)		; long name for Monday
#|LOCALE_SDAYNAME2 = &H2B        '  long name for Tuesday
' LOCALE_SDAYNAME3 = &H2C        '  long name for Wednesday
' LOCALE_SDAYNAME4 = &H2D        '  long name for Thursday
' LOCALE_SDAYNAME5 = &H2E        '  long name for Friday
' LOCALE_SDAYNAME6 = &H2F        '  long name for Saturday
' LOCALE_SDAYNAME7 = &H30        '  long name for Sunday|#
(defconstant LOCALE_SABBREVDAYNAME1 #x31)	; abbreviated name for Monday
#|LOCALE_SABBREVDAYNAME2 = &H32  '  abbreviated name for Tuesday
' LOCALE_SABBREVDAYNAME3 = &H33  '  abbreviated name for Wednesday
' LOCALE_SABBREVDAYNAME4 = &H34  '  abbreviated name for Thursday
' LOCALE_SABBREVDAYNAME5 = &H35  '  abbreviated name for Friday
' LOCALE_SABBREVDAYNAME6 = &H36  '  abbreviated name for Saturday
' LOCALE_SABBREVDAYNAME7 = &H37  '  abbreviated name for Sunday
 |#
(defconstant LOCALE_SMONTHNAME1 #x38)		; long name for January
#|LOCALE_SMONTHNAME2 = &H39      '  long name for February
' LOCALE_SMONTHNAME3 = &H3A      '  long name for March
' LOCALE_SMONTHNAME4 = &H3B      '  long name for April
' LOCALE_SMONTHNAME5 = &H3C      '  long name for May
' LOCALE_SMONTHNAME6 = &H3D      '  long name for June
' LOCALE_SMONTHNAME7 = &H3E      '  long name for July
' LOCALE_SMONTHNAME8 = &H3F      '  long name for August
' LOCALE_SMONTHNAME9 = &H40      '  long name for September
' LOCALE_SMONTHNAME10 = &H41     '  long name for October
' LOCALE_SMONTHNAME11 = &H42     '  long name for November
' LOCALE_SMONTHNAME12 = &H43     '  long name for December |#
(defconstant LOCALE_SABBREVMONTHNAME1 #x44)	; abbreviated name for January
#|' LOCALE_SABBREVMONTHNAME2 = &H45 '  abbreviated name for February
' LOCALE_SABBREVMONTHNAME3 = &H46 '  abbreviated name for March
' LOCALE_SABBREVMONTHNAME4 = &H47 '  abbreviated name for April
' LOCALE_SABBREVMONTHNAME5 = &H48 '  abbreviated name for May
' LOCALE_SABBREVMONTHNAME6 = &H49 '  abbreviated name for June
' LOCALE_SABBREVMONTHNAME7 = &H4A '  abbreviated name for July
' LOCALE_SABBREVMONTHNAME8 = &H4B '  abbreviated name for August
' LOCALE_SABBREVMONTHNAME9 = &H4C '  abbreviated name for September
' LOCALE_SABBREVMONTHNAME10 = &H4D '  abbreviated name for October
' LOCALE_SABBREVMONTHNAME11 = &H4E '  abbreviated name for November
' LOCALE_SABBREVMONTHNAME12 = &H4F '  abbreviated name for December
' LOCALE_SABBREVMONTHNAME13 = &H100F
 |#
;; Locale arg value
(defconstant LOCALE_SYSTEM_DEFAULT #x800)
(defconstant LOCALE_USER_DEFAULT #x400)
)

;;; The GetLocaleInfo function always retrieves information in text format.
;;; If the information is a numeric value, the function converts the number
;;; to text using decimal notation.
;;;
;;; The LOCALE_FONTSIGNATURE parameter will return a non-NULL terminated string.
;;; In all other cases, the string is NULL terminated.

#+(and win32 lispworks)
(defun get-locale-info (info-type &optional (locale :user))
  (fli:with-dynamic-foreign-objects ((buffer (:ef-mb-string :limit 128)))
					     ;:external-format *internal-format*
    (let ((length (|GetLocaleInfo| (case locale
                                     (:user LOCALE_USER_DEFAULT)
                                     (:system LOCALE_SYSTEM_DEFAULT)
                                     (otherwise locale))
                                   info-type buffer)))
    (unless (zerop length)
      (fli:convert-from-foreign-string buffer :external-format *internal-format*
                                       :length length :null-terminated-p t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Locale-specific reading and printing
;;; The IO and FORMAT packages are specific to LW 4.x on Windows/Linux.
;;; Starting from release 5, LW do not have the these packages on any platform.

(defparameter *decimal-separator* #\.)

;;; Format a la  ~w,d,k,overflowchar,padchar,decimalcharF
;;; where the decimalchar in non-standard
;;; LispWork NB: @ works for non-negative numbers only if w or d is non-null.

#+(and lispworks4 (or win32 linux)) 
(defun ~f (stream arg &optional colon atsign w d k over pad decimal)
  (declare (ignore colon))
  (cond ((not (realp arg))
         (prin1 arg stream)
         (return-from ~f arg))
        ((not (floatp arg))
         (setq arg (float arg))))
  #+lispworks (when (and atsign (not (or w d)) (plusp arg))
                (write-char #\+ stream))
  (when (and (or w d) (null pad)) (setq pad #\Space))
  (if (eql (or decimal (setq decimal *decimal-separator*)) #\.)
      (let ((io:*active-output* stream))
        (declare (dynamic-extent io:*active-output*))
        #1=(format::f-formatter arg w d k over pad atsign))
      (let ((io:*active-output* (make-string-output-stream)))
        (declare (dynamic-extent io:*active-output*))
        #1#
        (write-string (nsubstitute decimal #\.
                                   (get-output-stream-string io:*active-output*)
                                   :count 1)
                      stream)))
  arg)

#-(and lispworks4 (or win32 linux)) 
(defun ~f (stream arg &optional colon atsign w d k over pad decimal)
  (declare (ignore colon))
  (if (eql (or decimal (setq decimal *decimal-separator*)) #\.)
      (format stream #1=(if atsign "~V,V,V,V,V@F" "~V,V,V,V,VF") w d k over pad arg)
      (write-string (nsubstitute decimal #\. (format nil #1# w d k over pad arg)
                                 :count 1)
                    stream))
  arg)

(defun f-format-to-string (arg &key w d k over pad decimal atsign)
  (if arg
      (with-output-to-string (stream)
        (~f stream arg nil atsign w d k over pad decimal))
      +null-string+))

;;; Format a la ~w,d,e,k,overflowchar,padchar,'e,decimalcharE
;;; where the decimalchar in non-standard and exponentchar is always #\e.

#+(and lispworks4 (or win32 linux))
(defun ~e (stream arg &optional colon atsign w d e k over pad #|expchar=#\e|# decimal)
  (declare (ignore colon))
  (cond ((not (realp arg))
         (prin1 arg stream)
         (return-from ~e arg))
        ((not (floatp arg))
         (setq arg (float arg))))
  (when (and w (null pad)) (setq pad #\Space))
  (if (eql (or decimal (setq decimal *decimal-separator*)) #\.)
      (let ((io:*active-output* stream))			 ; k seems obliged
        #2=(format::e-formatter arg w d e (or k 1) over pad #\e atsign))
      (let ((io:*active-output* (make-string-output-stream)))
        (declare (dynamic-extent io:*active-output*))
        #2#
        (write-string (nsubstitute decimal #\.
                                   (get-output-stream-string io:*active-output*)
                                   :count 1)
                      stream)))
   ;     ((format stream (if atsign "~V,V:D" "~V,VD") w pad arg)))
  arg)

#-(and lispworks4 (or win32 linux))
(defun ~e (stream arg &optional colon atsign w d e k over pad #|expchar=#\e|# decimal)
  (declare (ignore colon))
  (if (eql (or decimal (setq decimal *decimal-separator*)) #\.)
      (format stream #1=(if atsign "~V,V,V,V,V,V,'e@E" "~V,V,V,V,V,V,'eE")
              w d e k over pad arg)
      (write-string (nsubstitute decimal #\. (format nil #1# w d e k over pad arg)
                                 :count 1)
                    stream))
  arg)

(defun e-format-to-string (arg &key w d e k over pad decimal atsign)
  (if arg
      (with-output-to-string (stream)
        (~e stream arg nil atsign w d e k over pad decimal))
      +null-string+))

;;; Currency

(defparameter *currency-thousand-separator* #\,)
(defparameter *currency-digits* 2)		; the number of fractional digits
(defparameter *currency-symbol* #\$)		; local monetary symbol
(defparameter *currency-order*  0)		; positive currency mode

(defun ~$ (stream arg &optional colon atsign d n w pad
                                decimal (thousand *currency-thousand-separator*)
                                symbol order &aux (dw w))
 ;;; Format a la ~d,n,w,padchar,decimalchar,thousandchar,currencysymbol,currencyorder$
  ;; We cannot rely on the standard ~$ as it does not support thousandchar.
  ;; d	    The number of digits after the decimal.
  ;; n	    The minimum number of digits before decimal (ignored).
  ;; w	    The minimum total width of the field.
  ;; symbol Currency symbol: T means default
  ;; order  0 Prefix, no separation, for example $1.1 
  ;;	    1 Suffix, no separation, for example 1.1$ 
  ;;	    2 Prefix, 1-character separation, for example $ 1.1 
  ;;	    3 Suffix, 1-character separation, for example 1.1 $ 
  ;; @ modifier  If the arg is non-negative, a plus sign is printed.
  ;; : modifier  The sign appears before any padding, and otherwise after the padding.
  ;;
  ;; If w is supplied and the number of other characters to be output is less than w,
  ;; then copies of padchar (which defaults to a space) are output to make the total
  ;; field width equal w. Then n digits are printed for the integer part of arg, 
  ;; with leading zeros if necessary; then a decimal character; 
  ;; then d digits of fraction, properly rounded. 
  ;;
  ;; If the magnitude of arg is so large that more than m digits would have
  ;; to be printed, where m is the larger of w and 100, then an implementation is free,
  ;; at its discretion, to print the number using exponential notation instead, 
  ;; as if by the directive ~w,d+n-1,,,,padcharE.
  ;;
  ;; NB: Due to our relying on LW ~D implementation, which does not breaks padding
  ;;	 by commachar, we can get something like this "00004,324,213.23"

  (declare (ignore n)) ;(unless n (setq n 1))
  (unless (realp arg)
    (prin1 arg stream)
    (return-from ~$ arg))
  (unless d (setq d *currency-digits*))
  (unless decimal (setq decimal *decimal-separator*))
  (when (eql symbol t) (setq symbol *currency-symbol*))
  (unless order (setq order *currency-order*))
  (when (and w symbol) (decf dw (+ (length (string symbol)) (if (> order 1) 1 0))))

  (when (and symbol (evenp order))		; Print prefix currency symbol
    (write-string (string symbol) stream)
    (when (> order 1) (write-char #\Space stream)))
  
  (if thousand
      (let ((sign (cond ((minusp arg) #\-) (atsign #\+)))
            integral fractional)
        (if (integerp arg)
            (setq integral (abs arg)
                  fractional 0s0)
            (multiple-value-setq (integral fractional)		; a la sql-round
                (floor (abs (if (zerop d)			; if d is non-zero
                                (float arg)
                                (let ((factor (if (eql d 2) 1/100 (expt 10 (- d)))))
                                  (* (fround (float arg) factor) factor)))))))
        (cond (w
               (cond (colon
                      (when sign (write-char sign stream) (decf dw))
                      (format stream "~V,V,V:D" (- dw (1+ d)) pad thousand integral))
                     (t
                      (format stream (if atsign "~V,V,V:@D" "~V,V,V:D")
                                     (- dw (1+ d)) pad thousand
                                     (if (minusp arg) (- integral) integral)))))
              (t
               (when sign (write-char sign stream))
               (format stream "~,,V:D" thousand integral)))
        (~f stream fractional nil nil (1+ d) d nil nil nil decimal))
      (~f stream (float arg) nil atsign dw d nil nil pad decimal))

  (when (and symbol (oddp order))		; Print suffix currency symbol
    (when (> order 1) (write-char #\Space stream))
    (write-string (string symbol) stream)))

(defun $-format-to-string (arg &key d n w pad decimal atsign colon
                                    (thousand *currency-thousand-separator*)
                                    symbol order)
  (if arg
      (with-output-to-string (stream nil :element-type yl:*default-char-type*)
        (~$ stream arg colon atsign d n w pad decimal thousand symbol order))
      +null-string+))

;;; Date and time

(defparameter *date-separator*	#\-)
(defparameter *date-format*	:ymd)
(defparameter *century-digits*	4)
(defparameter *month-names*  #("January" "February" "March" "April" "May" "June" "July"
                               "August" "September" "October" "November" "December"))
(defparameter *month-abbrevs* (map 'vector (lambda (string) (subseq string 0 3))
                                           *month-names*))
(defparameter *weekday-names*	#("Monday" "Tuesday" "Wednesday" "Thursday"
                                  "Friday" "Saturday" "Sunday"))
(defparameter *weekday-abbrevs*	#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defparameter *time-separator*	#\:)
(defparameter *time-format*	24)		; 12=AM/PM 12-hour, 24=24-hour

#+lispworks
(defun initialize-locale ()
 ;;; Initialization taking place both at load-time and run-time in delivered image
  (fli:set-locale)			; set C locale => "Russian_Russia.1251"
  (setq *internal-format* #+win32 (if (string= (software-type) "Windows NT")
                                      :unicode
                                      win32:*multibyte-code-page-ef*)
                          #-win32 :ascii)
  #+win32
  (setq *decimal-separator* (schar (get-locale-info LOCALE_SDECIMAL) 0)

        *currency-thousand-separator* (let((s (get-locale-info LOCALE_SMONTHOUSANDSEP)))
                                        (when (plusp (length s)) (schar s 0)))
        *currency-digits* (parse-integer (get-locale-info LOCALE_ICURRDIGITS))
        *currency-symbol* (get-locale-info LOCALE_SCURRENCY)
        *currency-order* (parse-integer (get-locale-info LOCALE_ICURRENCY))

        *date-separator* (schar (get-locale-info LOCALE_SDATE) 0)
        *date-format*	 (or (cdr (assoc (schar (get-locale-info LOCALE_IDATE) 0)
                                         '((#\0 . :mdy) (#\1 . :dmy)))) :ymd)
        *century-digits* (* 2 (1+ (parse-integer (get-locale-info LOCALE_ICENTURY))))

        *month-names*	 (apply #'vector (loop for info-type upfrom LOCALE_SMONTHNAME1
                                               repeat 12
                                               collect (get-locale-info info-type)))
        *month-abbrevs*	 (apply #'vector (loop for info-type upfrom LOCALE_SABBREVMONTHNAME1
                                               repeat 12
                                               collect (get-locale-info info-type)))
        *weekday-names*	 (apply #'vector (loop for info-type upfrom LOCALE_SDAYNAME1
                                               repeat 7
                                               collect (get-locale-info info-type)))
	*weekday-abbrevs*(apply #'vector (loop for info-type upfrom LOCALE_SABBREVDAYNAME1
                                               repeat 7
                                               collect (get-locale-info info-type)))
        *time-separator* (schar (get-locale-info LOCALE_STIME) 0)
        *time-format*	 (if (eq (schar (get-locale-info LOCALE_ITIME) 0) #\1) 24 12)
) )

(defun timestamp-string (&optional (timezone-p t))
 ;;; Format current universal time with English month abbreviation and optionally
  ;; timezone displacement hours. Useful for logging and debugging.
  (multiple-value-bind (seconds minutes hours day month year weekday daylight-p tz-hour)
      (get-decoded-time)
    (declare (ignore weekday))
    (setq tz-hour (- (if daylight-p 1 0) tz-hour))
    (format nil "~4D-~A-~2,'0D ~2,'0D:~2,'0D:~2,'0D~:[~;~:[+~;-~]~2,'0D~]"
            year (svref (load-time-value *month-abbrevs*) (1- month)) day
            hours minutes seconds
            timezone-p (minusp tz-hour) (abs tz-hour))))
   ;#+cmu (ext:format-universal-time nil (get-universal-time)
   ;                                 :print-timezone timezone-p :print-weekday nil)

#+lispworks (eval-when (:load-toplevel) (initialize-locale))
#+lispworks (lw:define-action "When starting image" "Initialize locale" 'initialize-locale)
