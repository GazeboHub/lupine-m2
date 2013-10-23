;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-
;;; Ystok Library - General purpose Win32 API definitions
;;; Copyright (c) 2003-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ystok.library)

#-(and lispworks win32)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This file can only be used on LispWorks for Windows!"))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defparameter *default-win32api-module* "kernel32")

 (defmacro define-win32api (name args result-type &rest rest
                            &key export module encoding &allow-other-keys)
 `(dspec:def (define-win32api ,name)
   (progn
     (when ,export (export ',name))
     (fli:define-foreign-function (,name ,(symbol-name name)
                                   ,@(when encoding `(,encoding))) ; :source or :dbcs
         ,args
       :result-type ,result-type
       :calling-convention :stdcall
       :module ,(or module *default-win32api-module*)
       ,@(remove-properties rest '(:export :module :encoding))))))

 (dspec:define-dspec-alias define-win32api (name) `(fli:define-foreign-function ,name))
    
 ;(fli:define-c-typedef WORD (:unsigned :short))
 ;(fli:define-c-typedef DWORD (:unsigned :long))
) ; eval-when

;;; Character and string case conversion
;;; Win32 API uses the language driver for the current language selected
;;; by the user at setup or by using Control Panel. 
;;; ListWorks:
;;;   Shall we use WIN32::LOCALE-CHAR-DOWNCASE$CHAR-CODE
;;;   and WIN32::LOCALE-CHAR-DOWNCASE$CHAR-CODE instead?

(define-win32api |CharLower|((lpsz :long)) :long :encoding :dbcs :module "user32")
(define-win32api |CharUpper|((lpsz :long)) :long :encoding :dbcs :module "user32")
(define-win32api |CharLowerBuff|
 ((lpsz :pointer)			; LPTSTR lpsz	  ; characters
  (length win32:DWORD))			; DWORD cchLength ; number of chars to process
 :void :encoding :dbcs :module "user32"); the number of TCHARs processed
(define-win32api |CharUpperBuff|
 ((lpsz :pointer)
  (length win32:DWORD))
 :void :encoding :dbcs :module "user32")

(defun %wstring-upper-down-case (string start end upper-case-p)
 ;;; Args: string  Unicode string (simple or not), character, or symbol (including NIL).
  ;; Value: Always a copy of the string.
  ;; CAUTION: Does not work on Windows 98.
  ;; NB: Despite the doc, fli:foreign-typed-aref really requires the byte offset argument.
  ;; NB: Use win32::char-lower$string instead?
  (declare (fixnum start) (type string string)
           (optimize (speed 3) (debug 0) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  #+win32
  (fli:with-dynamic-foreign-objects ()
    (let* ((length (length string))
           (end (or end length))
           (size (- end start))
           (ptr (fli:allocate-dynamic-foreign-object :type :wchar-t :nelems size)))
      (declare (type string string)
               (fixnum length end size))
      (do ((i start (1+ i)) (j 0 (+ j 2))) ((>= i end)) (declare (fixnum i j))
        (setf (fli:foreign-typed-aref '(unsigned-byte 16) ptr j)
              (char-code (char string i))))
        ;(setf (fli:dereference ptr) (char string i)) (fli:incf-pointer ptr)))
      ;(fli:decf-pointer ptr size)
      (if upper-case-p
          (|CharUpperBuff| ptr size)
          (|CharLowerBuff| ptr size))
      (let ((value (make-string length :element-type 'lw:simple-char)))
        (declare (type lw:simple-text-string value))
        (do ((i 0 (1+ i))) ((>= i start)) (declare (fixnum i))
          (setf (lw:stchar value i) (char string i)))
        (do ((i start (1+ i)) (j 0 (+ j 2))) ((>= i end)) (declare (fixnum i j))
          (setf (lw:stchar value i) (code-char (fli:foreign-typed-aref
                                                '(unsigned-byte 16) ptr j))))
                ;(fli:dereference ptr)) (fli:incf-pointer ptr))
        (do ((i end (1+ i))) ((>= i length)) (declare (fixnum i))
          (setf (lw:stchar value i) (char string i)))
        value))))


(define-win32api |GetLocaleInfo|
 ((Locale win32:DWORD)
  (LCType :int)
  (lpLCData :pointer)
  (:constant 64 :int))
 :integer
 :encoding :dbcs)

;;; file-utils.lisp

(define-win32api |GetFileTitle|
 ((lpszFile :pointer)
  (buffer :pointer)
  (length win32:WORD))
 :short
 :encoding :dbcs :module "comdlg32")


#||
;;; The below code will not work on Windows 98 - we do need explicit conversion to ANSI: 
(let ((*code-page* (if (consp ef) (EF::EF-CODED-CHARACTER-SET ef) ef)))
  (if (neq *internal-format* :unicode)
      (ef:find-external-char (|CharLower| (ef:char-external-code char *code-page*))
                             *code-page*))

;(WIN32:FORMAT-MESSAGE #x1000 nil 127 0 nil) ; FORMAT_MESSAGE_FROM_SYSTEM
;(WIN32:GET-LAST-ERROR)
;(WIN32:GET-LAST-ERROR-STRING)
;(WIN32:GET-LAST-ERROR-STRING :error-code 5)
||#
