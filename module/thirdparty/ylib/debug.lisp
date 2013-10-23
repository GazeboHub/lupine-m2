;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-
;;; Ystok-Library - Logging and debugging (a la AllegroServe)
;;; Copyright (c) 2003-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ystok.library)

#+debug (defvar *debug-all* nil)	; all of the debugging options
#+debug (defvar *debug-log* nil)	; all debug options that write info to *debug-stream*
#+debug (defvar *debug-current* nil)	; current options set
#+debug (defvar *debug-stream* t)	; i.e. standard-output; if Allegro *initial-terminal-io*

#+debug
(defmacro define-debug-option (option class debug-description)
  `(progn (ecase ,class
	    (:all (pushnew ,option *debug-all*))
	    (:log (pushnew ,option *debug-log*)
		  (pushnew ,option *debug-all*)))
	  (setf (get ,option 'debug-description) ,debug-description)))

;#+debug (define-debug-option :info :log "General information")

#+debug
(defun debug-on (&rest options)
 "Add the given debug options to the tracing list; if none given, display current status."
  (if (null options)
      (debug-status)
      (dolist (option options *debug-current*)
        (case option
          (:all (setq *debug-current* *debug-all*))
	  (:log (setq *debug-current* (union *debug-current* *debug-log*)))
          (t (pushnew option *debug-current*))))))

#+debug
(defun debug-off (&rest options)
 "Turn off tracing of the debugging options; if none given, display current status."
  (if (null options)
      (debug-status)
      (dolist (option options *debug-current*)
        (case option
          (:all (setq *debug-current* nil))
          (:log (setq *debug-current* (set-difference *debug-current* *debug-log*)))
	  (t (setq *debug-current* (remove option *debug-current*)))))))

#+debug
(defun debug-status ()
 "Describe what debugging options exist and if they are on and off"
  (dolist (option *debug-all*)
    (format t "~&~7S ~:[off~;on ~]  ~A" option (member option *debug-current* :test #'eq)
	    (get option 'debug-description))))

#+debug
(defun debug-format (option &rest args)
 "Do print to *debug-stream* if the option equals to T or matches *debug-current*"
  (when (and *debug-stream* (or (eq option t) (member option *debug-current* :test #'eq)))
    (format *debug-stream* "~&~@[{~A}~]> ~?"
            #+lispworks (mp:process-name mp:*current-process*)
            #-lispworks nil
            (first args) (rest args))))

;;; Retail (delivered) staff does nothing

#-debug
(defmacro debug-format (option &rest args) (declare (ignore option args)))

#| #+lispworks
(lw:define-action "Delivery Actions" "Drop debug options"
   (lambda ()
     (dolist (option *debug-all*) (remprop option 'debug-description))
     (makunbound '*debug-all*)
     (makunbound '*debug-log*)))|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  RESTARTS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Allow or suppress some standard restarts usually provided by Lisp systems, e.g.
;;; if %allow-debug% were true, that should permit invoke-debugger or printing backtrace.
;;; Used by: ywi:prompt-for-restarts

;; Bind to T within confirm-destroy or the like.
;; Bind to NIL if you establish another abort restart of your own.
(defvar %allow-quit% :default)

;; Set global value (may affect deliver) or bind to T if bug report should be available
;; in full.
(defparameter %allow-debug% nil)

(defmacro with-restarts ((&key (quit *unbound-value*) (debug *unbound-value*)) &body body)
  `(let (,@(unless (eq quit *unbound-value*)
             `((%allow-quit% ,quit)))
         ,@(unless (eq debug *unbound-value*)
             `((%allow-debug% ,debug))))
     ,@body))

;;; Asynchronous help about an error occured: condition classes and signalers
;;; If help-key equals to :type, that means the condition type symbol (must be kept!)
;;; Used by: ystok.widgets:prompt-for-restart

(defvar %help-key% nil)

(define-condition helped-condition ()
 ((help-key :reader help-key :initarg :help-key :initform :type)))

(define-condition simple-herror (simple-error helped-condition) ())

(defun herror (help-key datum &rest args)
  (if (symbolp datum)				; must designate a sublcass of hcondition
      (if (typep datum 'helped-condition)
          (apply #'error datum :help-key help-key args)
          (let ((%help-key% help-key))
            (apply #'error datum args)))
      (error 'simple-herror
             :format-control datum
             :format-arguments args
             :help-key help-key)))

(defun hcerror (help-key format-control datum &rest args)
 ;;; CAUTION: When datum is string, format-control should not contain formatting
  ;;          directives as the list of arguments it is fed follows 'simple-herror, i.e.
  ;;          (:format-arguments args ...)
  (if (symbolp datum)				; must designate a sublcass of hcondition
      (if (typep datum 'helped-condition)
          (apply #'cerror format-control datum :help-key help-key args)
          (let ((%help-key% help-key))
            (apply #'cerror format-control datum args)))
      (cerror format-control 'simple-herror	; do not use ~ directives in format-control
              :format-arguments args
              :format-control datum
              :help-key help-key)))

(define-condition simple-hwarning (simple-warning helped-condition) ())

(defun hwarn (help-key datum &rest args)
  (if (symbolp datum)				; must designate a sublcass of hcondition
      (if (typep datum 'helped-condition)
          (apply #'warn datum :help-key help-key args)
          (let ((%help-key% help-key))
            (apply #'warn datum args)))
      (warn 'simple-hwarning
            :help-key help-key
            :format-control datum
            :format-arguments args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  LOG  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging to *standard-output*, collecting-display-pane-stream, or broadcast-stream

(defparameter *log-print-length* 20)
(defparameter *log-print-level* 2)

(defvar *log-stream* nil)

;; Record of option T or :error is always logged, of option NIL is never logged
(defvar *log-options* '(:warn :info)			; :process, :timestamp
 "List of keywords, each denotes a program component or option")

(defvar %in-logg% nil)				; prevent recursion while in logger inself

(defun logg (option format-controller &rest format-arguments)
 "Output a log record to *log-stream*.
Arguments:
 option Designator of severity, module, or both by means of a dotted list
        (module1 [module2]... . severity), where
        module ::= symbol
        severity ::= :error | :warn | T
 format-controller
	String or condition instance (for :warn or :error option only).
Value: NIL.
 The module symbols and :warn are checked by means of calling
   (member symbol *log-options* :test #'eq).
 The severity designators T and :error are always assumed there.
 The option :module is equivalent (:module . :info).
 Several modules in the option list are combined according to AND principle."
  (when (and *log-stream* (not %in-logg%))
   (let ((%in-logg% t)
         (options (thys-preference '*log-options*)))
    (labels ((%warn ()
               ;#+debug (apply #'warn format-controller format-arguments)
               (when (and (memq :warn options)
                          #+debug (not (or (eq *log-stream* t)
                                           (eq *log-stream* *error-output*))))
                 (let #1=((*print-length* *log-print-length*)
                          (*print-level* *log-print-level*))
                   (format *log-stream* (if (typep format-controller 'condition)
                                            "~&**~@[{~A}~]~@[~A~] ~A"
                                            "~&**~@[{~A}~]~@[~A~] ~?")
                           #2=(when (memq :process options)
                                #+(or lispworks cmu) (mp:process-name mp:*current-process*)
                                #+allegro        (mp:process-name system:*current-process*)
                                #-(or lispworks allegro cmu) nil)
                           #3=(when (memq :timestamp options)
                                (timestamp-string nil))
                           format-controller format-arguments))))
             (%error ()
               (when (and #+debug (not (or (eq *log-stream* t)
                                           (eq *log-stream* *error-output*))))
                 (let #1#
                   (format *log-stream* (if (typep format-controller 'condition)
                                            "~&***~@[{~A}~]~@[~A~] ~A"
                                            "~&***~@[{~A}~]~@[~A~] ~?")
                           #2# #3# format-controller format-arguments))))
             (%info ()
               (let #1#
                 (format *log-stream* (if (or (eq *log-stream* t)
                                              (eq *log-stream* *standard-output*))
                                          "~&;~@[{~A}~]~@[~A~] ~?"
                                          "~&~@[{~A}~]~@[~A ~]~?")
                         #2# #3# format-controller format-arguments)))
             (%logg (option)
               (if (consp option)				; (module . rest) 
                   (when (memq (car option) options)		; module is in options
                     (%logg (or (cdr option) :info)))
                   (case option
                     (:warn     (%warn))
                     (:error    (%error))
                     (:info     (when (memq :info options) (%info)))
                     ((t)       (%info))
                     (otherwise (when (and (memq option options) (memq :info options))
                                   (%info)))))))
      (%logg option)))))
