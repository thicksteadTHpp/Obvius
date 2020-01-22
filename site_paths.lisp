;;;; Modify this file to contain appropriate pathnames for your system.

;;; [THO] no need to modify by hand
;;; try to make asdf lokk for the right paths

(in-package :obvius)
(export '(*default-printer*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO [THO] get rid of it
;;; Memory availability on your machines, in 64k segments
;; (lcl:change-memory-management :growth-limit 750)   ;50 Megabytes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; *** Not currently using logical pathnames, probably should.
;;;[tho} set up logical pathnames

(defvar *real-obvius-directory* (namestring (asdf:system-relative-pathname "obvius01" "")))



(defmacro str+ (&rest args)
  `(concatenate 'string ,@args))

;;original set as in mcl.lisp
;; (setf (logical-pathname-translations "obv")
;;       `(("lisp-source;**;*.*" "Macintosh HD:Obvius:lisp-source:**:*.*")
;;         ("bin;**;*.*" "Macintosh HD:Obvius:bin:**:*.*")
;;         ("c-source;**;*.*" "Macintosh HD:Obvius:c-source:**:*.*")
;;         ("**;*.*" "Macintosh HD:Applications:mpw:**:*.*")))
;; |#


(setf (logical-pathname-translations "obv")
      `(("lisp-source;**;*.*" ,(str+ *real-obvius-directory* "**/*.*"))
        ("bin;**;*.*" ,(str+ *real-obvius-directory* "bin/**/*.*"))
        ("c-source;**;*.*" ,(str+ *real-obvius-directory* "c-source/**/*.*"))
	("images;*.*" ,(str+ *real-obvius-directory* "images/*.*"))
        ("**;*.*" ,(str+ *real-obvius-directory* "mpw/**/*.*"))))

;; [TODO] use xdg or something to make it implementation independent
;; this will only work on unix now
(defparameter *temp-dir* #+UNIX "/tmp/obv/")

(setf (logical-pathname-translations "tmp")
      `(("*;*.*" ,(str+ *temp-dir* "*/*.*"))
	(""      ,*temp-dir*)))

(defun in-tempdir (filename)
  (str+ (namestring (translate-logical-pathname "tmp:")) filename))   

(eval-when (:load-toplevel :execute)
  (uiop/common-lisp:ensure-directories-exist (translate-logical-pathname "tmp:")))


(defvar *obvius-directory-path* (pathname *real-obvius-directory*))
(defvar *lisp-source-path* "");;(str+ *obvius-directory-path* "lisp-source;"))
(defvar *c-source-path* "");;(str+ *obvius-directory-path* "c-source;"))
(defvar *binary-path* "");;(str+ *obvius-directory-path* "bin;"))
(defvar *obvius-image-path* (merge-pathnames #P"images/" *obvius-directory-path*))



;;;; Directory paths.  NOTE: these must end in a slash!!!

;;(defvar *lucid-path*                     "/usr/src/lucid-4.1/")
;; (defvar *obvius-directory-path*          "/usr/src/obvius-3.0/")
;; (defvar *lisp-source-path*               "/usr/src/obvius-3.0/lisp-source/")
;; (defvar *c-source-path*                  "/usr/src/obvius-3.0/c-source/")
;; (defvar *binary-path* 
;;   #+(and Sun (not Sparc))                "/usr/src/obvius-3.0/sun3-bin/"
;;   #+(and Sun Sparc (not *lisp-hardware)) "/usr/src/obvius-3.0/sun4-bin/"
;;   #+Irix                                 "/usr/src/obvius-3.0/sgi-bin/"
;;   #+Vax                                  "/usr/src/obvius-3.0/vax-bin/"
;;   #+HP                                   "/usr/src/obvius-3.0/hp-bin/")
;; (defvar *tiff-library*                   "/usr/local/lib/libtiff.a")
;; (defvar *obvius-image-path*              "/usr/src/obvius-3.0/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Printing environment

(defvar *temp-ps-directory* "tmp:"
  "Temporary directory for PostScript files which are being sent to the printer.")

(defvar *default-printer* ""
  "Name of a PostScript printer.")

;;; Set :type property of *default-printer* variable (don't change this).
(eval-when (load eval) (setf (get '*default-printer* :type) 'string))

;;; The ~@* directive skips back to re-process the first arg!
(defvar *print-command-string* "cat ~A | lpr -P~A ; rm ~@*~A"
  "A print command.  This should be a format string which takes two args:
the filename and the printer .  On Unix systems, it will be run as a shell
program.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; File suffixes

(defvar *source-file-suffix*
  #+Unix ".lisp"
  #+Sun  ".lisp"
  #+Vax  ".lisp"
  #+Irix ".lisp"
  #+HP   ".l")

(defvar *binary-file-suffix* 
  #+(and Sun (not Sparc)) ".lbin"
  #+(and Sun Sparc) ".sbin"
  #+Irix ".mbin"
  #+Vax ".vbin"
  #+HP  ".b")
