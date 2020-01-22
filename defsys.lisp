;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Edited by [THO] in 2019 to work under SBCL
;;; this file only contains some reelevant definitions which are
;;; needed
;;; The loading of modules is done by quicklisp/asdf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: lucid-defsys.lisp
;;;  Author: Simoncelli/Heeger
;;;  Description: System definition file for OBVIUS in Lucid Common Lisp.
;;;  Creation Date:  Spring, 1988
;;;  Modified:  Fall, 1989
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;   [THO] The following list is only printed for historic reasons
;;;  
;;; 1) Edit the global variables in the file lucid-site-paths.lisp.
;;; 2) Edit pathnames in the Makefile's in the <c-source> directory.
;;; 3) Edit lucid-site-init.lisp file to pre-load desired modules.
;;; 4) Start from a Lucid Lisp image, v4.0 or higher, with CLOS and
;;;    LispView loaded.  If you are in Emacs, do M-x load-file
;;;    "<obv>/emacs-source/cl-obvius", and then M-x run-cl.  Otherwise,
;;;    just run Lucid Common Lisp from the shell.
;;; 5) Load this file into your lisp world: (load "<obv>/lucid-defsys").
;;; 6) Before using OBVIUS, we recommend compiling all the files.  To do this,
;;;    execute (obv::load-obvius).  Then execute (quit), start up a new lisp,
;;;    and re-load lucid-defsys.
;;; 6) To run OBVIUS, execute (obvius::run-obvius).
;;; 7) To save OBVIUS as an executable lisp image, run (obvius::make-obvius). 
;;; 6) You may wish to create .obvius and/or .obvius-windows files in your
;;;    home directory for personal modifications.  The defaults are
;;;    <obv>/lucid-obvius-init.lisp and
;;;    <obv>/lucid-window-init.lisp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pushnew :OBVIUS *features*)	   ;Add OBVIUS to the *features* list

(in-package :obv)

;; maybe add one up [THO]
(defconstant *obvius-version* 3.1 "OBVIUS version number.")

;;; [THO] check if that is really needed
;;; Production mode compiler optimizations.  These are used when
;;; compiling OBVIUS source.  NOTE: It seems that we MUST COMPILE
;;; LUCID-FFI.lisp in production mode for it to work!
(defvar *obvius-compiler-optimizations*
  '((COMPILATION-SPEED 0) (SPEED 3) (SAFETY 0) (SPACE 0)))

;; (LCL:def-foreign-function (set-umask (:language :c)
;; 				     #+Unix (:name "umask")
;; 				     #+Sun (:name "_umask")
;; 				     #+Irix (:name "umask")
;; 				     (:return-type :signed-32bit))
;;     (number :signed-32bit))

#+Sun
(LCL:load-foreign-libraries '() '("-lc"))
#+Irix
(LCL:load-foreign-libraries '() '("-lsun" "-lc"))

;;; Set file write permissions.  Easiest to specify as octal (eg,
;;; #o002).  Nil arg means don't modify.
(defmacro with-umask (umask &body body)
  (let ((new-mask (gensym)) (old-mask (gensym)))
    `(let ((,new-mask ,umask)
	   ,old-mask)
      (if ,new-mask			;if nil, leave umask alone.
	  (unwind-protect
	       (progn (setq ,old-mask (set-umask ,new-mask))
		      ,@body)
	    (set-umask ,old-mask))
	  (progn ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Stuff for loading and compiling OBVIUS files

;;; [THO] this id done by asdf
;;; Load pathnames for this site:
;;(load (merge-pathnames "lucid-site-paths.lisp" lucid::*source-pathname*))

(defvar *obvius-features* nil
  "List of modules loaded using obv-require")

;;; Require a module for obvius.  Works like the Common Lisp require
;;; function, except it looks for pathnames in *obvius-module-plist*
;;; (defined below) and fills in missing directories in filenames with
;;; obvius source-path.  *** This is a little broken -- If user loads
;;; the file(s) manually, they will not go on the *obvius-features*
;;; list.
;; (defun obv-require (module-name
;; 		    &key
;; 		    (pathnames (or (getf *obvius-module-plist* module-name)
;; 				   (string-downcase module-name)))
;; 		    (initialize t))
;;   ;; Fill in default directory: look in *lisp-source-path*
;;   (setq pathnames
;; 	(mapcar #'(lambda (path)
;; 		    (merge-pathnames path *lisp-source-path*))
;; 		(if (listp pathnames) pathnames (list pathnames))))
;;   (when (not (member module-name *obvius-features*))
;;     (dolist (file pathnames)
;;       (obv-compile-load file))
;;     (setq *obvius-features* (pushnew module-name *obvius-features*))
;;     (when initialize (run-initialization-functions))
;;     t))					;return t => files loaded


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; [THO] printed here for historic reasons
;;;; you find a copy of this in the asdf files
;;;; Definition of the OBVIUS system and subsystems.

;; (defvar *obvius-viewable-files*
;;   (list "viewable" "viewable-classes" 
;;         "viewable-matrix" "viewable-sequence" "discrete-function" ))

;; (defvar *obvius-image-files*
;;   (list "lucid-image-loops" "image" "bit-image" 
;;         "image-matrix" "image-sequence" "image-pair"
;;         "synth")) 
        
;; (defvar *obvius-operations-files*
;;   (list "generic-ops" "user-macros"  
;;         "imops" "warp" "fft" "fileio" "filter"))

;; (defvar *obvius-system-files* 
;;   (append
;;    (list "lucid-source-file-extensions"	;extensions for recording Method source files
;; 	 "lucid-repl" "lucid-clos-extensions" "lucid-hacks" "lucid-ffi"
;; 	 "misc" "memory" "generic-fns"
;; 	 "array-ops" "arrayio-c" "matrix" "list-ops")
;;    *obvius-viewable-files* *obvius-image-files* *obvius-operations-files*))

;; (defvar *obvius-picture-files*
;;   (list "picture" "pane" "picture-classes"
;; 	"gray" "drawing"
;; 	"coord-xforms" "graph" "surface-plot"
;; 	"flipbook" "pasteup" "overlay" "hardcopy"))

;; (defvar *obvius-lispview-files*  
;;   (list
;;    "lv-patches"
;;    "lv-image-patch"     ;LispView patch for faster image blt'ing (but it's still slow!)
;;    "lv-bucky-patch"	;LispView patch to catch bucky events for mouse doc
;;    "lv-color-patch"	;LispView patch to keep track of allocated colors
;;    "lv-screen"
;;    "lv-window"
;;    "lv-blt"
;;    "lv-draw"
;;    "lv-mouse-utilities"
;;    "lv-mouse"
;;    "lv-dialogs"
;;    ))

;; (defvar *gl-ffi-files*
;;   (list
;;    "gl-ffi.lisp"   
;;    ))

;; (defvar *obvius-gl-files*
;;   (list
;;    "gl-obv-patches"
;;    "gl-obv-lucid-ffi"
;;    "gl-obv-dispatch"
;;    "gl-obv-cursor"
;;    "gl-obv-screen"
;;    "gl-obv-window"
;;    "gl-obv-blt"  
;;    "gl-obv-8-bit"
;;    "gl-obv-24-bit"
;;    "gl-obv-pane"   
;;    "gl-obv-events"  
;;    "gl-obv-colormap-manager"
;;    "gl-obv-draw"
;;    "gl-obv-mouse-utilities"
;;    "gl-obv-mouse"
;;    ))

;; ;;; P-list of module symbols and filenames.  Note that filename can be
;; ;;; a single file or a list of files.  Directory defaults to
;; ;;; *lisp-source-path*.

;; (defvar *obvius-module-plist*
;;       (append
;;        #+LispView
;;        `(:lispview-windows ,*obvius-lispview-files*
;; 	 :lispview-control-panel "lv-control-panel")
;;        #+Irix
;;        `(:gl-ffi ,*gl-ffi-files*
;; 	 :gl-windows ,*obvius-gl-files*)
      
;;        (list :pictures *obvius-picture-files*
;; 	     :color '("color-image" "color-picture")
;; 	     :tiff '("tiff-ffi" "tiffio")
;; 	     :contour-plot "contour-plot"
;; 	     :matrix '("svd" "qr" "regress" "row-ops")
;; 	     :statistics '("gaussian-noise" "numerical-recipes" "statistics")
;; 	     :stepit "stepit"
;; 	     :simplex "simplex"
;; 	     :pyramid '("gaussian-pyramid" "pyramid")
;; 	     :steer '("steer" "steer-pyramid")
;; 	     :psychophysics '("gamma" "psychophysics" "psychophysics-analysis")
;; 	     ;;***   :kalman
;; 	     ;;***   :Canny
;; 	     ;;***   :HIPS  ;Mike Landy's image processing package
;; 	     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Foreign (C) libraries.

;;; Runs a "make" on the foreign code.  Returns a value which indicates 
;;; whether the libraries were remade and need to be loaded again.
;;; this results in new libraries and executables which are placed at 
;;; their respective places.

;; (defun update-C-libraries ()
;;   (let ((dir (LCL:pwd)))
;;     (LCL:cd *c-source-path*)
;;     (multiple-value-bind (x y return-value)
;; 	#+HP  (LCL:run-program "make" :arguments '("-f" "Makefile-hp" "all"))
;; 	#+Vax (LCL:run-program "make" :arguments '("-f" "Makefile-vax"))
;; 	#+Irix (LCL:run-program "make" :arguments '("-f" "Makefile-sgi"))
;; 	#+(and Sun Sparc) (LCL:run-program "make" :arguments '("-f" "Makefile-sun4"))
;; 	#+(and Sun (not Sparc)) (LCL:run-program "make" :arguments '("-f" "Makefile-sun3"))
;;       (declare (ignore x y))
;;       (LCL:cd dir)
;;       (not (= return-value 0)))))

;; ;;; This is put on the *initialization-functions* list to be executed at startup.
;; ;;; See lucid-ffi.lisp.
;; (defun load-C-libraries ()
;;   (let ((dir (LCL:pwd))
;; 	(libs (if (probe-file "/dev/fpa") 
;; 		  (list "fpaobv.a" "-lm" "-lc")
;; 		  #+Sun (list "obv.a" "-lm" "-lc")
;; 		  #+Irix (list "obv.a" "-lsun" "-lm" "-lc" "-lgl" "-lfm" "-lX11")
;; 		  )))
;;     (LCL:cd *binary-path*)
;;     (LCL:load-foreign-files nil libs)
;;     (LCL:cd dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; [THO] this is mainly done by asdf
;;;; [THO] this will not be present in future releases
;;;; Loading and Running OBVIUSf starting from an empty Lisp World.

;;; Starting from an empty lisp world, this loads all of the obvius code,
;;; compiling when necessary, and then initializes the system.
(defun run-obvius ()
  (load-obvius)				;load code, compiling if necessary
  ;;(optimize-clos-calls)
  (initialize-obvius))			;initialize obvius

;;; This function loads all of the obvius lisp code, compiling when
;;; necessary.  It also compiles the C code into a library which will
;;; be loaded by initialize-obvius at run-time.  It does not
;;; initialize OBVIUS!
(defun load-obvius ()
  (format t "~%;;; Loading OBVIUS system files ...~%")
  (unless (cl-fad:directory-exists-p *binary-path*)
    (with-umask #o000			;make directory unprotected     
      (ensure-directories-exist obv::*binary-path*)))
  ;;(update-C-libraries)
  ;;(mapc 'obv-compile-load *obvius-system-files*)
  ;; (let ((site-init (merge-pathnames "lucid-site-init.lisp" *obvius-directory-path*)))
  ;;   (if (probe-file site-init)
  ;; 	(load site-init)
  ;; 	(warn "Cannot find file ~S" site-init)))
  (format t ";;; Done loading OBVIUS system files.~%~%"))

;;; Load files from the patches directory with names of the form
;;; patch-<num>.lisp in numerical order (according to <num>), starting
;;; with the one designated by the variable *starting-patch-file*.
;;; Increments this variable to be one more than the last patch number
;;; loaded.  *** wasteful re-parsing here....
;; (defun load-obvius-patches ()
;;   (declare (special *obvius-directory-path* *source-file-suffix*
;; 		    *starting-patch-file*))
;;   (let* ((dir (pathname (concatenate 'string *obvius-directory-path* "patches/")))
;; 	 (prefix "patch-")
;; 	 (len (length prefix))
;; 	 (patch-files (directory (merge-pathnames dir *source-file-suffix*)))
;; 	 (*redefinition-action* nil))	;get rid of redefinition-warnings
;;     (declare (special *redefinition-action*))
;;     ;; delete filenames that are not of the form "patch-<num>.lisp", where
;;     ;; <num> is greater than *starting-patch-file*.
;;     (setq patch-files
;; 	  (delete-if-not
;; 	   #'(lambda (name &aux num)
;; 	       (and (> (length name) len)
;; 		    (string= name prefix :end1 len)
;; 		    (setq num (parse-integer name :start len :junk-allowed t))
;; 		    (>= num *starting-patch-file*)))
;; 	   patch-files
;; 	   :key #'pathname-name))
;;     ;; sort files according to <num>:
;;     (setq patch-files
;; 	  (sort patch-files #'<
;; 		:key #'(lambda (p)
;; 			 (parse-integer (pathname-name p) :start len :junk-allowed t))))
;;     ;; Compile and load the patch files:
;;     (dolist (file patch-files)
;;       (obv-compile-load file)
;;       (setq *starting-patch-file*
;; 	    (1+ (parse-integer (pathname-name file) :start len :junk-allowed t))))
;;     *starting-patch-file*))

;; (defun compile-obvius-modules ()
;;   (loop for plist = *obvius-module-plist* then (cddr plist)
;; 	until (null plist)
;; 	for files = (cadr plist)
;; 	do
;; 	(if (consp files)
;; 	    (mapc #'(lambda (f) (obv-compile f)) files)
;; 	    (obv-compile files))))

;; (defun optimize-clos-calls ()
;;   (format t "~%;;; Compiling CLOS generic-functions and dispatch code ...~%")
;;   (with-compiler-optimizations *obvius-compiler-optimizations*
;;     (lcl::precompile-generic-functions)
;;     ;; ** Used to give lucid::STRUCTURE-IS-STANDARD-OBJECT-P not bound errors:
;;     (lcl::compile-all-dispatch-code)))

(defvar site-patch-filename
  #+Sun "site-patches-sun.lisp"
  #+Irix "site-patches-sgi.lisp")

;;; This function must be called to initialize obvius.  It loads patch
;;; files that are not already loaded, loads the foreign (C)
;;; libraries, initializes the window system, loads the user's startup
;;; file and starts the OBVIUS read-eval-print loop (repl).  It is
;;; called by run-obvius (if running from an empty world) and
;;; startup-internal (if running from a world containing OBVIUS).
;;; NOTE: patches and the window-init files are loaded BEFORE
;;; initializing the window system.  We load the C libraries at run
;;; time because otherwise it is hard to patch changes to the C code.
;;; *** Should use the lisp function user-homedir-pathname instead of
;;; "~/".
(defun initialize-obvius ()
  (format t "~%;;; >>>---  Initializing OBVIUS version ~A  ---<<<~%~%" *obvius-version*)
;;  (load-C-libraries)   ;*** should this be done as an initialization from lucid-ffi.lisp?
;;  (load-obvius-patches)
  ;; (let ((site-patches (merge-pathnames site-patch-filename *obvius-directory-path*)))
  ;;   (if (probe-file site-patches)
  ;; 	(obv-compile-load site-patches)
  ;; 	(warn "Cannot find file ~S" site-patches)))
  ;; (cond ((probe-file "~/.obvius-windows") (load "~/.obvius-windows"))
  ;; 	((probe-file "~/obvius-window-init.lisp") (load "~/obvius-window-init.lisp"))
  ;; 	(t
  ;; 	 (load (merge-pathnames
  ;; 		(merge-pathnames "lucid-window-init" *obvius-directory-path*) ".lisp"))))
  (run-initialization-functions)	;window system initializations
  ;; (push-onto-eval-queue
  ;;  (cond ((probe-file "~/.obvius") '(load "~/.obvius"))
  ;; 	 ((probe-file "~/obvius-init.lisp") '(load "~/obvius-init.lisp"))
  ;; 	 (t '(load (merge-pathnames
  ;; 		    (merge-pathnames "lucid-obvius-init" *obvius-directory-path*) ".lisp")))))
  ;; (push-onto-eval-queue '(run-initialization-functions)) ;Other initializations
  ;;(repl)
  )

;;; Code in various files puts functions (or symbols) on this list.
;;; They will be eval'ed by initialize-obvius at startup.
;;; *** should probably do this with a lexical closure (hide the list).
(defvar *initialization-functions* nil)

;;; Run the functions on the *initialization-functions* list in
;;; reverse order (assume that they were added to the list using
;;; push or pushnew).
(defun run-initialization-functions ()
  (let (list-o-functions)
    ;; Do this to make sure multiple processes don't bash...
    (psetq list-o-functions *initialization-functions*
	   *initialization-functions* nil)
    (mapc #'(lambda (func) (funcall func)) (nreverse list-o-functions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Building and saving a Lisp World containing OBVIUS.

;;; Save a lisp-world containing obvius.  Note that existing patch
;;; files are loaded before saving the world.  Also compiles all
;;; modules, so that they can be quickly loaded at runtime.
;; (defun make-obvius (&optional filename)
;;   (unless filename
;;     (setq filename (format nil "~Aobvius-~A" *obvius-image-path* *obvius-version*)))
;;   (load-obvius)
;;   (load-obvius-patches)
;;   (optimize-clos-calls)
;;   (save-lisp-world filename)
;;   (load-C-libraries)
;;   (compile-obvius-modules))

;; ;;; This function saves the current lisp world into the given pathname.
;; ;;; *** get rid of the magic numbers!
;; (defun save-lisp-world (pathname)
;;   (in-package 'user)			;make sure it's saved in 'user
;;   (format t "~%;;; ---- Saving lisp executable ~A ---- ;;;" pathname)
;;   (when (probe-file pathname)
;;     (format t "~%;;; ---- It will overwrite the previous executable ---- ;;;"))
;;   (if (y-or-n-p "~%;;; Do you want to continue? ")
;;       (lcl::disksave pathname
;; 		     :full-gc t
;; 		     :reserved-free-segments 128  ;8 Meg for static heaps
;; 		     :dynamic-free-segments  128   ;8 Meg for dynamic space
;; 		     :verbose T)
;;       (format t "~%;;; ---- ABORTED ---- ;;;")))

	
;;;; Define Lisp Startup hook to initialize obvius:

(defvar cl-user::*enter-top-level-hook* nil)
;;(defvar *pre-obvius-start-hook* cl-user::*enter-top-level-hook*)
;;(setq user::*enter-top-level-hook* 'obvius-top-level-hook)

;;; Set hook so that initialize-obvius is called automatically when
;;; the lisp world is loaded.  Note that the function bound to
;;; *enter-top-level-hook* must be compiled to work properly.
;; (defun obvius-top-level-hook ()
;;   (when *pre-obvius-start-hook*
;;     (funcall *pre-obvius-start-hook*))
;;   (initialize-obvius))

;;(unless (compiled-function-p (symbol-function 'obvius-top-level-hook))
;;  (compile 'obvius-top-level-hook))

