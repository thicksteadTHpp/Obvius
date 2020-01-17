;;;; obvius01.asd

(asdf:defsystem #:obvius01
  :description "Describe obvius01 here"
  :author "Your Name <your.name@example.com>"
  :license "MIT"
  :serial t
  :depends-on (:zpng :ffa :cl-fad :cffi :static-vectors :trivial-garbage :closer-mop :vom)
  :components ((:file "package")
               (:file "obvius01")
	       (:module obvius_system
		:components ((:file "lcl-compat")
			     (:file "lucid-hacks")
			     (:file "obv_repl")
			     (:file "lucid-clos-extensions")
			     (:file "lucid-ffi")
			     (:file "misc")
			     (:file "memory")
			     (:file "generic-fns")
			     (:file "array-ops")
			     (:file "arrayio-c")
			     (:file "matrix")
			     (:file "list-ops")
			     ;;(:file "arrayio-lisp")
			     ))
	       (:file "site_paths")
	       (:file "defsys")		
	       (:module obvius_viewable
	       		:components ((:file "viewable")
	       			     (:file "viewable-classes")
	       			     (:file "viewable-matrix")
	       			     (:file "viewable-sequence")
	       			     (:file "discrete-function")))
	       (:module obvius_image
			:components ((:file "lucid-image-loops")
				     (:file "image")
				     (:file "bit-image")
				     (:file "image-matrix")
				     (:file "image-sequence")
				     (:file "image-pair")
				     (:file "synth")))
	       (:module obvius_operations
			:components ((:file "generic-ops")
				     (:file "user-macros")
				     (:file "imops")
				     (:file "warp")
				     (:file "fft")
				     (:file "fileio")
				     (:file "filter")))
	       (:module obvius_pictures
			:components ((:file "picture")
				     (:file "pane")
				     (:file "picture-classes")
				     (:file "gray")
				     (:file "drawing")
				     (:file "coord-xforms")
				     (:file "graph")
				     (:file "surface-plot")
				     (:file "flipbook")
				     (:file "pasteup")
				     (:file "overlay")
				     (:file "hardcopy")
				     ))
	       (:file "obv_print")
	       ))
;;; missing lisp view gl-ffi and obvius-gl



;;; original definition of the obvius system
;;; as found in lucid-defsys.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; obvius eye for now being dependent on glfw3 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(asdf:defsystem #:obvius-eye
  :description "Preliminary Viewport for obvius 4.0"
  :author "Your Name <your.name@example.com>"
  :license "MIT"
  :pathname "obvius_eye/"
  :serial t
  :depends-on (:obvius01
	       :chanl
	       :cl-colors
	       :cl-opengl
	       :alexandria
	       :cffi
	       :ffa
	       :claw
	       :nuklear-blob
	       :bodge-nuklear
	       :glfw-blob
	       :bodge-glfw
	       :glad-blob
	       :bodge-glad
	       :trivial-main-thread
	       :cl-soil)
  :components ((:file "obv_eye_package")
	       (:file "gl-obv-lucid-ffi")
	       (:file "glfw-nk-font-helper")
	       (:file "glfw-nk-dispatch")
	       (:file "glfw-nk-screen")
	       (:file "glfw-nk-screen-8-bit")
	       (:file "glfw-nk-screen-24-bit")
	       (:file "glfw-nk-pane")
	       ;;(:file "glfw-nk-window")
	       (:file "glfw-nk-blt")
	       (:file "glfw-nk-render")
	       (:file "glfw-nk-draw")
	       (:file "glfw-nk-content")
	       (:file "obv_eye_post")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; obvius modules as defined in lucid-defsys                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem #:obvius-modules
  :description "Modules for obvius legacy 3.0"
  :author "Your Name <your.name@example.com>"
  :license "MIT"
  :serial t
  :depends-on (:obvius01)
  :components ((:module color
	       		:components ((:file "color-image")
	       			     (:file "color-picture")))
	       (:module tiff
	       		:components ((:file "tiff-ffi")
	       			     (:file "tiff-io")))
	       (:module contour-plot
	       		:components ((:file "contour-plot")))
	       (:module matrix
			:components ((:file "svd")
				     (:file "qr")
				     (:file "regress")
				     (:file "row-ops")))
	       (:module statistics
			:components ((:file "gaussian-noise")
				     (:file "numerical-recipes")
				     (:file "statistics")))
	       (:module stepit
			:components  ((:file "stepit")))
	       
	       (:module simplex
			:components ((:file "simplex")))
	       (:module pyramid
			:components ((:file "gaussian-pyramid")
				     (:file "pyramid")))
	       (:module steer
			:components ((:file "steer")
				     (:file "steer-pyramid")))
	       (:module psychophysics
			:components ((:file "gamma")
				     (:file "psychophysics")
				     (:file "psychophysics-analysis")))))
	       			    




