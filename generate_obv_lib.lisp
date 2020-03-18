;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: generate_obv_lib.lisp
;;;  Author: [THO]
;;;  Description: tries to compile the c-source of obvius
;;;               if that fails downloads a precompiled library
;;;  Creation Date: March '2020
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage #:obv.lib
  (:use #:cl)
  (:export *obvius-lib-path*))

(in-package :obv.lib)


(eval-when (:compile-toplevel :load-toplevel :execute)
  
(defun run (command &rest args &key (output T) )
  (apply #'uiop:run-program command :output output args))


(defun call-cmake ()
  (uiop:run-program "cmake --version" :output T)
  T)

(defmacro command-or-error (command &optional (error-message "~&the following error occured: ~a"))
  `(lambda ()
     (handler-case (prog1 T
		     (run ,command))
       (error (c)
	 (format t ,error-message c)
	 nil))))
       

;;the name of the system
;;added (not darwin) as hinted by lispm
(defparameter *system-name* "obvius")
(defparameter *c-files-dir* "c-source")
(defparameter *build-dir*   "build")
(defparameter *version*     "v3.2.2" "Version string for downloadable libs")
(defparameter *lib-format-string* "libobvius.~a.-~a-~a-~a")
(defparameter *suffix* #+darwin "dylib"
                       #+(and unix (not darwin)) "so"
                       #+windows "dll"
		       )
(defparameter *ext* #+darwin "osx"
                    #+(and unix (not darwin)) "linux"
                    #+windows "windows"
		       "awkward naming convention")

(defparameter *mtype* #+x86 "i686"
	              #+x86-64 "x86_64"
	              #-(or x86 x86-64) "unknown"
		      )
(defparameter *lib-name* (format nil *lib-format-string* *suffix* *mtype* *ext* *version*))

;;download links look like this
;;;https://github.com/thicksteadTHpp/Obvius/releases/download/v3.2.0/libobvius.so.-x86_64-linux-v3.2.0

(defparameter *download-link* "https://github.com/thicksteadTHpp/Obvius/releases/download")


(defparameter *obvius-lib-name* "libobvius")

;; (defun in-fasl-bin-dir (name suffix)
;;   (uiop:merge-pathnames*
;;    (make-pathname :directory
;; 		  (append
;; 		   (substitute
;; 		    :relative
;; 		    :absolute
;; 		    (pathname-directory (asdf:system-source-directory *system-name*))
;; 		    :test #'eq :count 1 :end 1)
;; 		   '("bin"))
;; 		  :name name
;; 		  :type suffix)
;;    uiop:*user-cache*)) 

(defun in-fasl-bin-dir (name suffix)
  (uiop:merge-pathnames*
   (make-pathname :directory (append (pathname-directory (uiop:relativize-pathname-directory (asdf:system-source-directory *system-name*))) '("bin"))
							 
		  :name name
		  :type suffix)
   uiop:*user-cache*)) 



(defparameter *obvius-lib-path* (in-fasl-bin-dir *obvius-lib-name* *suffix*))

;;try to do all filesystem sacces in a osindependent way
;;get the directory for the build
(defparameter *system-source* (asdf:system-source-directory *system-name*))

(defparameter *c-source-dir* (uiop:merge-pathnames* (make-pathname :directory `(:relative ,*c-files-dir*)) *system-source*))

(defparameter *build-dir* (uiop:merge-pathnames* (make-pathname :directory `(:relative ,*build-dir*)) *c-source-dir*))


(defparameter *locally-compiled-lib* (asdf:system-relative-pathname *system-name* (format nil "bin/~a.~a" *obvius-lib-name* *suffix*)))

;;tries to build libobvius local if cmake is present and copies it to bin/ dir in quicklisp dir
;;if this succeeds T is returned otherwise nil
(defun try-build-and-install ()
  (uiop:ensure-all-directories-exist (list *build-dir*))
  (if (and 
       (uiop:call-with-current-directory *build-dir*
					 #-windows(command-or-error "cmake .." "Error during cmake init ~a")
					 #+windows(command-or-error "cmake -G \"MSYS Makefiles\" .." "Error during cmake init ~a")
					 )

       (uiop:call-with-current-directory *build-dir* (command-or-error "cmake --build ." "Error during cmake build ~a"))


       (uiop:call-with-current-directory *build-dir* (command-or-error "cmake --install ." "Error during cmake install ~a")))
      (progn
	(format t "~&[CMAKE] build successful")
	;;under win copy the generated lib out of build dir
	#+windows (uiop:copy-file (uiop:merge-pathnames* (make-pathname :directory '(:relative "lib") :name *obvius-lib-name* :type *suffix*) *build-dir*) *c-source-dir*) 
	(uiop:file-exists-p *locally-compiled-lib*)) ;;check if target file is there
      (progn
	(format t "~&[CMAKE] BUILD or INSTALL FAILED")
	(format t "~&..try downloading precompiled library")
	(format t "~&..this is not recommended .. try recompiling for using specialised hardware")
	nil)))

;;just for testing the download
;;(defun try-build-and-install () nil)

;;v3.2.1
;; (defparameter *file-digest*
;;   #+(and unix (not darwin) x86-64) #(222 84 146 209 55 55 82 239 152 190 236 60 230 132 24 172)
;;   #+(and unix (not darwin) x86) #(222 254 41 230 54 19 192 194 45 226 63 91 151 176 102 244)
;;   #+(and darwin x86-64) #(192 178 18 221 63 147 5 127 25 154 174 207 91 92 203 110)
;;   #+(and darwin x86) #(171 193 239 89 169 56 145 175 95 99 27 12 89 59 40 242)
;;   #+(and windows x86) #(215 130 109 176 212 55 4 40 102 165 82 21 236 195 149 133)
;;   #+(and windows x86-64) #(203 241 197 110 81 220 3 71 166 61 23 198 69 65 140 118)
;;   )

;;v3.2.2
(defparameter *file-digest* 
#+(and darwin x86) #(171 193 239 89 169 56 145 175 95 99 27 12 89 59 40 242)
#+(and darwin x86-64) #(140 186 77 103 113 176 50 204 200 144 153 65 134 41 157
                        1)
#+(and unix (not darwin) x86) #(15 193 164 41 170 232 127 104 19 152 201 168
                                124 152 107 95)
#+(and unix (not darwin) x86-64) #(206 112 131 14 116 225 243 15 28 194 108 47
                                   64 65 249 92)
#+(and windows x86) #(164 168 124 12 19 206 63 89 163 27 225 228 74 252 243 208)
#+(and windows x86-64) #(39 69 25 63 45 97 159 201 29 253 214 83 182 81 74 46))




;; returns T if check is ok 
(defun md5-validiy-check (file digest)
  (declare (type simple-vector digest))
  (let ((file-digest (md5:md5sum-file file))
	(integrity nil))
    (if (/= (length digest) 16)
	(progn
	  (format t "~&[md5] not a valid file digest: ~a" digest)
	  nil)
	(progn
	  (format t "~&[md5] checking file integrity: ~a" file)
	  (setf integrity (loop for n fixnum from 0 below 16
				if (/= (svref digest n) (aref file-digest n))
				  return nil
				finally (return T)
				))
	  (format t "~&[md5] integrity check: ~a" integrity)
	  integrity))))
	  

(defun download-lib ()
  (let ((cmpl-down-link (format nil "~a/~a/~a" *download-link* *version* *lib-name*)))
    (format T "..Try to download precompiled library for ~a ~&...from: ~a " *ext* cmpl-down-link)
    (trivial-download:download cmpl-down-link *obvius-lib-path*)
    (if (md5-validiy-check *obvius-lib-path* *file-digest*)
	T
	(progn
	  (format t "[download] delete (probably) compromised file: ~a" *obvius-lib-path*)
	  (uiop:delete-file-if-exists *obvius-lib-path*)
	  nil))))

;; be careful!!!
(defun clean-up-build-dir ()
  (uiop:delete-directory-tree *build-dir* :validate T :if-does-not-exist :ignore)
  (uiop:delete-file-if-exists *locally-compiled-lib*))



(defun copy-lib-to-fasl-cache-dir ()
  (uiop:ensure-all-directories-exist (list (uiop:pathname-directory-pathname *obvius-lib-path*)))
  (uiop:copy-file *locally-compiled-lib* *obvius-lib-path*))


(defun remove-obvlib ()
  (uiop:delete-file-if-exists *obvius-lib-path*))

    
;;things to do to get a working libobvius
;; 0. check if there is a lib in the fasl path!!! then do nothing
;; 1. check if we can compile locally
;; 2. if we have a locally compiled lib copy it to fasl dir
;; 3. if its not possible to compile
;; 4. try to download a precompiled lib
;; 5  otherwise fail



(cond ((uiop:file-exists-p *obvius-lib-path*) (format t "~&[lib] ...nothing to do. ~a exists." *lib-name*))
      ((try-build-and-install)
       (format t "~&[lib] ..successfully built ~a" *lib-name*)
       (copy-lib-to-fasl-cache-dir)
       (clean-up-build-dir))
      ((download-lib)
       (format t "~&[lib]..successfully downloaded precompiled lib to ~a" *obvius-lib-path*))
      (T (error "Obvius won't work without a c-lib. Try compiling by hand"))))
	 
	 
	


