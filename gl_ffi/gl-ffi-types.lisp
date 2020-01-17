;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  gl-types.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package 'gl)


;;; Record the C equivalents of all the internal types
;;; These are the only ones, we'll use.  The rest are either disallowed
;;; or seem to have problems.
(record-c-type :character "char")
(record-c-type :signed-8bit "char")
(record-c-type :unsigned-32bit "unsigned int")
(record-c-type :signed-32bit "int")
(record-c-type :double-float "double")

;;; Synonym types for all of the primitive C types
(def-exported-foreign-synonym-type "char" :character)
(def-exported-foreign-synonym-type "unsigned_char" :unsigned-8bit)
(def-exported-foreign-synonym-type "short"  :signed-16bit)
(def-exported-foreign-synonym-type "unsigned_short" :unsigned-16bit)
(def-exported-foreign-synonym-type "int" :signed-32bit)
(def-exported-foreign-synonym-type "unsigned_int" :unsigned-32bit)
(def-exported-foreign-synonym-type "long" :signed-32bit)
(def-exported-foreign-synonym-type "unsigned_long" :unsigned-32bit)
(def-exported-foreign-synonym-type "float" :single-float)
(def-exported-foreign-synonym-type "double"  :double-float)
(def-exported-foreign-synonym-type "void" :signed-32bit)
(def-exported-foreign-synonym-type "short_int" short)
(def-exported-foreign-synonym-type "long_int" long)
(def-exported-foreign-synonym-type "unsigned" unsigned-int)
(def-exported-foreign-synonym-type "long_float" double)

;;; Some of the standard C type synonyms from /usr/include/sys/types.h
(def-exported-foreign-synonym-type "caddr_t" int)
(def-exported-foreign-synonym-type "u_char" unsigned-char)
(def-exported-foreign-synonym-type "u_short" unsigned-short)
(def-exported-foreign-synonym-type "u_int" unsigned-int)
(def-exported-foreign-synonym-type "u_long" unsigned-long)
(def-exported-foreign-synonym-type "fd_mask" long)

;;; GL types
(def-exported-foreign-synonym-type "Byte" u-char)
(def-exported-foreign-synonym-type "Boolean" long)
(def-exported-foreign-synonym-type "String" (:pointer char))

(def-exported-foreign-synonym-type "Angle" short)
(def-exported-foreign-synonym-type "Screencoord" short)
(def-exported-foreign-synonym-type "Scoord" short)
(def-exported-foreign-synonym-type "Icoord" long)
(def-exported-foreign-synonym-type "Coord" float)
(def-exported-foreign-synonym-type "Matrix" (:array float (4 4) :row-major))

(def-exported-foreign-synonym-type "Colorindex" u-short)
(def-exported-foreign-synonym-type "RGBvalue" u-char)

(def-exported-foreign-synonym-type "Device" u-short)

;;; pattern-16-size evalues to
(eval `(def-exported-foreign-synonym-type "Pattern16" (:array u-short (,pattern-16-size))))
(eval `(def-exported-foreign-synonym-type "Pattern32" (:array u-short (,pattern-32-size))))
(eval `(def-exported-foreign-synonym-type "Pattern64" (:array u-short (,pattern-64-size))))

(def-exported-foreign-synonym-type "Linestyle" u-short)
  
(def-exported-foreign-struct "Fontchar"
  (offset :type u-short)
  (w :type Byte)
  (h :type Byte)
  (xoff :type char)
  (yoff :type char)
  (width :type short))

(def-exported-foreign-synonym-type "Object" long)
(def-exported-foreign-synonym-type "Tag" long)
(def-exported-foreign-synonym-type "Offset" long)

;;; typedef void (*__PFV_)();	/* C++ can't handle function ptr prototype */

(def-exported-foreign-struct "GLXconfig"
  (buffer :type int)
  (mode :type int)
  (arg :type int))

;;; obsolete typedefs
(def-exported-foreign-synonym-type "Cursor" (:array u-short (16)))
  






