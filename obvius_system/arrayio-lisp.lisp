;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: fileio-lisp.lisp
;;;  Author: tho
;;;  Description: reading and writing arrays to files using lisp routines
;;                as replacement for c routines
;;;  Creation Date: summer 2016
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obv)


;; (defun fread (buf  size num file)
;;   (with-open-file (byte-stream file :direction :input :element-type '(unsigned-byte 8))
;;     (loop for i = 0 then (1+ i)
;;        for b = (handler-case (read-byte byte-stream t nil)
;; 		 (end-of-file () (progn
;; 				   (vom:info "[fread] end of file reached")
;; 				   (return i))))   ;;we are one byte ahead of fil end so we don#t need to add 1 
;;        until (>= i (1- num))
;;        do (setf (row-major-aref buf i) b)
;;        finally (return (1+ i))))) ;; we need to add 1 because we started at zero



;;returns a local anaphoric function
;,captures file buf num size form context
;;for reading n bytes
(defmacro local-n-byte-reader (element-reader)
  `(loop for i = 0 then (1+ i)
       for b = (handler-case ,element-reader
		 (end-of-file () (progn
				   (vom:info "[fread] end of file reached")
				   (return i))))   ;;we are one byte ahead of fil end so we don#t need to add 1 
       until (>= i (1- num))
       do (setf (row-major-aref buf i) b)
       finally (return (1+ i))));; we need to add 1 because we started at zero


;;creates a nested dpb of read-byte
(defmacro times-read-byte (n stream eof-value)
  (labels ((rec (n)
	     (if (= 0 n)
		 `(dpb (read-byte ,stream nil ,eof-value) (byte 8 0) 0)
		 `(dpb (read-byte ,stream nil ,eof-value) (byte 8 ,(* n 8))
		       ,(rec (1- n))))))
    `(dpb (read-byte ,stream t) (byte 8 ,(* (1- n) 8)) ,(rec (- n 2)))))


;;lisp replacement for c fread
(defun fread (buf size num file)
  (assert (> num 0))
  (assert (> size 0))
  (with-open-file (byte-stream file :direction :input :element-type '(unsigned-byte 8))
    (flet ((read-1-byte ()
	     (local-n-byte-reader (read-byte byte-stream t nil)))
	   (read-2-bytes ()
	     (local-n-byte-reader (dpb (read-byte byte-stream t nil) (byte 8 8)
				       (dpb (read-byte byte-stream nil 0) (byte 8 0) 0))))
	   (read-3-bytes () ;;maybe useful for rgb values
	     (local-n-byte-reader (times-read-byte 3 byte-stream 0)))
	   (read-4-bytes ()
	     (local-n-byte-reader (times-read-byte 4 byte-stream 0)))
	   (read-8-bytes ()
	     (local-n-byte-reader (times-read-byte 8 byte-stream 0)))
	   (read-n-bytes ()
	     (local-n-byte-reader (loop with res = (dpb (read-byte byte-stream t) (byte 8 (* (1- size) 8)) 0)
				     for s from (- size 2) downto 0
				     for byte-pos = (* s 8)
				     do (dpb (read-byte byte-stream nil 0) (byte 8 byte-pos) res)
				     finally (return res)))))
      (case size
	(1 (read-1-byte))
	(2 (read-2-bytes))
	(3 (read-3-bytes))
	(4 (read-4-bytes))
	(8 (read-8-bytes))
	(otherwise (read-n-bytes))))))
      
		 
