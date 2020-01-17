;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: fileio-c.lisp
;;;  Author: sokolov
;;;  Description: reading and writing arrays to files using c routines
;;;  Creation Date: summer '88
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)
(export '())

;;; code to read and write ascii data

(defun ascii-array-read (array path)
  (with-open-file (data-stream (pathname path)
			       :direction :input)
    (loop for y from 0 below (y-dim array) do
	  (loop for x from 0 below (x-dim array) do
		(setf (aref array y x) (read data-stream)))))
  array)

(defun ascii-array-write (array path)
  (with-open-file (data-stream (pathname path)
			       :direction :output)
    (loop for y from 0 below (y-dim array) do
	  (loop for x from 0 below (x-dim array) do
		(format data-stream "~a " (aref array y x)))
	  (format data-stream "~%")))
  array)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code to read and write byte and float data via C calls

;;; skip-bytes keyword is here so that you can skip file header
(defun array-read (a path &key (skip-bytes 0))
  (let ((fp (fopen path "r"))
	(num 0))
    (when (cffi:null-pointer-p fp)
      (error "Can't open ~s for reading." path))
    (when (plusp skip-bytes)
      (let ((tmp-arr (make-array skip-bytes :element-type '(unsigned-byte 8))))
	(setq num (fread tmp-arr 1 skip-bytes fp))
	(when (< num skip-bytes)
	  (format t "~%Warning: incomplete read from ~s.~%" path))))
    (setq num
	  (fread a (sizeof (array-element-type a)) (array-total-size a) fp))
    (when (< num (array-total-size a))
      (format t "~%Warning: incomplete read from ~s, only read ~d ~ss.~%" 
	      path num (array-element-type a)))
    (fclose fp))
  a)



(defun array-write (a path &key header)
  (let ((fp (fopen path "w"))
	(num 0))
    (when (= fp 0)
      (error "Can't open ~s for writing." path))
    (when header
      (setq num (fprintf fp header))
      (when (< num (length header))
	(format t "~%Warning: incomplete write to ~s, only wrote ~d ~ss.~%" 
		path num (array-element-type header))))
    (setq num
	  (fwrite a (sizeof (array-element-type a)) (array-total-size a) fp))
    (when (< num (array-total-size a))
      (format t "~%Warning: incomplete write to ~s, only wrote ~d ~ss.~%" 
	      path num (array-element-type a)))
    (fclose fp))
  a)


#|
;;; Old version cannot write a text header
(defun array-write (a path)
  (let ((fp (fopen path "w"))
	(num 0))
    (when (= fp 0)
      (error "Can't open ~s for writing." path))
    (setq num
	  (fwrite a (sizeof (array-element-type a)) (array-total-size a) fp))
    (when (< num (array-total-size a))
      (format t "~%Warning: incomplete write to ~s, only wrote ~d ~ss.~%" 
	      path num (array-element-type a)))
    (fclose fp))
  a)
|#

(defun float-array-read (a path &key (skip-bytes 0))
  (array-read a path :skip-bytes skip-bytes))

(defun float-array-write (a path)
  (array-write a path))

(defun array-read-and-swap (a path)
  (let ((fp (fopen path "r"))
	(num 0))
    (when (= fp 0)
      (error "Can't open ~s for reading." path))
    (setq num
	  (fread a (sizeof (array-element-type a)) (array-total-size a) fp))
    (when (< num (array-total-size a))
      (format t "~%Warning: incomplete read from ~s, only read ~d ~ss.~%" 
	      path num (array-element-type a)))
    (fclose fp)
    (byteswap a num))
  a)

;;; Note: this only works on 32-bit arrays.
(defun array-swap-and-write (a path)
  (let ((fp (fopen path "w"))
	(temp (similar a))
	(num 0))
    (when (= fp 0)
      (error "Can't open ~s for writing." path))
    (swapcopy a temp (array-total-size a))
    (setq num
	  (fwrite temp (sizeof (array-element-type a)) (array-total-size a) fp))
    (when (< num (array-total-size a))
      (format t "~%Warning: incomplete write to ~s, only wrote ~d ~ss.~%" 
	      path num (array-element-type a)))
    (fclose fp))
  a)

;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
