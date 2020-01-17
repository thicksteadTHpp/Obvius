;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  gl-lock.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package 'gl)

(export '(with-GL-lock process-owns-GL-lock-p nobody-owns-GL-lock-p))

;;; GL Locking
;;;
;;; The GL libraries are not reentrant; as a result, only one LISP process
;;; can run a GL library function at a time.  Applications must wrap any code
;;; that uses GL functions with "with-GL-lock".  Code largely taken
;;; from Lispview.
;;;

(defvar *GL-lock* nil)

(defmacro with-GL-lock (&body body)
  `(let ((lock-already-mine  (process-owns-GL-lock-p)))
    (SYS:interruptions-inhibited-unwind-protect
     (progn
       (unless lock-already-mine
	 (grab-GL-lock))
       ,@body)
     (when (and (not lock-already-mine) (process-owns-GL-lock-p))
       (free-GL-lock)))))


(defmacro without-GL-lock (&body body)
  `(let ((lock-is-mine  (process-owns-GL-lock-p)))
     (when lock-is-mine (free-GL-lock))
     (SYS:interruptions-inhibited-unwind-protect     
      (progn . ,body)
      (when lock-is-mine
	(grab-GL-lock)))))


(defmacro process-owns-GL-lock-p (&optional (process '*current-process*))
  `(eq ,process GL::*GL-lock*))


(defmacro nobody-owns-GL-lock-p ()
  `(null GL::*GL-lock*))

(defmacro grab-GL-lock ()
  `(with-scheduling-inhibited
    (or (if (null *GL-lock*)
	    (setf *GL-lock* *current-process*)) 
	(with-scheduling-allowed
	 (process-wait "Waiting for GL Lock"
		       #'(lambda () (if (null *GL-lock*)
					(setf *GL-lock* *current-process*))))))))
    
(defmacro free-GL-lock ()
  `(setf *GL-lock* nil))


