;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-dispatch.lisp
;;;  Author: Patrick C. Teo
;;;  Description: Raw GL global event handler
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)


;;; The dispatcher in addition to serving as a global event loop registers/removes
;;; devices from its list of devices to block on.  

(def-simple-class GL-dispatcher ()
  ((process :initform nil)
   (active-devices :initform nil)
   (current-window :initform (GL:with-GL-lock (GL:winget)))))


(defvar *foreign-data* (LCL:with-static-area (make-array '(1) :element-type '(signed-byte 16) :initial-element 0)))

(defgeneric receive-dispatch (dispatch window device data)
  (:documentation
   "Receive a dispatch from the dispatcher"))

(defmethod gl-event-loop ((dispatch GL-dispatcher))
  (with-slots (active-devices current-window) dispatch
    (let ((data 0) (device 0))
      (loop
	;;
	;; We cannot use GL:with-GL-lock because it does a process
	;; block but process-wait executes within in the scheduler
	;; and you can't block within the scheduler.
        ;;
	(LCL:process-wait "Waiting for event"
			  #'(lambda () (LCL:with-scheduling-inhibited
					   (and (or (GL:process-owns-GL-lock-p) (GL:nobody-owns-GL-lock-p))
						(plusp (GL:qtest))))))
	(GL:with-GL-lock (setf device (GL:qread *foreign-data*)))
	(setf data (aref *foreign-data* 0))
      
	(cond ((= device GL:inputchange)
	       (if (zerop data)
		   (progn (receive-dispatch dispatch current-window device data)
			  (setf current-window data))
		   (progn (setf current-window data)
			  (receive-dispatch dispatch current-window device data))))
	       
	      ((or (= device GL:redraw)
		   (= device GL:redrawiconic)
		   (= device GL:winshut)
		   (= device GL:winquit))
	       (receive-dispatch dispatch data device data))

	      ((assoc device active-devices)
	       (receive-dispatch dispatch current-window device data))
	      (t
	       ;(format t "Warning:: Unknown or inactive device ~d with data ~d~%" device data)
	       (GL:with-GL-lock (GL:qreset))
	       nil)
	      )))))


;;;
;;; Adds a device to the screen device list to be monitored.
;;;
(defmethod add-device ((dispatch GL-dispatcher) device)
  (with-slots (active-devices) dispatch
    (let ((pair (assoc device active-devices)))
      (if (consp pair)
	  (rplacd (assoc device active-devices) (1+ (cdr pair)))
	  (progn
	    (push (cons device 1) active-devices)
	    (GL:with-GL-lock (GL:qdevice device)))))))


;;;
;;; Removes a current device from the screen device list.
;;;
(defmethod remove-device ((dispatch GL-dispatcher) device)
  (with-slots (active-devices) dispatch
    (let ((pair (assoc device active-devices)))
      (if (consp pair)
	  (if (> (cdr pair) 1)
	      (rplacd (assoc device active-devices) (1- (cdr pair)))
	      (progn
		(setf active-devices (remove device active-devices :test #'= :key #'car))
		(GL:with-GL-lock (GL:unqdevice device))))
	  (error "No such device registered.")))))



(defmethod activate-GL-dispatcher ((dispatch GL-dispatcher))
  (with-slots (process active-devices) dispatch
    (when (null process)
      (GL:with-GL-lock
	(GL:qreset)
	(GL:noise GL:mousex 2)
	(GL:noise GL:mousey 2)
	(dolist (dev active-devices) (GL:qdevice (car dev))))
      (setf process (LCL:make-process :name "GL-dispatch"
				      :function #'gl-event-loop
				      :args `(,dispatch))))))


(defmethod deactivate-GL-dispatcher ((dispatch GL-dispatcher))
  (with-slots (process) dispatch
    (unless (null process)
      (LCL:kill-process process)
      (setf process nil))))
