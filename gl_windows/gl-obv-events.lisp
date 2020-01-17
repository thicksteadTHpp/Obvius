;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-events.lisp
;;;  Author: Patrick C. Teo
;;;  Description: OBVIUS-GL event handler
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Window Interests
;;;
;;; Each window will have an interests slot which specify
;;; the events that it is interested in.  The event handler
;;; will try to match actual events with those interests and
;;; if there is a match, it will invoke a method with the
;;; appropriate event info packet.
;;;
;;; There are three types of interests: mouse interests,
;;; window manager interests and keyboard interests.  For
;;; mouse interests, the state of modifier keys may also
;;; be specified as an additional constraint.  The mouse
;;; event generates a GL-mouse-event packet which contains
;;; the x,y coordinates and specifications of the event
;;; that generated it.   The coordinates are window-aligned
;;; and the y-coordinate has been adjusted to go from top
;;; to bottom.
;;;
;;; Window manager interests include window closing, quiting
;;; refreshing and focus changes.  Currently focus changes
;;; are handled internally and only refresh events are
;;; taken care of.  Each Window event generate a GL-window-event
;;; which simply contains that event.
;;;
;;; Keyboard interests currently include only the hot keys.
;;; It is used primarily to indicate a change of modifier
;;; keys.  Each keyboard event generates a GL-keyboard-event
;;; whicih again simply contains that event.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-simple-class GL-interest () ())
(def-simple-class GL-event () ())

(defparameter *default-window-devices*
  `(,GL:leftctrlkey
    ,GL:leftshiftkey
    ,GL:leftaltkey
    ,GL:leftmouse
    ,GL:middlemouse
    ,GL:rightmouse
    ,GL:redraw
    ;,GL:winshut
    ,GL:winquit
    ,GL:inputchange
    ,GL:keybd))

;;; Generic receive-event catch-all.
(defmethod receive-event ((window GL-window) (interest GL-interest) (event GL-event)))

;;; Generic check interests function.
(defmethod check-interests ((window GL-window) device status)
  (with-slots (interests) window
    (dolist (interest interests)
      (check-interest interest window device status))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mouse interests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-simple-class GL-mouse-interest (GL-interest)
  ((mouse-interest-gesture :initform nil)))

(def-simple-class GL-mouse-event (GL-event)
  (mouse-event-x
   mouse-event-y
   mouse-event-gesture))

(defparameter *mouse-modifier-name-to-device-map*
  `((:ctrl    . ,GL:leftctrlkey)
    (:shift   . ,GL:leftshiftkey)
    (:meta    . ,GL:leftaltkey)
    (:left    . ,GL:leftmouse)
    (:middle  . ,GL:middlemouse)
    (:right   . ,GL:rightmouse)))

(defparameter *mouse-action-name-to-device-map*
  `((:left    . ,GL:leftmouse)
    (:middle  . ,GL:middlemouse)
    (:right   . ,GL:rightmouse)
    (:move-x  . ,GL:mousex)
    (:move-y  . ,GL:mousey)))

(defun mouse-modifier-name-to-device (modifier-name)
  (cdr (assoc modifier-name *mouse-modifier-name-to-device-map*)))

(defun device-to-mouse-modifier-name (device)
  (car (rassoc device *mouse-modifier-name-to-device-map*)))

(defun mouse-modifier-state-to-device-status (modifier-state)
  (if (eq modifier-state :down) GL:true
      (if (eq modifier-state :up) GL:false nil)))

(defun device-status-to-mouse-modifier-state (status)
  (if (= status GL:true) :down
      (if (= status GL:false) :up nil)))

(defun mouse-action-name-to-device (modifier-name)
  (cdr (assoc modifier-name *mouse-action-name-to-device-map*)))

(defun device-to-mouse-action-name (device)
  (car (rassoc device *mouse-action-name-to-device-map*)))

(defmacro mouse-action-state-to-device-status (action-state)
  `(mouse-modifier-state-to-device-status ,action-state))

(defmacro device-status-to-mouse-action-state (status)
  `(device-status-to-mouse-modifier-state ,status))

(defmacro get-device-status (device) `(GL:with-GL-lock (GL:getbutton ,device)))

(defvar *mouse-move-x-p*)
(defvar *mouse-move-y-p*)
(defvar *mouse-move-x*)
(defvar *mouse-move-y*)

(defvar *f-origin-x* (LCL:with-static-area (make-array '(1) :element-type '(signed-byte 32) :initial-element 0)))
(defvar *f-origin-y* (LCL:with-static-area (make-array '(1) :element-type '(signed-byte 32) :initial-element 0)))

(defun get-window-origin (window)
  (let ((origin-x 0) (origin-y 0))
    (GL:with-GL-lock
      (GL:winset (wid window))
      (GL:getorigin *f-origin-x* *f-origin-y*))
    (setq origin-x (aref *f-origin-x* 0)
	  origin-y (aref *f-origin-y* 0))
    (values origin-x origin-y)))


;;; Make a mouse gesture given current device/status
(defun make-mouse-gesture (device status)
  (if (null (device-to-mouse-action-name device)) nil
      `(,(make-mouse-modifier-gesture)
	,(make-mouse-action-gesture device status))))

(defun make-mouse-modifier-gesture ()
  (mapcar #'(lambda (single-map)
	      (list (car single-map)
		    (device-status-to-mouse-modifier-state (get-device-status (cdr single-map)))))
	  *mouse-modifier-name-to-device-map*))

#|
(defun make-mouse-action-gesture (device status)
  (cond ((or (eql device GL:mousex) (eql device GL:mousey))
	 `(,(device-to-mouse-action-name device) :down))
	(t
	 `(,(device-to-mouse-action-name device)
	   ,(device-status-to-mouse-action-state status)))))
|#
(defun make-mouse-action-gesture (device status)
  (cond ((eql device GL:mousex)
	 (setf *mouse-move-x-p* t *mouse-move-x* status)
	 `(,(device-to-mouse-action-name device) :down))
	((eql device GL:mousey)
	 (setf *mouse-move-y-p* t *mouse-move-y* status)	 
	 `(,(device-to-mouse-action-name device) :down))	 
	(t
	 (setf *mouse-move-x-p* nil *mouse-move-y-p* nil)
	 `(,(device-to-mouse-action-name device)
	   ,(device-status-to-mouse-action-state status)))))

;;; Match mouse event with gesture and first interest
(defmethod match-mouse-event-spec (gesture (window GL-window))
  (with-slots (interests) window
    (match-mouse-event-spec-helper gesture interests)))

(defmethod match-mouse-event-spec-helper (gesture interest-class-list)
  (if (null interest-class-list) nil
      (let ((result (match-mouse-interest-with-gesture gesture (car interest-class-list))))
	(if (not (eq result 'fail)) (car interest-class-list)
	    (match-mouse-event-spec-helper gesture (cdr interest-class-list))))))
		    
;;; Checks interest

(defmethod check-interest ((interest-class GL-mouse-interest) (window GL-window) device status)
  (let ((gesture (make-mouse-gesture device status)))
    (if (null gesture) 'fail
	(let ((result (match-mouse-interest-with-gesture gesture interest-class)))
	  (unless (eq result 'fail)
	    (let ((mouse-x (if nil *mouse-move-x* (GL:with-GL-lock (GL:getvaluator GL:mousex))))
		  (mouse-y (if nil *mouse-move-y* (GL:with-GL-lock (GL:getvaluator GL:mousey))))
		  (origin-x 0) (origin-y 0))
	      (multiple-value-setq (origin-x origin-y) (get-window-origin window))
	      (receive-event window interest-class
			     (make-instance 'GL-mouse-event
					    :mouse-event-x (- mouse-x origin-x)
					    :mouse-event-y (- (height window) (1+ (- mouse-y origin-y)))
					    :mouse-event-gesture gesture))))))))


;;; Checks if the modifier matches the modifier in the gesture
;;; (used by drag-interest in gl-obv-mouse-utilities.lisp)
(defmethod check-modifier (modifiers gesture)
  (match-mouse-modifier (first gesture) modifiers))

;;; Matches the mouse interest with the given gesture
(defmethod match-mouse-interest-with-gesture (gesture (interest-class GL-mouse-interest))
  (with-slots (mouse-interest-gesture) interest-class
    (let ((mouse-action-result
	   (match-mouse-action (second gesture) (second mouse-interest-gesture))))
      (if (eq mouse-action-result 'fail) 'fail
	  (match-mouse-modifier (first gesture) (first mouse-interest-gesture))))))

(defmethod match-mouse-interest-with-gesture (gesture interest)
  (declare (ignore gesture interest))
  'fail)

;;; Matches the mouse action interest
(defun match-mouse-action (gesture interest)
  (cond ((symbolp interest)
	 (match-primitive-mouse-action gesture `(,interest :down)))
	((consp interest)
	 (cond ((eq 'or (car interest))
		(match-disjunctive-mouse-action gesture (cdr interest)))
	       ((= (length interest) 2)
		(match-primitive-mouse-action gesture interest))
	       (t 'fail)))
	(t 'fail)))

(defun match-disjunctive-mouse-action (gesture interest-list)
  (if (null interest-list) 'fail
      (let ((result (match-mouse-action gesture (car interest-list))))
	(if (not (eq result 'fail)) result
	    (match-disjunctive-mouse-action gesture (cdr interest-list))))))

(defun match-primitive-mouse-action (gesture interest)
  (if (or (equal (second interest) '(or :up :down))
	  (equal (second interest) '(or :down :up)))
      (if (eq (first gesture) (first interest)) gesture 'fail)
      (if (equal gesture interest) gesture 'fail)))


;;; Matches the mouse modifier interest
(defun match-mouse-modifier (gesture-list interest)
  (cond ((null interest) gesture-list)
	((symbolp interest)
	 (match-primitive-modifier gesture-list `(,interest :down)))
	((consp interest)
	 (cond ((eq 'or (car interest))
		(match-disjunctive-mouse-modifier gesture-list (cdr interest)))
	       ((eq 'and (car interest))
		(match-conjunctive-mouse-modifier gesture-list (cdr interest)))
	       ((= (length interest) 2)
		(match-primitive-modifier gesture-list interest))
	       (t 'fail)))
	(t 'fail)))

(defun match-disjunctive-mouse-modifier (gesture-list interest-list)
  (if (null interest-list) 'fail
      (let ((result (match-mouse-modifier gesture-list (car interest-list))))
	(if (eq result 'fail)
	    (match-disjunctive-mouse-modifier gesture-list (cdr interest-list))
	    gesture-list))))

(defun match-conjunctive-mouse-modifier (gesture-list interest-list)
  (if (null interest-list) gesture-list
      (let ((result (match-mouse-modifier gesture-list (car interest-list))))
	(if (eq result 'fail) 'fail
	    (match-conjunctive-mouse-modifier gesture-list (cdr interest-list))))))

(defun match-primitive-modifier (gesture-list interest)
  (if (null gesture-list) 'fail
      (let ((result (match-primitive-modifier-helper (car gesture-list) interest)))
	(if (not (eq result 'fail)) result
	    (match-primitive-modifier (cdr gesture-list) interest)))))

(defun match-primitive-modifier-helper (gesture interest)
  (if (or (equal (second interest) '(or :up :down))
	  (equal (second interest) '(or :down :up)))
      (if (eq (first gesture) (first interest)) gesture 'fail)
      (if (equal gesture interest) gesture 'fail)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Window manager interests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-simple-class GL-window-manager-interest (GL-interest)
  ((window-manager-interest-gesture :initform nil)))

(def-simple-class GL-window-manager-event (GL-event)
  (window-manager-event-gesture))

(defmethod make-window-manager-gesture ((window GL-window) device status)
  (cond ((= device GL:winshut) :close)
	((= device GL:winquit) :quit)
	((= device GL:redraw)  :redraw)
	((= device GL:inputchange)
	 (cond ((= status (wid window)) :enter)
	       ((= status 0) :exit)
	       (t nil)))
	(t nil)))

(defmethod check-interest ((interest-class GL-window-manager-interest) (window GL-window) device status)
  (let* ((gesture (make-window-manager-gesture window device status)))
    (if (null gesture) 'fail
	(let ((result (match-window-manager-interest gesture (window-manager-interest-gesture interest-class))))
	  (unless (eq result 'fail)
	    (receive-event window interest-class
			   (make-instance 'GL-window-manager-event
					  :window-manager-event-gesture result)))))))

(defun match-window-manager-interest (gesture interest)
  (cond ((symbolp interest)
	 (match-primitive-window-manager-interest gesture interest))
	((and (consp interest) (eq 'or (car interest)))
	 (match-disjunctive-window-manager-interest gesture (cdr interest) ))
	(t 'fail)))

(defun match-disjunctive-window-manager-interest (gesture interest-list)
  (if (null interest-list) 'fail
      (let ((result (match-window-manager-interest gesture (car interest-list))))
	(if (not (eq result 'fail)) result
	    (match-disjunctive-window-manager-interest gesture (cdr interest-list))))))

(defun match-primitive-window-manager-interest (gesture interest)
  (if (eq gesture interest) (list gesture) 'fail))
      
	    



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keyboard interests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-simple-class GL-keyboard-interest (GL-interest)
  ((keyboard-interest-gesture :initform nil)))

(def-simple-class GL-keyboard-event (GL-event)
  (keyboard-event-gesture))

(defparameter *key-set-to-devices-map*
  `((:modifier . (,GL:leftctrlkey ,GL:leftshiftkey ,GL:leftaltkey))))

(defun device-to-key-sets (device)
  (do ((key-sets nil) (map *key-set-to-devices-map* (cdr map)))
      ((null map) key-sets)
    (when (member device (cdar map) :test #'=)
      (setq key-sets (cons (caar map) key-sets)))))
      
(defun make-keyboard-key-set-gesture (device) (device-to-key-sets device))

(defmacro make-keyboard-transition-gesture (status)
  `(device-status-to-mouse-action-state ,status))

(defun make-keyboard-gesture (device status)
  (let ((key-sets (make-keyboard-key-set-gesture device)))
    (if (null key-sets) nil
	(let ((transition (make-keyboard-transition-gesture status)))
	  (mapcar #'(lambda (key-set) (list key-set transition)) key-sets)))))

(defmethod check-interest ((interest-class GL-keyboard-interest) (window GL-window) device status)
  (let ((gesture-list (make-keyboard-gesture device status)))
    (if (null gesture-list) 'fail
	(let ((result (match-keyboard-interest-with-gesture gesture-list (keyboard-interest-gesture interest-class))))
	  (unless (eq result 'fail)
	    (receive-event window interest-class
			   (make-instance 'GL-keyboard-event
					  :keyboard-event-gesture result)))))))

(defun match-keyboard-interest-with-gesture (gesture-list interest)
  (cond ((symbolp interest)
	 (match-primitive-keyboard-interest gesture-list `(,interest :down)))
	((consp interest)
	 (cond ((eq 'or (car interest))
		(match-disjunctive-keyboard-interest gesture-list (cdr interest)))
	       ((= (length interest) 2)
		(match-primitive-keyboard-interest gesture-list interest))
	       (t 'fail)))
	(t 'fail)))

(defun match-disjunctive-keyboard-interest (gesture-list interest-list)
  (if (null interest-list) 'fail
      (let ((result (match-keyboard-interest-with-gesture gesture-list (car interest-list))))
	(if (not (eq result 'fail)) result
	    (match-disjunctive-keyboard-interest gesture-list (cdr interest-list))))))

(defun match-primitive-keyboard-interest (gesture-list interest)
  (if (null gesture-list) 'fail
      (let ((result (match-primitive-keyboard-interest-helper (car gesture-list) interest)))
	(if (not (eq result 'fail)) result
	    (match-primitive-keyboard-interest (cdr gesture-list) interest)))))

(defun match-primitive-keyboard-interest-helper (gesture interest)
  (if (or (equal (second interest) '(or :up :down))
	  (equal (second interest) '(or :down :up)))
      (if (eq (first gesture) (first interest)) gesture 'fail)
      (if (equal gesture interest) gesture 'fail)))
  
    
