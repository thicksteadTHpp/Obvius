;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: glfw-nk-dispatch.lisp
;;;  Author: THO
;;;  Description: 
;;;  Creation Date: 12/19
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;   The main idea is of this file is shamelessly stolen
;;;   from Pavel Korolevs clutz package   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl:in-package :obv)


;;(defparameter *default-display* nil)

(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter *nk-glfw-double-click-lo* 0.02)
(defparameter *nk-glfw-double-click-hi* 0.2)
(defparameter *NK-GLFW-TEXT-MAX* 256)
;;this is for accessing glfw. parameters as if they were c-structs
;; so we can use glfw.width as var name but it is really an accessor function
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *glfw* nil)) ;;the current glfw class


(defvar *in-render-loop* nil) ;;global voar to check if a call to render is made from inside the render-loop
;; or from outside. if called from outside we can not use open gl calls and we don't have the nk-context



(defmacro def-glfw-class (name superclasses  slot-descriptions)
  (flet ((glfw-accessor (name)
	   (intern (concatenate 'string "GLFW." (string-upcase (symbol-name name))))))

    `(progn 
       (def-simple-class ,name ,superclasses ,slot-descriptions)
       ,@(loop
	    for form in slot-descriptions
	    for slot-name = (if (listp form) (car form) form) 
	    collect `(define-symbol-macro ,(glfw-accessor slot-name) (slot-value *glfw* ',slot-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The glfw dispatcher holds all glfw specific needs
;; responsible for opening a window naming it and getting opengl context
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-glfw-class glfw-dispatcher ()
  ((window :accessor %window-of)
   (width :initform *window-width*)
   (height :initform *window-height*)
   (opengl-version :initarg :opengl-version :initform '(3 3))
   (window-title :initarg :window-title :initform "Eye Of Obvius")
   (depth-bits :initarg :depth-bits :initform 24)
   (v-sync :initarg :v-sync :initform t :reader v-sync-enabled-p)
   (status :accessor %status-of :type (member :active :initializing :destroyed) :initform :destroyed)))


;; (defmethod (setf %status) ((new-status (eql :destroyed)) (this glfw-dispatcher))
;;   (setf (slot-value this 'status) new-status))


;; (defmethod (setf %status) ((new-status (eql :active)) (this glfw-dispatcher))
;;   (setf (slot-value this 'status) new-status))

;; (defmethod (setf %status) ((new-status T) (this glfw-dispatcher))
;;   (warn "Don't know how to set staus for GLFW-dispatcher to: ~a" new-status))


(defmethod settable-parameters ((class-name (eql 'glfw-dispatcher)))
  (append '(window-title) (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; nk dispatcher holds all fields for nk to work
;; this is similar as in the glfw3 examples from nuklear.h 
;; additionally holds the render queue to communicate between
;; the repl thread and the display (glfw-nk) thread
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-glfw-class glfw-nk-dispatcher (glfw-dispatcher)
  (nk-context
   (nk-renderer :initform nil)
   (pixel-ratio :initform 0f0)
   (display-width :initform 0)
   (display-height :initform 0)
   ogl
   ;;atlas           ;; now in font-helper
   font-helper       ;;used for determining the width of drawn (rendered) text
   fb_scale
   (text :initform (make-array *nk-glfw-text-max* :fill-pointer 0 :element-type 'base-char :initial-element #\SPACE))
   (text-len :initform 0)
   scroll      ;;is a foreign vec2
   last-button-click
   is-double-click-down
   double-click-pos  ;;is foreign vec2
   (background-color :initform nil) ;;(claw:calloc '(:struct (%nk:colorf)))))
   ;;[THO] take this out (nk-functions :accessor nk-functions :initform #+SBCL (make-hash-table :synchronized T) #-SBCL (make-hash-table))
   (render-queue :reader render-queue :initform (make-instance 'chanl:bounded-channel :size 8)) ;;channel to send functions to the render process
   ))


(defmethod settable-parameters ((class-name (eql 'glfw-nk-dispatcher)))
  (append '(background-color) (call-next-method)))


(defmethod is-active-p ((disp glfw-dispatcher))
  (not (claw:wrapper-null-p (%window-of disp))))
;;  (eq (%status-of disp) :active))

(defmethod is-active-p ((disp glfw-nk-dispatcher))
  (and (not (null (slot-value disp 'nk-context))) (call-next-method)))


(defmethod is-in-wait-state ((disp glfw-nk-dispatcher))
  (with-slots (status) disp
    (or (eq status :initializing)
	(eq status :destroying))))

;;this function allocates foreign memory - be sure to free it after use
(defun make-vec2 (x y)
  (claw:c-let ((v (:struct (%nk:vec2))))
    (setf (v :x) (float x 1.0)
	  (v :y) (float y 1.0))
    v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; disptatcher init is most-specific-last so that the glfw dispatcher
;; which is the superclass is initialized first. we could have made the
;; same with around methods but we keep the two init methods together
;; so we use mop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; this is most-specific last because we want at first to initialize the
;; glfw3 window and then then nuklear (nk) stuff
(defgeneric init-dispatcher (dispatcher)
  (:method-combination progn :most-specific-last)
  (:method progn ((disp glfw-dispatcher))
    (unless (is-active-p disp)
      (setf (%status-of disp) :initializing)
      (vom:info "[init-dispatcher] glfw3")
      (with-slots (window opengl-version width height window-title depth-bits) disp
	(when (= (%glfw:init) 0)
	  (error "Failed to init GLFW"))
	(glfw:with-window-hints ((%glfw:+context-version-major+ (first opengl-version))
				 (%glfw:+context-version-minor+ (second opengl-version))
				 (%glfw:+opengl-profile+ %glfw:+opengl-core-profile+)
				 (%glfw:+opengl-forward-compat+ %glfw:+true+)
				 (%glfw:+depth-bits+ depth-bits)
				 (%glfw:+stencil-bits+ 8))
	  (claw:c-let ((win %glfw:window :from (%glfw:create-window width height window-title nil nil)))
	    (when (claw:wrapper-null-p win)
	      (%glfw:terminate)
	      (error "Failed to create GLFW window"))
	    (%glfw:make-context-current win)
	    (glad:init)
	    (when (v-sync-enabled-p disp)
	      (%glfw:swap-interval 1))
	    (setf window win)
	    (%glfw:set-scroll-callback win (claw:callback 'scroll-cb))
	    (%glfw:set-char-callback win (claw:callback 'char-cb))
	    (%glfw:set-mouse-button-callback win (claw:callback 'mouse-cb))
	    ;;(setf (%status-of disp) :active)
	    (vom:info "...[init-dispatcher] ready init glfw window window= ~a" win))))))
  (:method progn ((this glfw-nk-dispatcher))
    (unless (is-active-p this)
      (vom:info "...[init-dispatcher] nuklear")
      (with-slots (font-helper double-click-pos is-double-click-down last-button-click ogl scroll fb_scale background-color nk-context nk-renderer) this
	(unless double-click-pos
	  (setf double-click-pos (make-vec2 0 0)))
	(unless scroll
	  (setf scroll (make-vec2 0 0)))
	(unless fb_scale
	  (setf fb_scale (make-vec2 1 1)))
	(unless background-color
	  (setf background-color (claw:calloc '(:struct (%nk:colorf)))))
	;; (unless atlas
	;;   (setf atlas (claw:calloc '(:struct (%nk:font-atlas)))))
	;;set up font bakery
	(vom:info "...[init-dispatcher] set up font bakery")
	(when font-helper
	  (destroy font-helper :dispatcher this))
	(setf font-helper (make-font-helper))
	(bake-nk-default-font font-helper)
	;;init nk-context
	(claw:c-let ((color-v (:struct (%nk:colorf)) :from background-color))
	  (setf nk-renderer (nk:make-renderer)
		nk-context (nk:make-context (font-helper-font-handle font-helper))
		ogl (nuklear::nk-renderer-handle nk-renderer)
		(color-v :r) 0.10f0
		(color-v :g) 0.18f0
		(color-v :b) 0.24f0
		(color-v :a) 1f0
		last-button-click 0
		is-double-click-down %nk:+false+
		*glfw* this)))
;;	(%nk:style-set-font nk-context (nk:renderer-font nk-renderer)))
      (vom:info "...[init-dispatcher] nuklear ... done")
      (setf (%status-of this) :active))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; when destroying the dispatcher take care of freeing all foreign pointers
;; this time most-specific-first so the nk-dispatcher is decommisioned first
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;when destroying the dispatcher we destroy the nk stuff at first
;; and then the glfw window related things
(defgeneric destroy-dispatcher (dispatcher)
  (:method-combination progn :most-specific-first)
  (:method progn ((disp glfw-dispatcher))
    (when (is-active-p disp)
      (vom:info "[destroy-dispatcher] glfw")
      (with-slots (window render-queue) disp
	;;empty the render queue
	(vom:info "[destroy-dispatcher] glfw: emptying render-queue: ~a" render-queue)
	(loop until (chanl:recv-blocks-p render-queue) do
	  (funcall (chanl:recv render-queue :blockp nil) disp))
	(%glfw:destroy-window window)
	(%glfw:terminate)
	(setf window nil))
      (setf (%status-of disp) :destroyed)))
  (:method progn ((this glfw-nk-dispatcher))
    (when (is-active-p this)
      (setf (%status-of this) :destroying)
      (vom:info "[destroy-dispatcher] nuklear")
      ;;[THO] take this out (clear-nk-render-loop this)
      (with-slots (font-helper scroll ogl double-click-pos fb_scale nk-context nk-renderer background-color) this
	(destroy font-helper :dispatcher this)
	(nk:destroy-context nk-context)
	(nk:destroy-renderer nk-renderer)
	(claw:free ogl)
	(claw:free scroll)
	(claw:free double-click-pos)
	(claw:free fb_scale)
	(setf scroll nil
	      double-click-pos nil
	      fb_scale nil)
	(claw:free background-color)
	(setf *glfw* nil)))))


;; we have to decide whether the window was closed outside the repl
;; or we called destroy inside the repl
;; so we check whether the render loop has stopped and window should close
;; if not send window-should-close to the glfw-window
(defmethod destroy :around ((disp glfw-nk-dispatcher) &key silent suppress-error &allow-other-keys)
  (declare (ignore silent suppress-error))
  (vom:info "[destroy] GLFW dispatcher :around")
  (if (= 0 (%glfw:window-should-close (%window-of disp)))
      (progn
	
	(vom:info "[destroy] called from outside the render loop")
	(%glfw:set-window-should-close (%window-of disp) 1))
      ;; after window-should-close is siganlled destroy is called
      ;; again from the end of the render-loop
      (progn
	(vom:info "[destroy] called from end of render loop")
	(call-next-method)
	(destroy-dispatcher disp))))

;;init the dispatcher in this thread, so pane creation etc is blocked
;; otherwise the pane functions have no working screen reference
(defmethod activate-glfw-dispatcher ((dispatcher glfw-nk-dispatcher))
  (vom:info "[activate-glfw-dispatcher] ~a" dispatcher)
  ;; (handler-case (init-dispatcher dispatcher)
  ;;   (error (c)
  ;;     (vom:error "[ERROR] initializing glfw-nk-dispatcher ~a" c)
  ;;     (destroy dispatcher)))  ;; don' know if this works
  (let ((cv (bt:make-condition-variable))
	(lock (bt:make-lock)))
	
    (run-masked-in-main-thread :func (lambda ()
				       (nk-loop-01 dispatcher cv))
			       :blocking nil)
    ;;wait until the dispatcher is initialized
    (bt:with-lock-held (lock)
      (bt:condition-wait cv lock))
    (vom:info "[activate-glfw-dispatcher] ... done")))
  


(defmethod send-to ((disp glfw-nk-dispatcher) val)
  (chanl:send (render-queue disp) val))


;;macro for gl  calls in the thread the dispathcer is running
(defmacro in-gl-thread-of (GLFW-dispatcher &body body)
  `(send-to ,GLFW-dispatcher (lambda (disp)
			       (declare (ignore disp))
			       ,@body)))

(defmacro in-gl-thread-of* ((disp) GLFW-dispatcher &body body)
  `(send-to ,GLFW-dispatcher (lambda (,disp)
			       ,@body)))



(defmethod destroy-texture ((disp glfw-nk-dispatcher) (id fixnum))
  (in-gl-thread-of disp
    (vom:info "[destroy] texture: ~d" id)
    (gl:delete-texture id)))



;;[THO] not needed the disp is already active
;; (defmethod activate-glfw-dispatcher :after ((disp glfw-dispatcher))
;;   (setf (%status-of disp) :active))


(defun start-event-loop (dispatcher)
  (unwind-protect
     (progn
       (init-dispatcher dispatcher)
       (let ((window (%window-of dispatcher)))
	 (loop while (= (%glfw:window-should-close window) 0) do
	      (%glfw:poll-events)
	      (render dispatcher)
	      (%glfw:swap-buffers window))))
       (destroy dispatcher)))


(defun nk-loop (dispatcher &optional lock  cv)
  (unwind-protect
       (progn
	 (init-dispatcher dispatcher)
	 (vom:info "~&*glfw* is: ~a" *glfw*)
	 (with-slots (window render-queue background-color nk-context nk-renderer) dispatcher
	   (claw:c-let ((bg (:struct (%nk:colorf)) :from background-color))
	     (loop while (= (%glfw:window-should-close window) 0) do

	       ;; 1.) input handling
	       (%glfw:poll-events)
	       (register-nk-inputs nk-context window)
	       ;; 2.) the nk widgets
	       (gl:clear-color (bg :r) (bg :g) (bg :b) 1f0)
	       (gl:clear :color-buffer-bit)
	       (claw:c-with ((rect (:struct (%nk:rect))))
		 (let ((val (%nk:begin nk-context "Hello Nuklear" (%nk:rect rect 50f0 50f0 230f0 250f0)
				       (logior %nk:+window-border+ %nk:+window-movable+ %nk:+window-scalable+
					       %nk:+window-minimizable+ %nk:+window-title+))))
		   (unless (= val 0)
		     (%nk:layout-row-static nk-context 30f0 80 1)
		     (unless (= (%nk:button-label nk-context "button") 0)
		       (format T "~&button pressed")))))
	       (%nk:end nk-context)

	       ;;get somthing from the render queue

	       (unless (chanl:recv-blocks-p render-queue)
		 (funcall (chanl:recv render-queue :blockp nil) dispatcher))
	       
	       (do-nk-functions dispatcher nk-context)
	       
	       ;; 3.) nuklear render functions and clear
	       (gl:viewport 0 0 glfw.width glfw.height)	   
	       (nk:render-nuklear nk-renderer nk-context glfw.width glfw.height)
	       (%nk:clear nk-context)
	       (%glfw:swap-buffers window)))))
    (destroy dispatcher)))  ;;call the destroy method so that the screen object will also be destroyed


;;cv is condition var to signal that init is ready
(defun nk-loop-01 (dispatcher &optional cv)
  (unwind-protect
       (progn
	 (init-dispatcher dispatcher)
	 (vom:info "~&*glfw* is: ~a" *glfw*)
	 (with-slots (window render-queue background-color nk-context nk-renderer) dispatcher
	   (claw:c-let ((bg (:struct (%nk:colorf)) :from background-color))
	     (when cv
	       (bt:condition-notify cv))
	     
	     (let ((*in-render-loop* T))
	       (declare (special *in-render-loop*))  
	       (loop while (= (%glfw:window-should-close window) 0) do

	       ;; 1.) input handling
		 (%glfw:poll-events)
		 (register-nk-inputs nk-context window)
		 ;; 2.) the nk widgets
		 (gl:clear-color (bg :r) (bg :g) (bg :b) 1f0)
		 (gl:clear :color-buffer-bit)

		 ;; (claw:c-with ((rect (:struct (%nk:rect))))
		 ;; 	 (let ((val (%nk:begin nk-context "Hello Nuklear" (%nk:rect rect 50f0 50f0 230f0 250f0)
		 ;; 			       (logior %nk:+window-border+ %nk:+window-movable+ %nk:+window-scalable+
		 ;; 				       %nk:+window-minimizable+ %nk:+window-title+))))
		 ;; 	   (unless (= val 0)
		 ;; 	     (%nk:layout-row-static nk-context 30f0 80 1)
		 ;; 	     (unless (= (%nk:button-label nk-context "button") 0)
		 ;; 	       (format T "~&button pressed")))))
		 ;; (%nk:end nk-context)

		 ;;get somthing from the render queue

		 (unless (chanl:recv-blocks-p render-queue)
		   (funcall (chanl:recv render-queue :blockp nil) dispatcher))
		 
		 (%display-panes dispatcher nk-context)
		 
		 ;; 3.) nuklear render functions and clear
		 (gl:viewport 0 0 glfw.width glfw.height)	   
		 (nk:render-nuklear nk-renderer nk-context glfw.width glfw.height)
		 (%nk:clear nk-context)
		 (%glfw:swap-buffers window))))))
    ;;call the destroy method so that the screen object will also be destroyed
    (destroy dispatcher)
    ))  



(defun demo01 ()
  (let ((dispatcher (make-instance 'glfw-nk-dispatcher)))
    (unwind-protect
	 (progn
	   (init-dispatcher dispatcher)
	   (vom:info "~&*glfw* is: ~a" *glfw*)
	   (with-slots (window background-color nk-context nk-renderer) dispatcher
	     (claw:c-let ((bg (:struct (%nk:colorf)) :from background-color))
	       (loop while (= (%glfw:window-should-close window) 0) do
		    (%glfw:poll-events)
		    (register-nk-inputs nk-context window) 
		    (gl:clear-color (bg :r) (bg :g) (bg :b) 1f0)
		    (gl:clear :color-buffer-bit)
		    (claw:c-with ((rect (:struct (%nk:rect))))
		      (let ((val (%nk:begin nk-context "Hello Nuklear" (%nk:rect rect 50f0 50f0 230f0 250f0)
					    (logior %nk:+window-border+ %nk:+window-movable+ %nk:+window-scalable+
						    %nk:+window-minimizable+ %nk:+window-title+))))
			(unless (= val 0)
			  (%nk:layout-row-static nk-context 30f0 80 1)
			  (unless (= (%nk:button-label nk-context "button") 0)
			    (format T "~&button pressed")))))
		    (%nk:end nk-context)

		    (gl:viewport 0 0 glfw.width glfw.height)	   
		    (nk:render-nuklear nk-renderer nk-context glfw.width glfw.height)
		    (%nk:clear nk-context)
		    (%glfw:swap-buffers window)))))
      (destroy-dispatcher dispatcher))))



(defun run-masked-in-main-thread (&rest args &key (blocking t) (func #'demo01))
  (let ((standard-output *standard-output*))
    (flet ((run-masked ()
             (claw:with-float-traps-masked ()
               (handler-bind ((t (lambda (e)
                                   (format standard-output "~&Unhandled error:~&~A" e)
                                   (break "~A" e))))
                 (funcall func)))))
      (trivial-main-thread:call-in-main-thread #'run-masked :blocking blocking))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the render loop
;;; is called from within a a valid nk-context and renders all the
;;  nk -windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-window-id (&optional (title "Hello Obvius"))
  (timestamped-temp-file (str+ title "_XXXX")))


;; (defun push-to-nk-render-loop (dispatcher id form)
;;   (setf (gethash id (nk-functions dispatcher)) form))

;; (defun remove-from-nk-render-loop (dispatcher id)
;;   (remhash id (nk-functions dispatcher)))


;; (defun clear-nk-render-loop (dispatcher)
;;   (clrhash (nk-functions dispatcher)))



;; (defun do-nk-functions (disp nk-context)
;;   (maphash (lambda (k v)
;; 	     (declare (ignore k))
;; 	     (funcall v nk-context))
;; 	   (nk-functions disp)))


;;get the context
;; (defun nk-context (dispatcher)
;;   (slot-value dispatcher 'nk-context))




;; (defun push-test ()
;;   (push-to-nk-render-loop (first *screen-list*)
;; 			  "win01"
;; 			  (lambda (ctx)
;; 			    (claw:c-with ((rect (:struct (%nk:rect))))
;; 			      (let ((val (%nk:begin-titled ctx "win01" "Hello Nuklear 2" (%nk:rect rect 150f0 50f0 230f0 250f0)
;; 							   (logior %nk:+window-border+ %nk:+window-movable+ %nk:+window-scalable+
;; 								   %nk:+window-minimizable+ %nk:+window-title+))))
;; 				(unless (= val 0)
;; 				  (%nk:layout-row-static ctx 30f0 80 1)
;; 				  (unless (= (%nk:button-label ctx "button") 0)
;; 				    (format T "~&button 2 pressed~&")))))
;; 			    (%nk:end ctx))))



;; GLFW specific callbacks

(glfw:define-char-callback char-cb (win codepoint)
  (declare (ignore win))
  (when (< glfw.text-len *NK-GLFW-TEXT-MAX*)
    (vector-push (code-char codepoint) glfw.text)))
    


(glfw:define-scroll-callback scroll-cb (window xoff yoff)
  (declare (ignore window))
  (claw:c-let ((scroll (:struct (%nk:vec2)) :from glfw.scroll))
    (setf (scroll :x) (float (+ (scroll :x) xoff))
	  (scroll :y) (float (+ (scroll :y) yoff)))))



(glfw:define-mouse-button-callback mouse-cb (window button action mod-keys)
  (declare (ignore mod-keys))
  (claw:c-with ((x :double)
		(y :double))
    (when (= button %glfw:+mouse-button-left+)
      (%glfw:get-cursor-pos window (x &) (y &))
;;      (format t "~&[mouse callback] cursor pos x: ~a y: ~a" x y)
      (if (= action %glfw:+press+)
	  (let ((dt (- (%glfw:get-time)
		       glfw.last-button-click)))
	    (when (and (> dt *nk-glfw-double-click-lo*)
		       (< dt *nk-glfw-double-click-hi*))
	      (setf glfw.is-double-click-down %nk:+true+)
	      (claw:c-val ((v (:struct (%nk:vec2)) glfw.double-click-pos))
		;;(format T "~&[mouse callback] double click pos: ~a" glfw.double-click-pos)
		(setf (v :x) (float x 1.0f0)
		      (v :y) (float y 1.0f0)))
	  (setf glfw.is-double-click-down %nk:+false+)))))))




(defun window-size (glfw-window)
  (if glfw-window
    (claw:c-with ((height :int)
		  (width :int))
      (%glfw:get-window-size glfw-window (width &) (height &))
      (values width height))
    (values 0 0)))


(defun frame-buffer-size (glfw-window)
  (if glfw-window
    (claw:c-with ((height :int)
		(width :int))
      (%glfw:get-framebuffer-size glfw-window (width &) (height &))
      (values width height))
    (values 0 0)))



(defun cursor-position (glfw-window)
  (claw:c-with ((x :int)
		(y  :int))
    (%glfw:get-cursor-pos glfw-window (x &) (y &))
    (values  x y)))


(defun mouse-button-state (dispatcher button)
  (let ((button (ecase button
                  (:left %glfw:+mouse-button-left+)
                  (:right %glfw:+mouse-button-right+)
                  (:middle %glfw:+mouse-button-middle+))))
    (alexandria:switch ((%glfw:get-mouse-button (%window-of dispatcher) button) :test #'=)
      (%glfw:+press+ :pressed)
      (%glfw:+release+ :released)
      (%glfw:+repeat+ :repeating))))

;;return  0 for false and 1 for true
(defmacro nk-cif (test)
  `(if ,test %nk:+true+ %nk:+false+))


(define-symbol-macro nk-left-mouse-button-down-p (nk-cif (= (%glfw:get-mouse-button win %glfw:+mouse-button-left+) %glfw:+press+)))
(define-symbol-macro nk-right-mouse-button-down-p (nk-cif (= (%glfw:get-mouse-button win %glfw:+mouse-button-right+) %glfw:+press+)))
(define-symbol-macro left-mouse-button-down-p (= (%glfw:get-mouse-button win %glfw:+mouse-button-left+) %glfw:+press+))
(define-symbol-macro right-mouse-button-down-p (= (%glfw:get-mouse-button win %glfw:+mouse-button-right+) %glfw:+press+))



(defun register-nk-inputs (ctx win)
  (and win
       ctx
       (multiple-value-bind (ww wh) (window-size win)
	 (multiple-value-bind (dw dh) (frame-buffer-size win)
	   (setf glfw.width ww
		 glfw.height wh
		 glfw.display-width dw
		 glfw.display-height dh)
	   (claw:c-val ((sc (:struct (%nk:vec2)) glfw.fb_scale))
	     (setf (sc :x) (float (/ dw ww) 1.0)
	   	   (sc :y) (float (/ dh wh) 1.0)))

	   (%nk:input-begin ctx)
	   (dotimes (i glfw.text-len)
	     (%nk:input-unicode ctx (aref glfw.text i)))

	   ;;TODO mouse grabbing

	   ;;[TODO] input keys
	   (claw:c-with ((x :double)
			 (y :double))
	     (%glfw:get-cursor-pos win (x &) (y &))
	     (%nk:input-motion ctx (round x) (round y))

	   ;;mouse grabbing again
	     ;; (if (= (%glfw:get-mouse-button win %glfw:+mouse-button-left+) %glfw:+press+)
	     ;;  	 (format t "~&mouse button pressed at x: ~d y: ~d status: ~a" x y nk-left-mouse-button-down-p))

	     ;;(when right-mouse-button-down-p (format t "~&right mouse button at x: ~d y: ~d" x y))
	     
	     (%nk:input-button ctx %nk:+button-left+ (floor x) (floor y) nk-left-mouse-button-down-p)

	     (%nk:input-scroll ctx glfw.scroll))
	   (%nk:input-end ctx)
	   (setf glfw.text-len 0
		 (fill-pointer glfw.text) 0)
	   (claw:c-val ((sc (:struct (%nk:vec2)) glfw.scroll))
	     (setf (sc :x) 0.0
		   (sc :y) 0.0))))))
