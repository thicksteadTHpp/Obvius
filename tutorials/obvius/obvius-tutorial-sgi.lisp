
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: obvius-tutorial-sgi.lisp
;;;  Author: Heeger
;;;  Description:
;;;  Creation Date:
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is an obvius tutorial for the implementation of Obvius that
;;; runs on the Silicon Graphics machines.  If you are running on a
;;; sun, you should use obvius-tutorial.lisp instead.  The main
;;; difference is that the control panel, menus, and dialog boxes have
;;; not yet been implemented for the SGI implementation.  But there
;;; are direct function calls that allow you to perform all of these
;;; operations.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file is a tutorial to take new users through some simple
;; examples using OBVIUS.  The purpose is to familiarize new users
;; with the basic functionality of OBVIUS, and with the user
;; interface.

;; Throughout this document, we use the following abbreviations:
;;
;;      M-      meta 
;;      Sh-     shift
;;      C-      control
;;      left    mouse button click
;;      middle  mouse button click
;;      right   mouse button click
;;
;; For example, C-M-left means to click the left mouse button while
;; holding down the control and meta keys.  Note that the standard
;; meta key on a Sun workstation is immediately to the left of the
;; space bar.  On a Sun SPARCstation, this key has a diamond label.

;; The previous section (Getting Started) explained how to set up your
;; environment on a Sun computer in order to run OBVIUS.  The last
;; thing you did was to type "M-x run-obvius" in emacs.  This loaded
;; OBVIUS and loaded your initialization files.  You now have a *lisp*
;; buffer in emacs.  This is an interactive lisp environment that has
;; OBVIUS loaded on top of it.  It is also an emacs buffer, so all of
;; the usual emacs commands (e.g., saving and yanking from the kill
;; rings may be used).

;; Split the emacs window so that both this buffer and the *lisp*
;; buffer are on display (if this is not already true).  You can do
;; this using or by using "C-M-l".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EVALUATING LISP EXPRESSIONS: Click left in front of the OBVIUS>
;; prompt at the end of the *lisp* buffer.  Type a lisp expression
;; like:
;;
;;      (+ 1 2) <cr>
;;
;; Lisp returns 3, and issues a new prompt.  Try typing some other
;; lisp expressions.  

;; Errors: When an error occurs in lisp, the debugger is automatically
;; invoked.  For example, type:
;;
;;      (/ 1 0) <cr>
;;
;; at the OBVIUS> prompt.  Lisp responds by printing:
;;
;;      >>Error: Attempt to divide by zero: (/ 1 0)
;;      /:
;;      Required arg 0 (X): 1
;;      Required arg 1 (Y): 0
;;      :A  0: Abort to Lisp Top Level
;;      -> 
;;
;; You are now in the debugger.  The OBVIUS status line in the control
;; panel should read "entering debugger...".  Note that the menu
;; buttons are disabled when you are in the debugger, as is the
;; auto-display mechanism.  In fact, obvius will completely ignore the
;; mouse while you are in the debugger!  Type ":b <cr>" to see a
;; backtrace of the sequence of functions that were called resulting
;; in the error.  Type ":a <cr>" or "C-c a" to abort from the error
;; and return to the top-level OBVIUS> prompt.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTINUABLE ERRORS: Lisp sometimes gives a choice of what to do
;; when an error occurs.  Typing :a (abort) is usually one of the
;; choices.  Often, you may also type :c that will take some action
;; to recover from the error, and then continue with the computation.
;; Usually, typing :a (abort) {abort} is an OK thing to do.  There
;; are two errors that you should NOT abort from.  These two errors
;; might occur as you go through this tutorial.

;; The first error may occur when OBVIUS runs out of storage space.
;; It looks like this:
;;
;;      >>Error: Not enough static memory space of type SINGLE-FLOAT
;;      OBVIUS::FIND-FREE-ARRAY:
;;      Required arg 0 (HEAP): #<STATIC-HEAP SINGLE-FLOAT 0>
;;      Required arg 1 (MINSIZE): 65536
;;      :C  0: Expand the SINGLE-FLOAT static memory heap by 1048576 elements
;;      :A  1: Abort to Lisp Top Level
;;      -> 

;; When you see an error like this, you should usually type :c.  This
;; will allocate more storage space for OBVIUS, and then continue with
;; the computation.  Obviously, you can't allocate storage
;; indefinitely.  Later in this tutorial, you'll see you to clean up
;; your use of memory.

;; The second error may occur when something goes wrong with the
;; mouse, or with a menu.  In this case, the choices will be:
;;
;;      0: Kill process #<Process Solo Local Event Dispatch 17989CE>
;;      1: Restart process #<Process Solo Local Event Dispatch 17989CE>
;;
;; Notice that :a is not one of the choices.  When faced with a choice
;; like this, you should ALWAYS choose to restart the Event Dispatch
;; process.  Otherwise, all future mouse events will be ignored!  In
;; the case above, you would do this by typing "1 <cr>".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STUCK WAITING FOR THE OBVIUS PROMPT: Sometimes the OBVIUS> prompt
;; will not reappear after returning from the debugger.  Don't just
;; sit there.  Try typing "t" or "nil" (depending on how optimistic
;; you feel).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RE-EVALUATING: Move the cursor onto one of the lisp expressions
;; that you have previously typed (by clicking left with the mouse on
;; the expression, or using C-p, C-n, C-f, C-b).  Now type <cr>.  The
;; lisp expression is copied to the end of the buffer.  You can now
;; re-evaluate it by typing another <cr>.

;; Another way to re-evaluate is to use M-p and M-n at the OBVIUS
;; prompt.  M-p and M-n cycle through the previously evaluated
;; lisp expressions.  Try typing M-p now.  Type M-p again.  Now
;; type M-n.  Edit the expression and type <cr>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EVALUATION FROM ANOTHER BUFFER: You can also evaluate expressions
;; from another emacs buffer that is in lisp mode.  Note that this
;; buffer is in lisp mode (it says so in the Emacs mode-line).  Files
;; with names ending in a .lisp extension are automatically put into
;; lisp mode.  Note, however, that lisp mode is different from
;; lisp-interaction mode.  If you have a buffer that is not in lisp
;; mode, and you want it to be in lisp mode, type "M-x lisp-mode".

;; Now evaluate the following expression by: (1) positioning the
;; cursor (click left) somewhere (anywhere) in the expression, and (2)
;; typing "C-c e".

(+ 1 2)

;; This copies the expression into the *lisp* buffer, evaluates the
;; expression, and the result is printed in the *lisp* buffer.  Emacs
;; chooses the next top-level expression containing or after the point
;; (cursor) to send to lisp.  In emacs, a top-level expression is just
;; one that starts with a left parenthesis against the left margin
;; (i.e., not preceded by any whitespace).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LISP HELP, ARGUMENT LISTS: Type "C-c C-a".  Emacs puts the cursor
;; in the mini-buffer at the bottom of the window and prompts you with
;; "Arglist of CL function:".  Respond by typing "point-operation".
;; This calls the Common Lisp function 'arglist to give you the
;; arguments for the function.  In this case, there aren't many
;; arguments so it comes up in the Emacs mini-buffer.  Longer
;; arg-lists will come up in a new Emacs buffer. You can use this
;; command, "C-c C-a", at any time to get the argument list for a
;; function.  If the emacs cursor is within an expression, the prompt
;; will include the first symbol of that expression (typically the
;; function name) as a default.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LISP HELP, DOCUMENTATION: Now type "C-c C-d".  Emacs prompts you
;; with "CL documentation of:".  Respond by typing "point-operation".
;; This brings up a new Emacs buffer with documentation about the
;; function.  The information is retrieved from Common Lisp using the
;; function 'documentation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LISP HELP, APROPOS: You can type "C-c C-h" when you don't remember
;; a function name.  Enter a substring that you think might be part of
;; the function name (try "filter" as an example).  Emacs will call
;; the Common Lisp function 'apropos to retrieve all symbols
;; containing that substring.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LISP HELP, EDIT SOURCE CODE: Now type "C-c ."  (that is Control-c
;; period).  Emacs prompts you with "Edit CL definition of:".  Respond
;; again by typing "point-operation".  This brings up a new buffer
;; with a list of all of the "point-operation" methods.  Emacs gets
;; these by running the Common Lisp function 'get-source-file.  Use
;; C-n and C-p to move the cursor to different lines.  Position
;; the cursor on the first line where that starts with:
;;
;;      "((METHOD POINT-OPERATION (IMAGE T))..."
;;
;; and type "e".  This brings up another new Emacs buffer with source
;; code for the point-operation method (function) on images.  When you
;; are finished looking at the source code, use the "C-x k" to kill
;; the buffer.  Then click left in this buffer and type "C-M-l" to
;; bring back the *lisp* buffer.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NEW PANES: You should have at least one window that is labeled
;; "empty pane" in the title bar.  This is an OBVIUS pane in which
;; pictures will be displayed.  OBVIUS panes can be moved around and
;; resized like any other X window.  You can make a new pane by using
;; the function {new-pane}.  This function takes keywords to specify
;; the initial location and size for the pane.  The default size is
;; 300x256 pixels (this aspect-ratio makes the graphs look better).
;; If you have only one pane, make a new one now by evaluating the
;; following lisp expression.  Position the cursor on the line below
;; and type "C-c e":

(new-pane)

;; You may position and resize the pane with the mouse as you would
;; with any other X window.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESTROYING PANES: If you are running OpenWindows, you can destroy a
;; pane as usual by clicking right in the title bar and releasing on
;; the quit option.

;; WARNING: If you use the TWM window manager, do not try to quit from
;; an OBVIUS pane using the usual window manager menu; this will kill
;; the entire lisp process.  Instead, select the pane (by clicking the
;; right mouse button in that pane) and evaluate:

(destroy *current-pane*)

;; If you just destroyed all of your panes, a new one will
;; automatically appear.  If you now have only one pane, make a second
;; one using the (new-pane) function.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CURRRENT PANE: There is always one pane that is designated as the
;; (or selected) pane; it is bound to the symbol, *current-pane*.
;; With some window managers, this pane will have a different border
;; color or width.  On others, unfortunately, there will be no
;; distinguishing features.  OBVIUS displays pictures in the current
;; pane.  Evaluating the (new-pane) function sets the current pane
;; to be the new one.  Clicking right in a pane selects it as the
;; current pane.  You can also call the function (next-pane) to
;; select the next pane in the list of panes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MOUSE DOCUMENTATION: Position the mouse in an OBVIUS pane.  The
;; emacs minibuffer provides a brief description of the operation of
;; each of the mouse buttons.  The mouse bindings are dependent on the
;; state of the keyboard modifier keys: control, shift, and meta.
;; Press the control key and notice that the documentation changes.
;; Hold down various combinations of the control, meta, and shift keys
;; to see all of the mouse bindings.  Remember to pay attention to the
;; mouse documentation line to help you keep track of the various
;; mouse bindings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LOADING AN IMAGE: Evaluate the following expression (using C-c e) to load an
;; image and display it:

(load-image (merge-pathnames "images/einstein" obv::*obvius-directory-path*))

;; You may have to type ":c" in answer to the continuable error messages to
;; expand the heaps for image storage space.

;; OBVIUS automatically displays images (and other viewables) that are returned
;; to top level.  The function load-image makes an image, fills it with data
;; from the specified file, and returns that image to the interactive lisp
;; environment.  Then OBVIUS's auto-display mechanism takes over and displays
;; the image in the current pane.

;; Thre is also an emacs key binding for loading images.  Type C-c C-l to
;; emacs.  The emacs minibuffer will prompt you for a pathname.  Enter the
;; pathname of an image datfile.  You may have to ask your system manager for
;; an appropriate pathname.  You may press the tab or space keys, as usual in
;; Emacs, to have Emacs complete the filename for you.  Now hit <cr>.  Emacs
;; will ask you (in the minibuffer) to confirm.  Type <cr> again.  OBVIUS echos
;; (load-image "<path>/einstein") at the end of the *lisp* buffer and evaluates
;; it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESCRIBING VIEWABLES: Try clicking M-left on the picture of
;; Einstein.  This invokes the Common Lisp function (describe) on the
;; image, printing the output to the Lisp listener.  An image is an
;; object with several slots (or fields).  Some of the important slots
;; are the name and the data (that holds the actual data array).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; VIEWABLE NAMES AND SYMBOL BINDINGS: Viewables (e.g., images) in
;; OBVIUS have names that are displayed in the title bar.  By default,
;; load-image gives the image a name that is the same as its filename.
;; You can also name an image using setq, as below (if no-one at your
;; site has used this tutorial before, you may have to edit this line
;; to use the appropriate pathname).

(setq bonzo (load-image (merge-pathnames
			 "images/reagan"
			 obv::*obvius-directory-path*)))

;; OBVIUS loads the image from the file and names it reagan.  Setq
;; then renames it bonzo (that is, the name slot in the image object
;; is set to be bonzo and the symbol bonzo is bound to the image).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
;; PICTURE STACK: At this point, you have two images, bonzo and
;; einstein.  These are the names of the images, and they are also
;; symbols that are bound to the images.  Type "einstein <cr>" to the
;; OBVIUS prompt.  This symbol evaluates to an image, that is then
;; automatically displayed in the current pane. Einstein is now
;; displayed, but what happened to bonzo?  You should think of each
;; pane as holding a circular stack of images, one on top of the
;; other.  When einstein was displayed, it was put on top of the
;; stack; bonzo is still there underneath.  The stack is cycled by
;; clicking either C-left (previous picture) or C-middle (next
;; picture) on the pane.  Try this.  You can also call the function
;; (cycle-pane) from the prompt.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; POPPING PICTURES: Click C-Sh-left on bonzo to remove the picture
;; from the pane.  Remember to pay attention to the mouse
;; documentation line in the emacs mini-buffer to help you keep track
;; of the various mouse bindings.  You can also pop a picture by
;; calling the function (pop-picture).  Evaluate the following line
;; using "C-c e":

(pop-picture)

;; Now you have popped both bonzo and einstein.  The pane should now
;; be blank and its title bar should now say "empty pane".  Although
;; we have popped the pictures from the stack, the images are still
;; bound to symbols in the lisp environment.  Try evaluating the
;; symbol einstein at the prompt.  The picture is redisplayed.  Now
;; evaluate bonzo to redisplay that picture.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MOVE TO HERE: You should still have two panes, one containing a
;; stack of both einstein and reagan, the other empty.  Use C-middle
;; to bring einstein to the top of the stack.  Click C-right on the
;; empty pane.  This moves the picture of einstein from the top of the
;; other pane onto the empty pane.  The pane showing einstein is now
;; the current pane.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; POSITION MESSAGE: Click right on einstein.  This selects the pane.
;; It also prints a message into the emacs mini-buffer telling you
;; where you clicked (which pixel you clicked on) and what was the
;; image value at that pixel.  Now move the mouse around on einstein
;; while holding right mouse button down.  Watch the status line in
;; the emacs minibuffer: the position message is continuously updated.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DRAG: Sometimes, pictures are bigger than the panes in which that
;; they are displayed.  Position the mouse cursor on the einstein
;; picture.  Hold down the control and shift keys and hold down the
;; right mouse button.  Move the mouse around.  Release the right
;; button and then click C-Sh-middle to recenter the picture.  Most
;; pictures can be dragged and centered in this way.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESTROY: Clicking C-M-Sh-left destroys a viewable.  Try it on
;; bonzo.  The picture goes away leaving an empty pane.  Note,
;; however, that this is different from popping the pane.  It destroys
;; the viewable, returns any storage that it might be using, unbinds
;; any symbols that were bound to it, and destroys (pops) any pictures
;; of the viewable.

;; OBVIUS stores data for pictures and viewables separate from the
;; rest of lisp memory.  The responsibility for managing this separate
;; storage area is up to you.  Storage space is reclaimed by
;; destroying a viewable.  Storage space may also be reclaimed by
;; using the (ogc) function discussed below.

;; Now evaluate the symbol bonzo at the prompt.  You will get an
;; error.  Only use (destroy) when you really mean it.  There is no
;; way to recover bonzo without reloading it from a file.  Type :a
;; or "C-c a" to abort from the debugger.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SCALING INFORMATION, SETP AND GETP: In addition to the name, OBVIUS
;; displays picture parameter information in the title bar.  Images
;; are stored as floating-point arrays.  They are rescaled to be
;; displayed in the gray colors of an 8bit colormap.  The title bar
;; for einstein looks like:
;;
;;      (EINSTEIN - 0.0) / 237.
;; 
;; This means that the floating point numbers in the image were
;; rescaled by first subtracting 0.0 and then dividing by 237.  These
;; numbers are also the minimum value and the range of values,
;; respectively.  The units of the scaled gray values are such that 0
;; will map to the blackest color on the screen, and 1 to the whitest.

;; There are a number of parameters associated with a picture of a
;; viewable (e.g., the scale and pedestal).  These can be accessed via
;; the function (getp) and altered using the function (setp).  These
;; functions always operate on the picture on top of the stack of the
;; current pane.  Select the pane that has einstein in it (click
;; right) and evaluate (using "C-c e"):

(setp scale 400.0)

;; Notice that einstein is re-displayed with lower contrast.  Now
;; evaluate:

(getp)

;; to see all of the other parameter settings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SCREEN PARAMETERS: There are also a handful of parameters
;; associated with each and every OBVIUS pane, regardless of what
;; picture is being displayed.  For example, you can choose the
;; background color by evaluating:

(setf (obv::background (current-screen)) :blue)

;; You can also set the gamma correction factor by evaluating:

(setf (obv::gray-gamma (current-screen)) 0.7)

;; Then click middle to refresh the display.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {Hardcopy.} Click C-M-Sh-right to print a copy of einstein to a
;; postscript printer.  In order for this to work, you must have
;; access to a postscript printer from your machine, and the variables
;; *default-printer* and *print-command-string* must be set up
;; appropriately.  You can also call the (hardcopy) function from the
;; prompt to save a postscript version of the picture to a file:

(hardcopy einstein 'gray :path "~/einstein.ps")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BOOST AND REDUCE CONTRAST: Click C-M-left on einstein to boost
;; the contrast.  Do it again.  Click C-M-middle to reduce the
;; contrast.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ZOOM: Click Sh-middle (zoom-out) on einstein.  Then click Sh-left
;; (zoom-in).  Click Sh-left to zoom-in again.  Try dragging the
;; zoomed picture using C-Sh-right.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SLICES AND GRAPHS: Click M-Sh-left on einstein to get an x-slice.
;; You can also call the function (make-slice) from the prompt.  The
;; slice is displayed as a graph.  Evaluate the following to reset
;; some of the parameters of the graph:

(setp :y-range '(100 200) :axis-color :green :color :red)

;; Evaluate (getp) to see all the other parameter settings.

(getp)

;; Click C-Sh-right on the graph and move the mouse while holding the
;; right button down, to drag the graph.  All pictures in OBVIUS can
;; be dragged.  Click C-Sh-middle to re-center the graph.  Then click
;; the right mouse button and on the graph and move the mouse around
;; while holding the right mouse button down.  Note that the
;; coordinates displayed in the minibuffer are those of the graph.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HISTOGRAMS: Bring einstein to the top of the picture stack.  Click
;; C-M-right on einstein to compute a histogram of the image intensity
;; values.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERACTIVE CROP: Bring einstein to the top of the picture stack.
;; Position the mouse somewhere on his face.  Hold down meta and shift
;; and hold down the right mouse button.  Now move the mouse down his
;; shoulder, while still holding down M-Sh-right.  You see a box from
;; the point where you started to the current mouse position.  Look at
;; the status line in the emacs minibuffer.  It tells you where you
;; started and how big the box is.  Move the mouse again to change the
;; shape of the box and watch as the status line is updated.  Release
;; the mouse button.  The region that was within the box is copied
;; into a new image, which is then displayed.  You can also call the
;; (crop) function directly from the prompt.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PICTURE SPECIFIC MOUSE BINDINGS: The mouse bindings for C-M and
;; M-Sh depend on what type on picture is being displayed.  For gray
;; pictures, C-M mouse clicks are bound to boost-contrast,
;; reduce-contrast, and histogram.  For gray pictures, M-Sh mouse
;; clicks are bound to slices and histograms.  We will see later that
;; for overlay pictures, C-M mouse clicks are bound to previous-frame
;; and next-frame.  For flipbook pictures, C-M mouse clicks are bound
;; to previous-frame, next-frame, and display-sequence.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OPERATIONS ON VIEWABLES: Evaluate:

(setq little-al (gauss-out einstein)) 

;; This does a gaussian convolution and subsampling.
;; Now Type:
;;
;;      (*.
;; 
;; and then click left on little-al image.  The string LITTLE-AL is
;; echoed to the *lisp* buffer where the cursor was positioned.  Then
;; type
;; 
;;      -1.0) <cr>

;; The result is displayed, but you did not bind it to a symbol, so
;; the result has no name (the title bar says nil).  Note that you
;; could have typed little-al rather than using the left clicks.
;; However, if the viewable has no name (i.e., it is not bound to a
;; symbol), you can only get at it by using the left click.  Not only
;; that, if you pop the image off the picture stack, you will lose the
;; ability to access it!  We will return to this issue later, when we
;; discuss memory management.

;; Try adding the unnamed image to little-al and putting the result
;; in an image named zero by typing:
;;
;;     (setq zero (+. <space> <click left on nil> little-al))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IREF, MODIFICATION, AND REFRESH: The iref function is parallel to
;; the Common Lisp aref function: it returns the value of a pixel in
;; an image.  For example, to get the value of the (0,0) pixel of the
;; little-al image, evaluate:

(iref little-al 0 0)

;; Now evaluate:

(setf (iref little-al 0 0) 500.0)

;; This modifies the image, setting the value of pixel (0,0) to be
;; 500.  Now evaluate the following expression to see that the pixel
;; value has been changed:

(iref little-al 0 0)

;; Setf iref does not, however, update the picture display.  Cycle the
;; stack so that the little-al image appears on top of the stack.  The
;; title bar now reads "**-little-al".  The "**-" means that the
;; viewable has been modified and the display needs to be refreshed.
;; This is analogous to the "**" that appears in the mode-line in
;; Emacs to indicate that a buffer has been modified.  Click middle on
;; little-al to refresh the display.  You can also call the (refresh)
;; function from the prompt.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESTRUCTIVE MODIFICATION: 

Now evaluate:

(gauss-in (gauss-out little-al) :-> little-al)

;; This does a gaussian convolution and downsampling, followed by
;; upsampling and gaussian convolution.  Note the use of the :->
;; (result) keyword here.  OBVIUS destructively modifies the little-al
;; image, writing the result of the gauss-in into the pixels of
;; little-al.  Click middle to update the picture.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RESUING A NAME: Now try evaluating:

(setq little-al (sqr. little-al))

;; This squares little-al, and creates a new image to hold the result.
;; Then the symbol 'little-al is bound to the new image.  This is just
;; the standard behavior of the Common Lisp setq function.  In
;; addition, the name of the new image is set to this symbol.  This is
;; an OBVIUS enhancement of the setq function (that works only at top
;; level, i.e., at the OBVIUS> prompt).  The name of the old image
;; (i.e., the one formerly named little-al) is changed to NIL.  Note,
;; however, that the old image is NOT destructively modified in this
;; case.  You can see the old image by cycling the picture stack (use
;; C-left).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OGC: The function (ogc) (obvius-garbage-collector) is a primitive
;; garbage collector that offers a means of reclaiming lost storage
;; space.  Earlier, we noted that if you created a viewable without a
;; name and popped it off the picture stack, there would be no way to
;; access that viewable.  Such viewables are known as orphans.  The
;; garbage collector will assume that all such viewables are no longer
;; needed and it will re-claim their storage space.

;; Specifically, (ogc) destroys all the viewables that are not bound
;; to symbols and are not on any of the picture stacks.  Ogc also
;; prints a list of symbols in the current package that are currently
;; bound to viewables, so that you can explicitly destroy them if you
;; want.  Try evaluating (ogc) to see what images you have made so
;; far:

(ogc)

;; Warning: The function (ogc) should only be called from top level.
;; If you try to call it from within your code, you may free storage
;; that belongs to locally bound intermediate images.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SYNTHETIC IMAGES: OBVIUS provides a number of functions for
;; synthesizing certain kinds of images.  It is also quite easy to
;; write code to synthesize other images.  Try evaluating:

(setq sine (make-sin-grating '(128 128) 
			     :period 32
			     :orientation (/ pi 8.0)))

;; Now try:

(setq fractal (make-fractal '(64 64)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IMAGE PAIRS: Evaluate the following expression:

(setq pair (make-image-pair
	    (list (make-ramp '(32 32))
		  (make-ramp '(32 32) :orientation (/ pi 2.0)))))

;; This creates an image-pair viewable that is displayed (by default)
;; as a pasteup picture (the two subimages are displayed next to each
;; other).  This is our first example of a compound viewable: a
;; viewable that is composed of a collection of sub-viewables.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; VECTOR FIELDS: Evaluate:

(display pair 'vector-field)

;; This displays the image pair as a vector field, interpreting the
;; first subimage as the y-component and the second subimage as the
;; x-component.  Evaluate:

(setp :skip 4 :scale 20 :color :green)

;; The skip parameter specifies the sampling rate of the vector field.
;; The scale parameter specifies the vector lengths.  Evaluate:

(getp)

;; to see all the other parameter settings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SURFACE PLOTS: Evaluate:

(display einstein 'surface-plot :x-step 7 :y-step 7)

;; to display the einstein image as a graphical surface plot of
;; intensity.  The display function allows you to specify the
;; display-type (in this case 'surface-plot) and values for the
;; picture parameters.  Evaluate:

(setp :theta 0.5 :phi 0.5 :x-step 5 :y-step 5 :zoom 1/2)

;; Parameters theta and phi specify the orientation and elevation of
;; the viewpoint (in radians). X-step and y-step specify the sampling
;; interval for the wire mesh.  Evaluate:

(getp)

;; to see all the other parameter settings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OBV-REQUIRE: Not all of OBVIUS is loaded when you start up.  The
;; modules that are not loaded may be added by using obv-requre.
;; Alternatively, you can pull down the "Misc" menu and choose the
;; module from the "Load module" sub-menu.  Evaluate the following to
;; load the contour-plot code:

(obv-require :contour-plot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTOUR PLOTS: Evaluate:

(setq surface
      (make-synthetic-image
       '(65 65)
       #'(lambda (y x)
	   (* y (sin (* 2-pi x))
	      (exp (- (+ (/ (sqr x) (sqr 1/2)) (/ (sqr y) (sqr 1/2)))))))))
(display surface 'contour-plot
	 :z-min -0.2 :z-max 0.2 :num-levels 14 :skip 3 :zoom 4)

;; Notice that we passed a bunch of parameters to the display
;; function.  In general, everything you can do with setp can also be
;; done by passing the parameters when the picture is first made.

;; Contour plotting is a bit slow, so don't try to draw too many
;; contour levels (keep :num-levels a relatively small number), and
;; make sure to subsample the image by choosing :skip to be a
;; relatively large number (depending on the image size).  If you
;; don't specify values for these parameters, OBVIUS will choose
;; defaults that will be relatively fast.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BIT-IMAGES: Evaluate the following to create a bit-image.  The
;; result will be 1 for each pixel that is greater than 200.

(setq thresh (>. einstein 200.0))

;; Note: You may have to type :c in response to a continuable error
;; message expanding the bit heap.

;; Now evaluate:

(setq edges (zero-crossings (-. (blur einstein)
 				(blur einstein :level 3))))
(setp :foreground :red)
(setp :background :green)
(getp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; COLOR-IMAGES: Evaluate the following to load and display a color
;; image.

(obv-require :color)
(load-image (merge-pathnames "images/clown" obv::*obvius-directory-path*))

;; By default, color images are displayed as pasteups.  This is
;; because the color bands may or may not correspond to RGB.  To
;; display the image in RGB:

(display clown 'color-picture)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FILTERS: Evaluate the following to make a filter:
 
(setq lap-filter (make-filter '(( 0.0 -1.0  0.0) 
 				(-1.0  4.0 -1.0) 
 				( 0.0 -1.0  0.0))))

;; This filter is displayed as a zoomed-in gray-scale picture.  Now
;; evaluate:

(setq bandpass (apply-filter lap-filter einstein))

;; Click C-M-left several times to boost the contrast.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PYRAMIDS:  Evaluate:

(obv-require :gaussian-pyramid)
(setq pyr1 (make-laplacian-pyramid little-al :level 3))

;; Now, evaluate the following to see that you can multiply pyramids
;; too.

(setq pyr2 (*. pyr1 -1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IMAGE SEQUENCES: Evaluate:

(setq seq (make-image-sequence
	   (loop for i from 0 below 10
		 collect (circular-shift little-al 
					 :y-shift (* i 5) 
					 :x-shift 0))))

;; This creates an image sequence and displays it as a flipbook.  Click
;; C-M-left (previous-frame) and C-M-middle (next-frame) to cycle through
;; the subimages of the sequence.  Then, click C-M-right to display the
;; sequence as a movie.  It will continue cycling until you click the
;; mouse again (any mouse button will do).  Evaluate:

(getp)

;; to see the parameters for flipbooks.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GENERIC VIEWABLE SEQUENCE: Evaluate:

(display 
 (setq vf-seq (make-viewable-sequence
	       (loop for i from 1 to 5
		     collect 
		     (*. i (make-image-pair
			    (list (make-ramp '(32 32))
				  (make-ramp '(32 32) 
					     :orientation (/ pi 2.0))))))))
 'flipbook 
 :sub-display-type 'vector-field 
 :independent-parameters nil)

;; Now, click C-M-right to display the sequence of vector-fields.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OVERLAYS: Evaluate the following to overlay two bit-images on top
;; of einstein:

(display (setq overlay (make-viewable-sequence (list einstein thresh edges)))
	 'overlay)

;; You can set the parameters of each component of the overlay:

(setp :current-picture 0 :scale 300)
(setp :current-picture 1 :foreground :red)
(setp :current-picture 2 :foreground :green)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DISCRETE FUNCTIONS: Evaluate:

(setq log-df (make-discrete-function #'(lambda (x) (log x)) 
 				     10.0 100.0 :size 20))

;; OBVIUS displays a graph of the log function, discretely sampled
;; between 10 and 100.  Evaluate the following to interpolate between
;; the samples, and find the value of the log function at 25.

(evaluate log-df 25.0)

;; Discrete-function objects are displayed as graphs.  Change the
;; picture parameters by evaluating:

(setp :x-range '(10 50) :y-range '(2 4) :x-axis 2 :x-tick-step 5
      :plot-symbol :circle :zoom 20)

;; Discrete-functions are used by OBVIUS as lookup tables to do fast
;; nonlinear point operations on images (see the on-line documentation
;; of the point-operation function, using "C-c a" and "C-c d" or look
;; in the OBVIUS manual).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; POLAR PLOTS: Discrete functions can also be displayed as polar
;; plots.  Evaluate:

(display (make-discrete-function #'(lambda (x) (cos (* 2.0 x))) 
 				 0.0 2-pi :size 31)
 	 'polar-plot :plot-symbol :circle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SCATTER PLOTS: Obvius also displays scatter plots.  Evaluate:

(display (make-image-pair (list (+. (make-ramp 16)
				    (make-gaussian-noise 16))
 				(make-ramp 16)))
 	 'scatter-plot :plot-symbol :circle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OVERLAYING GRAPHS: Evaluate the following expressions:

(setq df0 (make-discrete-function #'(lambda (x) x) 
 				  0.0 1.0 :size 20))
(setq df1 (make-discrete-function #'(lambda (x) (sqr x)) 
 				  0.0 1.0 :size 20))
(setq df2 (make-discrete-function #'(lambda (x) (sqr (sqr x)))
 				  0.0 1.0 :size 20))
(display (make-viewable-sequence (list df0 df1 df2))
	 'overlay :plot-symbol :circle)

;; Obvius displays superimposed graphs.  Note that the :plot-symbol
;; keyword is used to initialize the picture.  Use setp to change the
;; picture parameters:

(setp :current-picture 0 :plot-symbol :square)
(setp :current-picture 1 :color :red)
(setp :current-picture 2 :color :green :fill-symbol-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; VIEWABLE MATRICES: Evaluate:

(load-image (merge-pathnames "images/reagan" obv::*obvius-directory-path*))
(setq al (gauss-out einstein))
(setq ron (gauss-out reagan))
(setq mat (make-image-matrix (list (list al ron) 
 				   (list (negate ron) (negate al)))))

;; This make a matrix of images, analogous to a matrix of numbers, that
;; allows you to do parallel matrix operations.  For example, evaluate:

(setq vec (make-image-matrix (list (list al) (list ron))))
(matrix-mul mat vec)

;; You can think of this as doing a pixel-by-pixel matrix multiplication,
;; taking corresponding pixels out of each image in mat and each
;; image in vec.  Here's an even weirder example.  Evaluate:

(display (setq inv (matrix-inverse mat))
 	 'pasteup
 	 :pedestal -2.5e-2 :scale 5e-2)

;; The warning messages mean that for several image positions, the
;; matrix is singular.  To demostrate that inv really is the matrix
;; inverse of mat, evaluate:

(matrix-mul mat inv)

;; This image matrix has ones (all pixels equal 1, except for the
;; singular points) along the diagonal and zeros off diagonal.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; QUITTING OBVIUS: To quit OBVIUS, you can call the Common Lisp
;; function (quit).  From within Emacs, you can also just kill the
;; *lisp* buffer using the standard command "C-x k".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
