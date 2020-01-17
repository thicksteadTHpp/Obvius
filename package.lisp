;;;; package.lisp

(defpackage #:obvius
  (:use #:cl)
  (:nicknames :obv)
  (:shadow *print-pretty*
	   float
	   union
	   set))


;;[THO] change all occurences of clos to closer-mp or appropriate
;; then get rid of that extra package
;;this is needed because lucid-cl had clos in
;; an extra package
;; (defpackage #:CLOS
;;   (:use #:closer-common-lisp)
;;   (:export (do-external-symbols (s #:closer-common-lisp)
;; 	     s)))

;;exports all of common lisp
;;and closer-mop
(macrolet ((define-clos-package ()
             (loop with symbols = (nunion (loop for sym being the external-symbols of :common-lisp
                                                if (find-symbol (symbol-name sym) :c2mop)
                                                collect it
                                                else collect sym)
                                          (loop for sym being the external-symbols of :c2mop
                                                collect sym))
                   with map = '()
                   for symbol in symbols do
                   (push (symbol-name symbol)
                         (getf map (symbol-package symbol)))
                   finally (return 
                            `(defpackage #:clos
                               (:use)
                               ,@(loop for (package symbols) on map by #'cddr
                                       collect `(:import-from ,(package-name package) ,@symbols))
                               (:export ,@(mapcar #'symbol-name symbols)))))))
(define-clos-package))
