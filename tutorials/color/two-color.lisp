;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Land Two-color demonstration

;; Look at a nice, colorful image
(setf file-name "/usr/csh/images/pugsley.ras")
(xv file-name)

;; Read in Raster file as byte data, convert it to floating-point, gamma-correct
(multiple-value-setq (pugsley-byte cols rows)
  (load-sun-raster-file file-name))
(setf pugsley-float (coerce-to-float pugsley-byte))
(power pugsley-float (/ gamma) :-> pugsley-float)

;; Create a sampling and a reconstruction matrix.
;; The sampling matrix projects the data onto two dimensions of color.
;; The reconstruction matrix generates 3D data from the projected data.
;; Of course, the product matrix is singular.
(setq sampling (matrix-transpose (matrix '((1 0 0) (0 1 0)))))
(setq basis (matrix '((0.8 0 0) (0.0 0.9 0.9))))
(print-values (setq transform (matrix-mul sampling basis)))

;; Hit the color data on the right with this matrix
;; (The RGB triplets are in the rows of this big array)
(setq transformed-pugsley-float (similar pugsley-float))
(matrix-mul pugsley-float transform :-> transformed-pugsley-float)
(coerce-to-8bit transformed-pugsley-float :-> pugsley-byte)
(power transformed-pugsley-float gamma :-> transformed-pugsley-float)

;; Put the transformed data on the screen
(let ((out-file "/tmp/pugsley"))
  (save-sun-raster-file pugsley-byte out-file :dimensions (list cols rows))
  (xv out-file))

;; Free up storage, please!
(mapcar 'free-array (list transformed-pugsley-float pugsley-float pugsley-byte))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Scratch area - Ignore this...

(setf raw (read-cap-matrix "/usr/csh/cap/envfiles/light/cieabc.1.mat"))
(setf sampled (downsample (col 0 raw) :x-step 10))
(setf out (vectorize sampled :x-offset 3 :size 31))
(write-cap-matrix out "cie-a")
(image-from-array out)

