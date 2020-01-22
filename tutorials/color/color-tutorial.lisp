;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: color-tutorial.lisp
;;;  Author: Chichilnisky
;;;  Description:
;;;  Creation Date: summer 1992
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In this tutorial, we will introduce some basic spectral calculations that we
;; can use to understand color vision, surface reflectances, illuminants,
;; photoreceptor encoding, and linear models of surface spectra.

;; You will notice that much of the work below involves matrix algebra.  I
;; suggest that you pull out a pad of paper and draw matrix tableaus as we
;; proceed to help you follow the calculations.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This tutorial on runs only on an 8bit SUN computer.  It uses true-color
;; pictures to display the color images.  True-color pictures allocate a color
;; map entry for each different color.  You will no doubt fill up the color map
;; before you are done.  When/if this happens, you will get a warning:
;;
;;     Failed to allocate requested colors
;;
;; and the next picture will be partially blank.  When/if this happens, you
;; should pop a few of your old true-color pictures (using C-Sh-left) to free
;; up some color map entries.

;; To get off to a good start, we will free up most of the color map by
;; evaluating:

(progn
  (setf (obv::gray-shades (current-screen)) 8)
  (setf (obv::rgb-bits (current-screen)) '(1 1 1))
  (setf (obv::pseudo-color-function (current-screen)) 'obv::pseudo-color-ramp)
  (setf (obv::pseudo-colors (current-screen)) 3))

;; It is also useful to run the colormap tool xcolmap by evaluating:

(run-program "xcolmap" :wait nil)

;; If xcolmap is not available on your system, ask your system manager about
;; other colormap tools that might be available.  It may be helpful to have
;; some way to keep track of the colormap.

;; We will be displaying pictures in two panes to compare them.  If you have
;; only one pane, then evaluate (new-pane) to make a second one.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup stuff. Just do it and don't worry.

(progn
  (setf (obv::background (obv::current-screen)) :black)
  (obv-require :matrix)			; OBVIUS matrix code
  (obv-require :conversion)		; File format conversion
  (obv-require :color)			; Color images and color pictures
  (lcl:cd (merge-pathnames "tutorials/color" obv::*obvius-directory-path*))
  (compile-load "auxiliary")		; Local routines
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; READ IN A SET OF SURFACES.

;; The Macbeth Color Checker is a set of 24 surfaces commonly used to
;; evaluate color balancing systems.  The materials in the Color
;; Checker were selected to have the same surface reflectance
;; functions as various important surfaces that are commonly used in
;; television and film images.  We will read in a matrix of data.
;; Each row of the matrix is the reflectance function of one of the
;; Macbeth surfaces, measured at each of 31 samples in the visible
;; spectrum.  (Most of the visible spectrum is in the 400-700
;; nanometers wavelength region.)  Thus we get a 24x31 matrix of
;; surface spectra. Each row is one surface.

(setf macbeth (read-cap-matrix "macbeth"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOOK AT THE SURFACE REFLECTANCE SPECTRA

;; The 8th row of this matrix is a surface that typically looks
;; greenish.  We plot the fractional reflectance (as a function of
;; wavelength) for this surface.
(setf green-macbeth-plot (make-discrete-function (displaced-row 7 macbeth) 400 700))

;; For example, about 28% of the light at wavelength 500 nm is
;; reflected by the "Green" surface.
(evaluate green-macbeth-plot 500)

;; Here is a red surface:
(setf red-macbeth-plot (make-discrete-function (displaced-row 9 macbeth) 400 700))

;; And here is a gray surface:
(setf gray-macbeth-plot (make-discrete-function (displaced-row 20 macbeth) 400 700))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; READ IN A TYPICAL DAYLIGHT SPECTRUM.

;; Now, read in a row vector (again, converting the file format
;; appropriately) whose entries represent the amount of light present
;; at each wavelength for a standard daylight (a mix of blue sky and
;; cloudy lighting).  Again, the light is sampled at 31 points in the
;; visible spectrum, so we get a 31-vector.

(setf daylight-65 (vectorize (read-cap-matrix "daylight-65")))

;; Make a plot of this light. Note: the energy units are arbitrary.
(setf daylight-65-plot (make-discrete-function daylight-65 400 700))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALCULATE THE SPECTRAL SIGNAL RESULTING FROM THIS LIGHT AND MACBETH SURFACES.

;; The spectral signal is the pointwise product of the incident light
;; and the reflectance of the surface at the each wavelength.  This
;; can be expressed as a matrix product, where the surface matrix is
;; multiplied by a big diagonal matrix with the light intensities at
;; each wavelength along the diagonal.  So, we get a 24x31 matrix with
;; the spectral signal.

(setf spectral-signals (matrix-mul macbeth (make-diagonal-matrix daylight-65)))

;; As examples, plot the spectral signal coming off the green and gray
;; Macbeth surfaces.

(setf green-macbeth-signal
      (make-discrete-function (displaced-row 7 spectral-signals) 400 700))
(setf gray-macbeth-signal
      (make-discrete-function (displaced-row 20 spectral-signals) 400 700))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; READ IN THE HUMAN CONE SPECTRAL SENSITIVITIES.

;; Now we want to measure the photoresponses of the human cones to
;; these stimuli.  The human cone spectral sensitivities have been
;; measured and correspond closely with behavioral color matching data
;; known for many years.  We now read in a matrix of the human cone
;; sensitivities.  There are three classes of cone, the so-called L,M,
;; and S cones ((L)ong-, (M)iddle-, and (S)hort-wavelength peak
;; sensitivities). Again, we represent each cone by its sensitivity at
;; each of 31 sample wavelengths in the spectrum.  Thus, we have a
;; 3x31 matrix representing the human cone spectral sensitivities.

(setf cones (read-cap-matrix "cones"))

;; Look at each cone class independently. The spectral sensitivity
;; profile of each cone class is stored in a row of the matrix.
(setf L-cone (make-discrete-function (displaced-row 0 cones) 400 700))
(setf M-cone (make-discrete-function (displaced-row 1 cones) 400 700))
(setf S-cone (make-discrete-function (displaced-row 2 cones) 400 700))

;; Look at the three cone classes superimposed.  Notice how close the
;; L and M cones are in terms of their peak sensitivities.
(progn
  (display (make-viewable-sequence (list L-cone M-cone S-cone))
	   'overlay)
  (setp :current-picture 0 :color :red)
  (setp :current-picture 1 :color :green)
  (setp :current-picture 2 :color :blue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALCULATE THE HUMAN CONE RESPONSES TO THE SPECTRAL SIGNALS.

;; We can describe the receptor encoding as a matrix multiplication
;; too. The matrix product of the receptor sensitivites and the
;; transposed spectral signals gives the photoisomerizations/second in
;; each receptor class due to each of the surfaces. Hence, we have a
;; 3x24 matrix of receptor responses.

(setf cone-signals (matrix-mul-transpose cones spectral-signals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RENDER THE SURFACES ON THE MONITOR, SO THEY LOOK RIGHT

;; Read in the spectra of a typical set of color monitor phosphors.
;; These are probably much like to the ones on your screen, but not
;; quite right because we haven't calibrated these monitors.  Each row
;; in this matrix corresponds to one phosphor, giving its relative
;; output energy as a function of wavelength.

(setf phosphors (read-cap-matrix "phosphors"))

;; Look at the three phosphor spectra individually and together
(setf R-phosphor (make-discrete-function (displaced-row 0 phosphors) 400 700))
(setf G-phosphor (make-discrete-function (displaced-row 1 phosphors) 400 700))
(setf B-phosphor (make-discrete-function (displaced-row 2 phosphors) 400 700))
(progn
  (display (make-viewable-sequence (list R-phosphor G-phosphor B-phosphor))
	   'overlay)
  (setp :current-picture 0 :color :red)
  (setp :current-picture 1 :color :green)
  (setp :current-picture 2 :color :blue))

;; Here we calculate the monitor phosophor intensities required to
;; generate the *same* receptor responses in your eye that the Macbeth
;; surfaces generate under daylight.  To do so, we first find the
;; linear tranform that gives the cone responses due to the different
;; monitor spectra. For obscure reasons, we will think of the phosphor
;; intensities and the cone responses as 3D row vectors (not columns),
;; so the equations are transposed from what you may have seen. To
;; understand this calculation, try pulling out a piece of paper and
;; convincing yourself that this is the way to get cone responses from
;; the phosphor intensities.

(setf monitor->cones (matrix-mul-transpose phosphors cones))

;; Now, the inverse of this matrix tells us how to set the
;; phosphors to achieve any desired cone responses:

(setf cones->monitor (matrix-inverse monitor->cones))

;; We apply this transformation to the desired cone responses and
;; obtain the necessary monitor intensities for rendering the image.

(setf monitor-signals (matrix-transpose-mul cone-signals cones->monitor))

;; The digital RGB values in the framebuffer are related in a
;; nonlinear way to the actual phosphor intensities that result on the
;; screen.  This can be approximately fixed by raising the desired
;; intensity values to a fixed power, for example, 0.6. This is
;; typically called "Gamma Correction" for historical reasons (the
;; exponent is called "gamma").

(setf *gamma* 0.6)
(power monitor-signals *gamma* :-> monitor-signals)

;; Note that this hack (arbitrarily chosen correction) does not
;; linearize the display perfectly.  To linearize accurately one
;; simply measures the static nonlinear relationship between the 8bit
;; R,G, and B values and the intensity of the resulting signal.  To do
;; so would involve time-consuming calibration procedures.

;; To display these values, we make a color-image out of the 24 rgb values.  We
;; do this using the make-color-checker function defined in auxilliary.lisp:

(display (setq macbeth-checker (make-color-checker monitor-signals))
	 'true-color :zoom :auto :pedestal 0 :scale 155)

;; You get a warning:
;;     Clipping color values between 0 and 155
;; meaning that there are some colors that are not properly represented.  The
;; color-image is made up of float rgb values that are rescaled to 8bit values
;; using the pedestal and scale keywords.  In this particular case, there is
;; one color with rgb values that are negative.  This means that this color can
;; not be rendered on this monitor.  It is out the monitor's gamut.  Click
;; right on the blue-ish square near the lower left corner.  The r and b values
;; are both around -8.

;; Also, we used an arbitrary scale factor to display these colors.  The
;; following will display the same colors under "dim" daylight conditions:
(setp :scale 200)

;; If we make this scale factor too small:
(setp :scale 100)
;; then we click values above 100 (e.g., click right on square in the lower
;; right corner).

(setp :scale 155)

;; If it weren't for this gamut problem (arbitrary scale factor and clipping
;; negative values), and if the phosphor spectra accurately reflected the
;; monitor properties, and if the gamma correction we applied was appropriate
;; to compensate for the monitor intensity nonlinearity, the Macbeth chart you
;; see would look like it would look under normal daylight.

;; Now we write a function to do all this work for us.  This function starts
;; with spectral representations of a set of surfaces, a light, the human,
;; cones, and the monitor phosphors.  It produces a color image that can be
;; displayed as a true-color picture.  Compile this function (using C-c c):

(defun render-surfaces (surfaces light cones phosphors
			&key (gamma *gamma*) (rows 4) (cols 6) ->)
  (let* ((spectral-signals (matrix-mul surfaces (make-diagonal-matrix light)))
	 (cone-signals (matrix-mul-transpose cones spectral-signals))
	 (monitor->cones (matrix-mul-transpose phosphors cones))
	 (cones->monitor (matrix-inverse monitor->cones))
	 (monitor-signals (matrix-transpose-mul cone-signals cones->monitor)))
    (power monitor-signals gamma :-> monitor-signals)
    (make-color-checker monitor-signals :rows rows :cols cols :-> ->)))

;; Now evaluate this expression to set the defaults:

(progn
  (set-default 'color-image :display-type 'true-color)
  (set-default 'true-color :zoom :auto)
  (set-default 'true-color :pedestal 0)
  (set-default 'true-color :scale 155))

;; Now re-render the Macbeth color checker:

(setq macbeth-checker (render-surfaces macbeth daylight-65 cones phosphors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USE A LOW-DIMENSIONAL APPROXIMATION OF THE SURFACE SPECTRA

;; We constructed this display by calculating the full spectral signal
;; from the surface reflectances and light spectrum, then calculating
;; the receptor responses and recreating those responses on the
;; monitor.  Unfortunately, 31 spectral samples are a rather expensive
;; way to represent the spectral properties of surfaces.  How can we
;; compress the information in the reflectances and still achieve an
;; accurate rendition of the surfaces?  A common method is to use the
;; principal components of the surface set.  Below, we use the
;; singular value decomposition (SVD) to reconstruct a
;; lower-dimensional representation of the full surface set. Try using
;; the 1,2,3, and 4-dimensional reconstructions of the surface set to
;; see how well they represent the spectral properties of the surfaces
;; under daylight.  Evaluate *each* of the reconstructions below.
;; After evaluating the first expression, go down a few lines and
;; evaluate the code that redisplays the approximated surface set on
;; the screen.  Do this for all 4 approximations and see how well they
;; capture the Macbeth surface spectra.

;; For camparison, we will put these up in the second pane:
(next-pane)

(setq macbeth-checker-1 (render-surfaces
			 (singular-value-reconstruction macbeth :dimension 1)
			 daylight-65 cones phosphors))
(setq macbeth-checker-2 (render-surfaces
			 (singular-value-reconstruction macbeth :dimension 2)
			 daylight-65 cones phosphors))
(setq macbeth-checker-3 (render-surfaces
			 (singular-value-reconstruction macbeth :dimension 3)
			 daylight-65 cones phosphors))
(setq macbeth-checker-4 (render-surfaces
			 (singular-value-reconstruction macbeth :dimension 4)
			 daylight-65 cones phosphors))

;; Notice that 3 or 4 dimensions adequately describe this set of
;; surfaces, which in principle could require 24 dimensions to be
;; reproduced accurately.  Hence there is a big savings conferred by
;; using this reconstruction.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A DIFFERENT LINEAR MODEL FOR SURFACE REFLECTANCES - "ONE-MODE"
;; DECOMPOSITION Above, we used the singular value decomposition of
;; the surface set to approximate the wavelength spectrum of the
;; surface reflectances.  But in many applications we are not
;; interested in representing the wavelenght spectrum, but rather its
;; effect on the human visual system.  In these cases the principal
;; components of the surfaces set are not the best low-dimnsional
;; linear model for the surfaces.  Another linear model of surfaces
;; called "one-mode" (which you can ask more about if you are
;; interested) captures the receptor responses optimally in a
;; least-squares sense.  Hence in this case it is equivalent to
;; reconstructing the receptor responses (rather than the surfaces)
;; from the SVD.  Below we recontstruct the receptor responses from
;; the first two dimensions of their SVD, to see how well one-mode
;; would perform.  How does one-mode fare compared to the
;; 2-dimensional SVD analysis of the surfaces?

(let ((light daylight-65)
      (surfaces macbeth)
      (dimension 2))
  (setf spectral-signals (matrix-mul surfaces (make-diagonal-matrix light)))
  (setf cone-signals (matrix-mul-transpose cones spectral-signals))
  (singular-value-reconstruction cone-signals :dimension dimension :-> cone-signals)
  (setf monitor-signals (matrix-transpose-mul cone-signals cones->monitor))
  (power monitor-signals *gamma* :-> monitor-signals)
  (setq macbeth-checker-one-mode (make-color-checker monitor-signals)))

;; Clean up (especially important for freeing color map entries:
(purge!)
(set-default 'true-color :scale :auto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; DIFFERENT LIGHT SOURCES

;; First, read in a xenon flash light source plot its spectrum with
;; daylight-65.
(setf xenon-flash (vectorize (read-cap-matrix "xenon-flash")))
(setf xenon-flash-plot (make-discrete-function xenon-flash 400 700))
(setf daylight-65-plot (make-discrete-function daylight-65 400 700))
(display (make-viewable-sequence (list xenon-flash-plot daylight-65-plot))
	 'overlay)

;; How does the Macbeth Color Checker look under this illumination?

(setq macbeth-daylight (render-surfaces macbeth daylight-65 cones phosphors))
(next-pane)
(setq macbeth-xenon (render-surfaces macbeth xenon-flash cones phosphors))

;; Yet another illuminant.
(setf cie-a (vectorize (read-cap-matrix "cie-a")))
(setf cie-a-plot (make-discrete-function cie-a 400 700))
(display (make-viewable-sequence (list cie-a-plot daylight-65-plot))
	 'overlay)

;; Macbeth surfaces again
(setq macbeth-cie-a (render-surfaces macbeth cie-a cones phosphors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; ANOTHER SET OF SURFACES

;; A much larger set of sample surfaces is the Munsell surface set,
;; with 359 surfaces.  Read these in:
(setf munsell (read-cap-matrix "munsell"))

;; Choose a random set of 24 to work with, since you can't display more than
;; about 200 different colors on the screen at any one time anyway,
;; and you want to have multiple plots up there for comparison.
(setf some-munsell (crop (shuffle-rows munsell) :y-size 24))

;; Render these surfaces in daylight
(setq munsell-daylight (render-surfaces some-munsell daylight-65 cones phosphors))

;; Now you're on your own. Try to represent these lights and surfaces
;; with low-dimensional models, and see how well the models represent
;; the color information for the visual system.  Remember to keep no
;; more than 5 XV windows up so you don't run out of space in the
;; system colormap.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:

