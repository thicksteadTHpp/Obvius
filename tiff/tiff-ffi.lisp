;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: TIFF -*-
;;; *-* File: tiff-ffi.lisp *-*
;;; *-* Last-Edit: 28-OCT-93 *-*
;;; *-* Edited-By: Harald Lange, Carsten Schroeder *-*
;;; *-* Institution: University of Hamburg, Dept. of Computer Science *-*
;;;
;;;; ************************* History ****************************************
;;;
;;;  Ported to Obvius-3.0/Lucid-4.0 by Patrick Teo (August 11, 1993)
;;;     - everything seems to work fine except for bitmaps.
;;;     - bitmaps work fine within Obvius but expoting and importing
;;;       bitmaps to other applications like 'xv' do not work yet.
;;;  Problems with bitmaps solved by Carsten Schroeder (October 28, 1993)
;;;     - works well for Lucid-4.1 running on Sun SPARC.
;;;     - hope this works for Lucid-4.0 and on SGI as well.
;;;
;;;; ************************* Description ************************************
;;;
;;;  LispWorks foreign function interface to Sam Leffler's TIFF
;;;  library version 3.00.
;;;  This code complies to TIFF Rev. 6.0.
;;;
;;;  The code is somewhat ugly in that it cannot use the library
;;;  functions TIFFGetField and TIFFSetField because of the variable
;;;  argument lists.  So, there are separate functions for the tag
;;;  fields most often used.
;;;
;;;  Note:
;;;  Special care must be taken for BIT-IMAGEs; their interpretation
;;;  depends on the implementation of bit arrays in our Common Lisp.
;;;
;;;  - bit ordering
;;;    If the bytes are interpreted in lsb-to-msb order (as in
;;;    LispWorks and Lucid) then the byte used for the bit vector 
;;;    #*10000000 will be the integer 1 in C instead of the
;;;    integer 128.  Upon reading of BIT-IMAGEs from TIFF files we 
;;;    therefore have to do a bit reversal when the FillOrder tag
;;;    field is set to msb2lsb (which is default).  We assume that the
;;;    TIFF library is *not* doing any bit reversal on its own
;;;    (otherwise we must do a bit reversal in any case).  
;;;    See read-image-plane-into.
;;;    Upon writing we are doing a bit reversal as well (back to
;;;    msb2lsb), because some TIFF readers cannot handle lsb2msb
;;;    files.  See write-image-plane.
;;;
;;;  - byte ordering
;;;    TIFF files can be in two different formats: big-endian byte
;;;    order and little-endian byte order.  The TIFF library handles
;;;    the conversion to the byte ordering of the current machine,
;;;    e.g. big-endian (i.e. most significant first) on a Sun SPARC.
;;;    Again, the implementation of Common Lisp is free to choose an
;;;    interpretation of a four-byte word as a bit array.
;;;    E.g., on a Sun SPARC the unsigned long 128 corresponds to the 
;;;    byte vector #(0 0 0 128) (big-endian byte order).
;;;    In LispWorks this is intepreted as the bit vector 
;;;      #*00000000000000000000000000000001
;;;    in Lucid, however, this is interpreted as the bit vector
;;;      #*00000001000000000000000000000000
;;;    The unsigned long 1 corresponds to the byte vector #(0 0 0 1),
;;;    and in LispWorks this is interpreted as to the bit vector
;;;      #*00000000000000000000000010000000
;;;    while in Lucid this is intepreted as the bit vector
;;;      #*10000000000000000000000000000000
;;;    Thus, in Lucid the interpretation of a four-byte word as a bit
;;;    array corresponds to a little-endian byte order.
;;;    Therefore, we have to swap the bytes of a word in Lucid upon
;;;    reading and writing of a BIT-IMAGE.
;;;    See read-image-plane-into and write-image-plane.


;;;; ************************* Interface-Commands *****************************

(in-package 'tiff
	    :use '(LUCID-COMMON-LISP SYSTEM LISP CLOS OBVIUS))

(export '(
	 TIFFTAG-SUBFILETYPE
	 FILETYPE-REDUCEDIMAGE
	 FILETYPE-PAGE
	 FILETYPE-MASK

	 TIFFTAG-OSUBFILETYPE
	 OFILETYPE-IMAGE
	 OFILETYPE-REDUCEDIMAGE
	 OFILETYPE-PAGE

	 TIFFTAG-IMAGEWIDTH
	 TIFFTAG-IMAGELENGTH
	 TIFFTAG-BITSPERSAMPLE

	 TIFFTAG-COMPRESSION
	 COMPRESSION-NONE
	 COMPRESSION-CCITTRLE
	 COMPRESSION-CCITTFAX3
	 COMPRESSION-CCITTFAX4
	 COMPRESSION-LZW
	 COMPRESSION-NEXT
	 COMPRESSION-CCITTRLEW
	 COMPRESSION-PACKBITS
	 COMPRESSION-THUNDERSCAN
	 COMPRESSION-JPEG
	 COMPRESSION-PICIO
	 COMPRESSION-SGIRLE

	 TIFFTAG-PHOTOMETRIC
	 PHOTOMETRIC-MINISWHITE
	 PHOTOMETRIC-MINISBLACK
	 PHOTOMETRIC-RGB
	 PHOTOMETRIC-PALETTE
	 PHOTOMETRIC-MASK
	 PHOTOMETRIC-SEPARATED
	 PHOTOMETRIC-YCBCR
	 PHOTOMETRIC-DEPTH

	 TIFFTAG-THRESHHOLDING
	 THRESHHOLD-BILEVEL
	 THRESHHOLD-HALFTONE
	 THRESHHOLD-ERRORDIFFUSE

	 TIFFTAG-CELLWIDTH
	 TIFFTAG-CELLLENGTH

	 TIFFTAG-FILLORDER
	 FILLORDER-MSB2LSB
	 FILLORDER-LSB2MSB

	 TIFFTAG-DOCUMENTNAME
	 TIFFTAG-IMAGEDESCRIPTION
	 TIFFTAG-MAKE
	 TIFFTAG-MODEL
	 TIFFTAG-STRIPOFFSETS

	 TIFFTAG-ORIENTATION
	 ORIENTATION-TOPLEFT
	 ORIENTATION-TOPRIGHT
	 ORIENTATION-BOTRIGHT
	 ORIENTATION-BOTLEFT
	 ORIENTATION-LEFTTOP
	 ORIENTATION-RIGHTTOP
	 ORIENTATION-RIGHTBOT
	 ORIENTATION-LEFTBOT

	 TIFFTAG-SAMPLESPERPIXEL
	 TIFFTAG-ROWSPERSTRIP
	 TIFFTAG-STRIPBYTECOUNTS
	 TIFFTAG-MINSAMPLEVALUE
	 TIFFTAG-MAXSAMPLEVALUE
	 TIFFTAG-XRESOLUTION
	 TIFFTAG-YRESOLUTION

	 TIFFTAG-PLANARCONFIG
	 PLANARCONFIG-CONTIG
	 PLANARCONFIG-SEPARATE

	 TIFFTAG-PAGENAME
	 TIFFTAG-XPOSITION
	 TIFFTAG-YPOSITION
	 TIFFTAG-FREEOFFSETS
	 TIFFTAG-FREEBYTECOUNTS

	 TIFFTAG-GRAYRESPONSEUNIT
	 GRAYRESPONSEUNIT-10S
	 GRAYRESPONSEUNIT-100S
	 GRAYRESPONSEUNIT-1000S
	 GRAYRESPONSEUNIT-10000S
	 GRAYRESPONSEUNIT-100000S

	 TIFFTAG-GRAYRESPONSECURVE

	 TIFFTAG-GROUP3OPTIONS
	 GROUP3OPT-2DENCODING
	 GROUP3OPT-UNCOMPRESSED
	 GROUP3OPT-FILLBITS

	 TIFFTAG-GROUP4OPTIONS
	 GROUP4OPT-UNCOMPRESSED

	 TIFFTAG-RESOLUTIONUNIT
	 RESUNIT-NONE
	 RESUNIT-INCH
	 RESUNIT-CENTIMETER

	 TIFFTAG-PAGENUMBER

	 TIFFTAG-COLORRESPONSEUNIT
	 COLORRESPONSEUNIT-10S
	 COLORRESPONSEUNIT-100S
	 COLORRESPONSEUNIT-1000S
	 COLORRESPONSEUNIT-10000S
	 COLORRESPONSEUNIT-100000S

	 TIFFTAG_TRANSFERFUNCTION
	 TIFFTAG-SOFTWARE
	 TIFFTAG-DATETIME
	 TIFFTAG-ARTIST
	 TIFFTAG-HOSTCOMPUTER
	 TIFFTAG-PREDICTOR
	 TIFFTAG-WHITEPOINT
	 TIFFTAG-PRIMARYCHROMATICITIES
	 TIFFTAG-COLORMAP
	 TIFFTAG_HALFTONEHINTS
	 TIFFTAG-TILEWIDTH
	 TIFFTAG-TILELENGTH
	 TIFFTAG-TILEOFFSETS
	 TIFFTAG-TILEBYTECOUNTS
	 TIFFTAG-BADFAXLINES

	 TIFFTAG-CLEANFAXDATA
	 CLEANFAXDATA-CLEAN
	 CLEANFAXDATA-REGENERATED
	 CLEANFAXDATA-UNCLEAN

	 TIFFTAG-CONSECUTIVEBADFAXLINES

	 TIFFTAG-INKSET
	 INKSET-CMYK

	 TIFFTAG_INKNAMES
	 TIFFTAG_DOTRANGE
	 TIFFTAG_TARGETPRINTER

	 TIFFTAG_EXTRASAMPLES
	 EXTRASAMPLE_UNSPECIFIED
	 EXTRASAMPLE_ASSOCALPHA
	 EXTRASAMPLE_UNASSALPHA

	 TIFFTAG_SAMPLEFORMAT
	 SAMPLEFORMAT_INT
	 SAMPLEFORMAT_UINT
	 SAMPLEFORMAT_IEEEFP
	 SAMPLEFORMAT_VOID

	 TIFFTAG_SMINSAMPLEVALUE
	 TIFFTAG_SMAXSAMPLEVALUE

	 TIFFTAG_JPEGPROC
	 JPEGPROC_BASELINE
	 JPEGPROC_LOSSLESS

	 TIFFTAG_JPEGIFOFFSET
	 TIFFTAG_JPEGIFBYTECOUNT
	 TIFFTAG_JPEGRESTARTINTERVAL
	 TIFFTAG_JPEGLOSSLESSPREDICTORS
	 TIFFTAG_JPEGPOINTTRANSFORM
	 TIFFTAG_JPEGQTABLES
	 TIFFTAG_JPEGDCTABLES
	 TIFFTAG_JPEGACTABLES
	 TIFFTAG_YCBCRCOEFFICIENTS
	 TIFFTAG_YCBCRSUBSAMPLING

	 TIFFTAG_YCBCRPOSITIONING
	 YCBCRPOSITION_CENTERED
	 YCBCRPOSITION_COSITED

	 TIFFTAG_REFERENCEBLACKWHITE

	 TIFFTAG-MATTEING
	 TIFFTAG-DATATYPE
	 TIFFTAG-IMAGEDEPTH
	 TIFFTAG-TILEDEPTH

	 with-open-tiff-file

	 current-directory
	 current-row
	 current-strip
	 current-tile
	 fd-open
	 fileno
	 filename
	 flush
	 flush-data
	 number-of-strips
	 number-of-pages
	 number-of-tiles
	 print-directory
	 read-directory
	 read-encoded-strip
	 read-encoded-tile
	 read-raw-strip
	 read-raw-tile
	 read-scanline
	 reverse-bits
	 scanline-size
	 set-directory
	 strip-size
	 swap-array-of-long
	 tiff-file-p
	 tile-size
	 tiledp
	 write-directory
	 write-encoded-strip
	 write-encoded-tile
	 write-raw-strip
	 write-raw-tile
	 write-scanline

	 get-bitspersample
	 get-colormap
	 get-compression
	 get-datetime
	 get-documentname
	 get-fillorder
	 get-hostcomputer
	 get-imagedescription
	 get-imagelength
	 get-imagewidth
	 get-make
	 get-model
	 get-newsubfiletype
	 get-pagename
	 get-pagenumber
	 get-photometric
	 get-planarconfig
	 get-samplesperpixel
	 get-software

	 set-bitspersample
	 set-colormap
	 set-compression
	 set-datetime
	 set-documentname
	 set-fillorder
	 set-hostcomputer
	 set-imagedescription
	 set-imagelength
	 set-imagewidth
	 set-make
	 set-model
	 set-newsubfiletype
	 set-pagename
	 set-pagenumber
	 set-photometric
	 set-planarconfig
	 set-samplesperpixel
	 set-software

	 read-image-plane
	 read-image-plane-into
	 write-image-plane
	 separate-image-planes))

;;;; ************************* Compiler-Directives ****************************

;;;; ************************* Body *******************************************

(defun tiff-array-element-type (array-type)
  (let ((result (array-element-type array-type)))
    (if (eq result 'bit) '(unsigned-byte 1) result)))

#-Obvius
(defun allocate-array (&rest args)
  (LCL::with-static-area (apply #'make-array args)))

#+LCL4.0
(defun upgraded-array-element-type (type) 
  (tiff-array-element-type (make-array 1 :element-type type)))


;;; ======================================================
;;; Constant declarations for names and values of TIFFTAGS
;;; (See C-Header-File /usr/local/tiff/libtiff/tiff.h)
;;; ======================================================

;; * TIFF Tag Definitions.
;; *
;; * Those marked with a + are obsoleted by revision 5.0.
;; * Those marked with a ! are introduced in revision 6.0.
;; * Those marked with a $ are obsoleted by revision 6.0.

(defconstant TIFFTAG-SUBFILETYPE            254) ;subfile data descriptor
(defconstant FILETYPE-REDUCEDIMAGE	      1) ;reduced resolution version
(defconstant FILETYPE-PAGE                    2) ;one page of many
(defconstant FILETYPE-MASK                    4) ;transparency mask

(defconstant TIFFTAG-OSUBFILETYPE	    255) ;+kind of data in subfile
(defconstant OFILETYPE-IMAGE		      1) ;full resolution image data
(defconstant OFILETYPE-REDUCEDIMAGE	      2) ;reduced size image data
(defconstant OFILETYPE-PAGE		      3) ;one page of many

(defconstant TIFFTAG-IMAGEWIDTH		    256) ;image width in pixels
(defconstant TIFFTAG-IMAGELENGTH	    257) ;image height in pixels
(defconstant TIFFTAG-BITSPERSAMPLE	    258) ;bits per channel (sample)

(defconstant TIFFTAG-COMPRESSION	    259) ;data compression technique
(defconstant COMPRESSION-NONE		      1) ;dump mode
(defconstant COMPRESSION-CCITTRLE	      2) ;CCITT modified Huffman RLE
(defconstant COMPRESSION-CCITTFAX3	      3) ;CCITT Group 3 fax encoding
(defconstant COMPRESSION-CCITTFAX4	      4) ;CCITT Group 4 fax encoding
(defconstant COMPRESSION-LZW		      5) ;Lempel-Ziv  & Welch
(defconstant COMPRESSION-JPEG		      6) ;!JPEG compression
(defconstant COMPRESSION-NEXT		  32766) ;NeXT 2-bit RLE
(defconstant COMPRESSION-CCITTRLEW	  32771) ;#1 w/ word alignment
(defconstant COMPRESSION-PACKBITS	  32773) ;Macintosh RLE
(defconstant COMPRESSION-THUNDERSCAN	  32809) ;ThunderScan RLE

(defconstant TIFFTAG-PHOTOMETRIC	    262) ;photometric interpretation
(defconstant PHOTOMETRIC-MINISWHITE	      0) ;min value is white
(defconstant PHOTOMETRIC-MINISBLACK	      1) ;min value is black
(defconstant PHOTOMETRIC-RGB		      2) ;RGB color model
(defconstant PHOTOMETRIC-PALETTE	      3) ;color map indexed
(defconstant PHOTOMETRIC-MASK		      4) ;$holdout mask
(defconstant PHOTOMETRIC-SEPARATED	      5) ;!color separations
(defconstant PHOTOMETRIC-YCBCR		      6) ;!CCIR 601
(defconstant PHOTOMETRIC_CIELAB	              8) ;!1976 CIE L*a*b*

(defconstant TIFFTAG-THRESHHOLDING	    263) ;+thresholding used on data
(defconstant THRESHHOLD-BILEVEL		      1) ;b&w art scan
(defconstant THRESHHOLD-HALFTONE	      2) ;or dithered scan
(defconstant THRESHHOLD-ERRORDIFFUSE	      3) ;usually floyd-steinberg

(defconstant TIFFTAG-CELLWIDTH		    264) ;+dithering matrix width
(defconstant TIFFTAG-CELLLENGTH		    265) ;+dithering matrix height

(defconstant TIFFTAG-FILLORDER		    266) ;+data order within a byte
(defconstant FILLORDER-MSB2LSB		      1) ;most significant -> least
(defconstant FILLORDER-LSB2MSB		      2) ;least significant -> most

(defconstant TIFFTAG-DOCUMENTNAME	    269) ;name of doc. image is from
(defconstant TIFFTAG-IMAGEDESCRIPTION	    270) ;info about image
(defconstant TIFFTAG-MAKE		    271) ;scanner manufacturer name
(defconstant TIFFTAG-MODEL		    272) ;scanner model name/number
(defconstant TIFFTAG-STRIPOFFSETS	    273) ;offsets to data strips

(defconstant TIFFTAG-ORIENTATION	    274) ;+image orientation
(defconstant ORIENTATION-TOPLEFT	      1) ;row 0 top, col 0 lhs
(defconstant ORIENTATION-TOPRIGHT	      2) ;row 0 top, col 0 rhs
(defconstant ORIENTATION-BOTRIGHT	      3) ;row 0 bottom, col 0 rhs
(defconstant ORIENTATION-BOTLEFT	      4) ;row 0 bottom, col 0 lhs
(defconstant ORIENTATION-LEFTTOP	      5) ;row 0 lhs, col 0 top
(defconstant ORIENTATION-RIGHTTOP	      6) ;row 0 rhs, col 0 top
(defconstant ORIENTATION-RIGHTBOT	      7) ;row 0 rhs, col 0 bottom
(defconstant ORIENTATION-LEFTBOT	      8) ;row 0 lhs, col 0 bottom

(defconstant TIFFTAG-SAMPLESPERPIXEL	    277) ;samples per pixel
(defconstant TIFFTAG-ROWSPERSTRIP	    278) ;rows per strip of data
(defconstant TIFFTAG-STRIPBYTECOUNTS	    279) ;bytes counts for strips
(defconstant TIFFTAG-MINSAMPLEVALUE	    280) ;+minimum sample value
(defconstant TIFFTAG-MAXSAMPLEVALUE	    281) ;+maximum sample value
(defconstant TIFFTAG-XRESOLUTION	    282) ;pixels/resolution in x
(defconstant TIFFTAG-YRESOLUTION	    283) ;pixels/resolution in y

(defconstant TIFFTAG-PLANARCONFIG	    284) ;storage organization
(defconstant PLANARCONFIG-CONTIG	      1) ;single image plane
(defconstant PLANARCONFIG-SEPARATE	      2) ;separate planes of data

(defconstant TIFFTAG-PAGENAME		    285) ;page name image is from
(defconstant TIFFTAG-XPOSITION		    286) ;x page offset of image lhs
(defconstant TIFFTAG-YPOSITION		    287) ;y page offset of image lhs
(defconstant TIFFTAG-FREEOFFSETS	    288) ;+byte offset to free block
(defconstant TIFFTAG-FREEBYTECOUNTS	    289) ;+sizes of free blocks

(defconstant TIFFTAG-GRAYRESPONSEUNIT	    290) ;$gray scale curve accuracy
(defconstant GRAYRESPONSEUNIT-10S	      1) ;tenths of a unit
(defconstant GRAYRESPONSEUNIT-100S	      2) ;hundredths of a unit
(defconstant GRAYRESPONSEUNIT-1000S	      3) ;thousandths of a unit
(defconstant GRAYRESPONSEUNIT-10000S	      4) ;ten-thousandths of a unit
(defconstant GRAYRESPONSEUNIT-100000S	      5) ;hundred-thousandths

(defconstant TIFFTAG-GRAYRESPONSECURVE	    291) ;$gray scale response curve

(defconstant TIFFTAG-GROUP3OPTIONS	    292) ;32 flag bits
(defconstant GROUP3OPT-2DENCODING	      1) ;2-dimensional coding
(defconstant GROUP3OPT-UNCOMPRESSED	      2) ;data not compressed
(defconstant GROUP3OPT-FILLBITS		      4) ;fill to byte boundary

(defconstant TIFFTAG-GROUP4OPTIONS	    293) ;32 flag bits
(defconstant GROUP4OPT-UNCOMPRESSED	      2) ;data not compressed

(defconstant TIFFTAG-RESOLUTIONUNIT	    296) ;units of resolutions
(defconstant RESUNIT-NONE		      1) ;no meaningful units
(defconstant RESUNIT-INCH		      2) ;english
(defconstant RESUNIT-CENTIMETER		      3) ;metric

(defconstant TIFFTAG-PAGENUMBER		    297) ;page numbers of multi-page

(defconstant TIFFTAG-COLORRESPONSEUNIT	    300) ;$color scale curve accuracy
(defconstant COLORRESPONSEUNIT-10S	      1) ;tenths of a unit
(defconstant COLORRESPONSEUNIT-100S	      2) ;hundredths of a unit
(defconstant COLORRESPONSEUNIT-1000S	      3) ;thousandths of a unit
(defconstant COLORRESPONSEUNIT-10000S	      4) ;ten-thousandths of a unit
(defconstant COLORRESPONSEUNIT-100000S	      5) ;hundred-thousandths

(defconstant TIFFTAG_TRANSFERFUNCTION       301) ;!colorimetry info
(defconstant TIFFTAG-SOFTWARE		    305) ;name & release
(defconstant TIFFTAG-DATETIME		    306) ;creation date and time
(defconstant TIFFTAG-ARTIST		    315) ;creator of image
(defconstant TIFFTAG-HOSTCOMPUTER	    316) ;machine where created
(defconstant TIFFTAG-PREDICTOR		    317) ;prediction scheme w/ LZW
(defconstant TIFFTAG-WHITEPOINT		    318) ;image white point
(defconstant TIFFTAG-PRIMARYCHROMATICITIES  319) ;!primary chromaticities
(defconstant TIFFTAG-COLORMAP		    320) ;RGB map for pallette image
(defconstant TIFFTAG_HALFTONEHINTS          321) ;!highlight+shadow info */
(defconstant TIFFTAG-TILEWIDTH		    322) ;!rows/data tile
(defconstant TIFFTAG-TILELENGTH		    323) ;!cols/data tile
(defconstant TIFFTAG-TILEOFFSETS	    324) ;!offsets to data tiles
(defconstant TIFFTAG-TILEBYTECOUNTS	    325) ;!byte counts for tiles
(defconstant TIFFTAG-BADFAXLINES	    326) ;lines w/ wrong pixel count

(defconstant TIFFTAG-CLEANFAXDATA	    327) ;regenerated line info
(defconstant CLEANFAXDATA-CLEAN		      0) ;no errors detected
(defconstant CLEANFAXDATA-REGENERATED	      1) ;receiver regenerated lines
(defconstant CLEANFAXDATA-UNCLEAN	      2) ;uncorrected errors exist

(defconstant TIFFTAG-CONSECUTIVEBADFAXLINES 328) ;max consecutive bad lines

(defconstant TIFFTAG-INKSET		    332) ;!inks in separated image
(defconstant INKSET-CMYK		      1) ;!cyan-magenta-yellow-black

(defconstant TIFFTAG_INKNAMES               333) ;!ascii names of inks
(defconstant TIFFTAG_DOTRANGE               336) ;!0% and 100% dot codes
(defconstant TIFFTAG_TARGETPRINTER          337) ;!separation target

(defconstant TIFFTAG_EXTRASAMPLES           338) ;!info about extra samples
(defconstant EXTRASAMPLE_UNSPECIFIED          0) ;!unspecified data
(defconstant EXTRASAMPLE_ASSOCALPHA           1) ;!associated alpha data
(defconstant EXTRASAMPLE_UNASSALPHA           2) ;!unassociated alpha data

(defconstant TIFFTAG_SAMPLEFORMAT           339) ;!data sample format
(defconstant SAMPLEFORMAT_INT                 1) ;!signed integer data
(defconstant SAMPLEFORMAT_UINT                2) ;!unsigned integer data
(defconstant SAMPLEFORMAT_IEEEFP              3) ;!IEEE floating point data
(defconstant SAMPLEFORMAT_VOID                4) ;!untyped data

(defconstant TIFFTAG_SMINSAMPLEVALUE        340) ;!variable MinSampleValue
(defconstant TIFFTAG_SMAXSAMPLEVALUE        341) ;!variable MaxSampleValue

(defconstant TIFFTAG_JPEGPROC               512) ;!JPEG processing algorithm
(defconstant JPEGPROC_BASELINE                1) ;!baseline sequential
(defconstant JPEGPROC_LOSSLESS               14) ;!Huffman coded lossless

(defconstant TIFFTAG_JPEGIFOFFSET           513) ;!pointer to SOI marker
(defconstant TIFFTAG_JPEGIFBYTECOUNT        514) ;!JFIF stream length
(defconstant TIFFTAG_JPEGRESTARTINTERVAL    515) ;!restart interval length
(defconstant TIFFTAG_JPEGLOSSLESSPREDICTORS 517) ;!lossless proc predictor
(defconstant TIFFTAG_JPEGPOINTTRANSFORM     518) ;!lossless point transform
(defconstant TIFFTAG_JPEGQTABLES            519) ;!Q matrice offsets
(defconstant TIFFTAG_JPEGDCTABLES           520) ;!DCT table offsets
(defconstant TIFFTAG_JPEGACTABLES           521) ;!AC coefficient offsets
(defconstant TIFFTAG_YCBCRCOEFFICIENTS      529) ;!RGB -> YCbCr transform
(defconstant TIFFTAG_YCBCRSUBSAMPLING       530) ;!YCbCr subsampling factors

(defconstant TIFFTAG_YCBCRPOSITIONING       531) ;!subsample positioning
(defconstant YCBCRPOSITION_CENTERED           1) ;!as in PostScript Level 2
(defconstant YCBCRPOSITION_COSITED            2) ;!as in CCIR 601-1

(defconstant TIFFTAG_REFERENCEBLACKWHITE    532) ;!colorimetry info

;;; tags 32995-32999 are private tags registered to SGI
(defconstant TIFFTAG-MATTEING             32995) ;$use ExtraSamples
(defconstant TIFFTAG-DATATYPE             32996) ;$use SampleFormat
(defconstant TIFFTAG-IMAGEDEPTH           32997) ;z depth of image
(defconstant TIFFTAG-TILEDEPTH            32998) ;z depth/data tile


;;; ===========================
;;; Declarations of alien types
;;; ===========================

#|
(declaim (inline null-alien-p))
(defun null-alien-p (alien)
  "Test if the alien is a C null pointer."
  ;; *** Highly implementation dependent -- CS
  (zerop (foreign::alien-base alien)))
|#

;;; Lucid Lisp Version
;;; Tests if alien is a C null pointer
(defun null-alien-p (alien)
  (and (foreign-pointer-p alien)
       (zerop (foreign-pointer-address alien))))


(def-foreign-synonym-type c-string :string)
(def-foreign-synonym-type simple-c-string (:pointer :char))
(def-foreign-synonym-type unsigned-short :unsigned-16bit)
(def-foreign-synonym-type unsigned-long :unsigned-32bit)
(def-foreign-synonym-type short :signed-16bit)
(def-foreign-synonym-type long :signed-32bit)
(def-foreign-synonym-type int :signed-32bit)
(def-foreign-synonym-type unsigned-int :unsigned-32bit)
(def-foreign-synonym-type char :character)



(def-foreign-struct tiff-header
  (tiff_magic   :type :unsigned-short)
  (tiff_version :type :unsigned-short)
  (tiff_diroff  :type :unsigned-long))


(def-foreign-struct tiff-directory
  (td_fieldsset        :type (:array :unsigned-long (2)))
  (td_imagewidth       :type :unsigned-long)
  (td_imagelength      :type :unsigned-long)
  (td_imagedepth       :type :unsigned-long)
  (td_tilewidth        :type :unsigned-long)
  (td_tilelength       :type :unsigned-long)
  (td_tiledepth        :type :unsigned-long)
  (td_subfiletype      :type :unsigned-short)
  (td_bitspersample    :type :unsigned-short)
  (td_sampleformat     :type :unsigned-short)
  (td_compression      :type :unsigned-short)
  (td_photometric      :type :unsigned-short)
  (td_threshholding    :type :unsigned-short)
  (td_fillorder        :type :unsigned-short)
  (td_orientation      :type :unsigned-short)
  (td_samplesperpixel  :type :unsigned-short)
  (td_predictor        :type :unsigned-short)
  (td_rowsperstrip     :type :unsigned-long)
  (td_minsamplevalue   :type :unsigned-long)
  (td_maxsamplevalue   :type :unsigned-long)
  (td_xresolution      :type :single-float)
  (td_yresolution      :type :single-float)
  (td_resolutionunit   :type :unsigned-short)
  (td_planarconfig     :type :unsigned-short)
  (td_xposition        :type :single-float)
  (td_yposition        :type :single-float)
  (td_group3options    :type :unsigned-long)
  (td_group4options    :type :unsigned-long)
  (td_pagenumber       :type (:array :unsigned-short (2)))
  (td_matteing         :type :unsigned-short)
  (td_cleanfaxdata     :type :unsigned-short)
  (td_badfaxrun        :type :unsigned-short)
  (td_badfaxlines      :type :unsigned-long)
  (td_colormap         :type (:array (:pointer :unsigned-short) (3)))
  (td_halftonehint     :type (:array :unsigned-short (2)))
  (td_documentname     :type simple-c-string)
  (td_artist           :type simple-c-string)
  (td_datetime         :type simple-c-string)
  (td_hostcomputer     :type simple-c-string)
  (td_imagedescription :type simple-c-string)
  (td_make             :type simple-c-string)
  (td_model            :type simple-c-string)
  (td_software         :type simple-c-string)
  (td_pagename         :type simple-c-string)
  (td_stripsperimage   :type :unsigned-long)
  (td_nstrips          :type :unsigned-long)
  (td_stripoffset      :type (:pointer :unsigned-long))
  (td_stripbytecount   :type (:pointer :unsigned-long))
  ;; include YCBCR_SUPPORT here
  ;; include JPEG_SUPPORT here
  ;; include COLORIMETRY_SUPPORT here
  ;; include CMYK_SUPPORT here
  )


(def-foreign-struct tiff
  (tif_name            :type simple-c-string)
  (tif_fd              :type short)
  (tif_mode            :type short)
  (tif_fillorder       :type char)
  (tif_options         :type char)
  (tif_flags           :type short)
  (tif_diroff          :type long)
  (tif_nextdiroff      :type long)
  (tif_dir             :type tiff-directory)
  (tif_header          :type tiff-header)
  (tif_typeshift       :type (:pointer int))
  (tif_typemask        :type (:pointer long))
  (tif_row             :type long)
  (itf_curdir          :type int)
  (tif_curstrip        :type int)
  (tif_curoff          :type long)
  (tif_col             :type long)
  (tif_curtile         :type int)
  (tif_tilesize        :type long)
  
  ;; The following slots are pointers to C-functions
  ;; (See /usr/local/tiff/libtiff/tiffioP.h)

  ;; As long as we are not going to use it, I'm
  ;; not going to bother setting it right for now.
  ;; -PT
  
  (tif_predecode      :type (:pointer int))
  (tif_preencode      :type (:pointer int))
  (tif_postencode     :type (:pointer int))
  (tif_decoderow      :type (:pointer int))
  (tif_encoderow      :type (:pointer int))
  (tif_decodestrip    :type (:pointer int))
  (tif_encodestrip    :type (:pointer int))
  (tif_decodetile     :type (:pointer int))
  (tif_encodetile     :type (:pointer int))
  (tif_close          :type (:pointer int))
  (tif_seek           :type (:pointer int))
  (tif_cleanup        :type (:pointer int))
  (tif_data           :type simple-c-string)
  (tif_scanlinesize   :type int)
  (tif_scanlineskew   :type int)
  (tif_rawdata        :type simple-c-string)
  (tif_rawdatasize    :type long)
  (tif_rawcp          :type simple-c-string)
  (tif_rawcc          :type long)
  
  ;; include MMAP_SUPPORT here
  )

;;; ===========
;;; User Macros
;;; ===========

(defmacro with-open-tiff-file ((stream filename &rest options) &body body)
  "Analogous to with-open-file for TIFF files.
Accepts :input and :output for the :direction option
and :error, :supercede or :append for the :if-exists option.
You can use the functions contained in the TIFF library 
for handling the opened TIFF file, not the Common Lisp IO-functions."
  (let ((mode 
	 (case (getf options :direction)
	   ((nil :input) "r")
	   (:output (if (eq (getf options :if-exists) :append)
                    "a"
                    "w"))
	   (t (error  "Use :input or :output for the keyword :direction!"))))
	(signal-error 
	 (case (getf options :if-exists)
	   ((nil :error) t)
	   ((:supercede :append) nil)
	   (t (error
	       "Use :error, :supercede or :append for the keyword :if-exists!"))))
	(tif (gensym)))
    `(progn
       ,(when (and (equal mode "w") 
		   signal-error)
	  `(when (probe-file ,filename)
	     (cerror "Supercede ~S." 
		     "File ~S already exists."
		     (pathname ,filename))))
       (let ((,tif (tiff-open ,filename ,mode)))
	 (unwind-protect
	     (let ((,stream ,tif))
	       ,@body)
	   (tiff-close ,tif))))))
	 

;;; =================================
;;; Declarations of foreign functions
;;; =================================

(eval-when (load compile eval)
  (defun fname (string)
    #+Sun (concatenate 'string "_" string)
    #+Irix string))

;; extern void TIFFClose(TIFF *tif);
(def-foreign-function (tiff-close (:name (fname "TIFFClose")))
  (tif (:pointer tiff)))

;; extern int TIFFCurrentDirectory(TIFF *tif);
(def-foreign-function (current-directory (:name (fname "TIFFCurrentDirectory"))
					 (:return-type int))
  (tif (:pointer tiff)))
				
;; extern long TIFFCurrentRow(TIFF *tif);
(def-foreign-function (current-row (:name (fname "TIFFCurrentRow"))
				   (:return-type int))
  (tif (:pointer tiff)))

;; extern int TIFFCurrentStrip(TIFF *tif);
(def-foreign-function (current-strip (:name (fname "TIFFCurrentStrip"))
				     (:return-type int))
  (tif (:pointer tiff)))
				
;; extern int TIFFCurrentTile(TIFF *tif);
(def-foreign-function (current-tile (:name (fname "TIFFCurrentTile"))
				    (:return-type int))
  (tif (:pointer tiff)))
				
;; extern TIFF* TIFFFdOpen(int fd, char *name, char *mode);
(def-foreign-function (tiff-fd-open (:name (fname "TIFFFdOpen"))
				    (:return-type (:pointer tiff)))
  (fd   int)
  (name :string)
  (mode :string))

(defun fd-open (fd name mode)
  ;; Assumes that a C NULL pointer is returned in case of an error.
  (let ((tif (tiff-fd-open fd name mode)))
    (if (null-alien-p tif)
        (error "Error opening TIFF file!")
        tif)))

;; extern int TIFFFileno(TIFF *tif);
(def-foreign-function (fileno (:name (fname "TIFFFileno"))
			      (:return-type int))
  (tif (:pointer tiff)))
				
;; extern char *TIFFFileName(TIFF *tif);
(def-foreign-function (filename (:name (fname "TIFFFileName"))
				(:return-type :simple-string))
  (tif (:pointer tiff)))
				
;; extern int TIFFFlush(TIFF *tif);
(def-foreign-function (ff-tiff-flush (:name (fname "TIFFFlush"))
				     (:return-type int))
    (tif (:pointer tiff)))

;;;
;;; Return value of 0 does not evaluate to false.
;;; -PT
;;;
(defun tiff-flush (tif) (plusp (ff-tiff-flush tif)))

(defun flush (tif)
  (if (plusp (tiff-flush tif))
      (values)
      (error "Error flushing TIFF file!")))

;; extern int TIFFFlushData(TIFF *tif);
(def-foreign-function (ff-tiff-flush-data (:name (fname "TIFFFlushData"))
					  (:return-type int))
    (tif (:pointer tiff)))

(defun tiff-flush-data (tif) (plusp (ff-tiff-flush-data tif)))

(defun flush-data (tif)
  (if (tiff-flush-data tif)
      (values)
      (error "Error flushing data to TIFF file!")))


;; extern int TIFFGetField(TIFF *tif, int tag, ...ap...);
;; Because of the varargs this function is seperated into several
;; ones, see below.

;; We'll buy that.  -PT

;; extern int TIFFIsTiled(TIFF *tif);
(def-foreign-function (ff-tiledp (:name (fname "TIFFIsTiled"))
				 (:return-type int))
  (tif (:pointer tiff)))

(defun tiledp (tif) (plusp (ff-tiledp tif)))

;; extern unsigned integer TIFFNumberOfStrips(TIFF *tif);
(def-foreign-function (number-of-strips (:name (fname "TIFFNumberOfStrips"))
					(:return-type unsigned-int))
  (tif (:pointer tiff)))

(defun number-of-pages (tif)
  (let ((current (current-directory tif)))
    (set-directory tif 0)
    (unwind-protect
        (do ((i 1 (1+ i)))
          ((not (tiff-read-directory tif)) i))
      (set-directory tif current))))

;; extern unsigned integer TIFFNumberOfTiles(TIFF *tif);
(def-foreign-function (number-of-tiles (:name (fname "TIFFNumberOfTiles"))
				       (:return-type unsigned-int))
  (tif (:pointer tif)))

;; extern TIFF* TIFFOpen(char *name, char *mode);
(def-foreign-function (ff-tiff-open (:name (fname "TIFFOpen"))
				    (:return-type (:pointer tiff)))
  (name :string)
  (mode :string))

(defun tiff-open (name mode)
  ;; Assumes that a C NULL pointer is returned in case of an error.
  (let ((tif (ff-tiff-open name mode)))
    (if (null-alien-p tif)
        (error "Error opening TIFF file!")
        tif)))


;; extern void TIFFPrintDirectory(TIFF *tif, struct _iobuf *fd, long flags);
;;(foreign:define-foreign-function (tiff-print-directory "TIFFPrintDirectory"
;;						       :object)
;;				 ((tif   (:alien tiff))
;;				  (fd    :pointer)
;;				  (flags :integer))
;;				 :language :ansi-C)
(def-foreign-function (print-directory (:name (fname "tiff_print_dir")))
  (tif (:pointer tiff)))

;; extern int TIFFReadDirectory(TIFF *tif);
(def-foreign-function (ff-tiff-read-directory (:name (fname "TIFFReadDirectory"))
					      (:return-type int))
  (tif (:pointer tiff)))

(defun tiff-read-directory (tiff) (plusp (ff-tiff-read-directory tiff)))

(defun read-directory (tif)
  (if (tiff-read-directory tif)
      (values)
      (error "Error reading TIFF directory!")))


;; extern int TIFFReadEncodedStrip(TIFF *tif, unsigned int strip,
;;                                 unsigned char *buf, unsigned int size);
(def-foreign-function (tiff-read-encoded-strip (:name (fname "TIFFReadEncodedStrip"))
					       (:return-type int))
  (tif (:pointer tiff))
  (strip int)
  (buf  (:array))
  (size int))


(defun read-encoded-strip (tif strip buf size)
  (let ((num (tiff-read-encoded-strip tif strip buf size)))
    (if (= num -1)
        (error "Error reading TIFF encoded strip!")
        (values buf num))))


;; extern int TIFFReadEncodedTile(TIFF *tif, unsigned int tile,
;;                                 unsigned char *buf, unsigned int size);
(def-foreign-function (tiff-read-encoded-tile (:name (fname "TIFFReadEncodedTile"))
					      (:return-type int))
  (tif (:pointer tiff))
  (tile int)
  (buf  int)
  (size int))

(defun read-encoded-tile (tif tile buf size)
  (let ((num (tiff-read-encoded-tile tif tile buf size)))
    (if (= num -1)
        (error "Error reading TIFF encoded tile!")
        (values buf num))))


;; extern int TIFFReadRawStrip(TIFF *tif, unsigned int strip,
;;                             unsigned char *buf, unsigned int size);
(def-foreign-function (tiff-read-raw-strip (:name (fname "TIFFReadRawStrip"))
					   (:return-type int))
  (tif   (:pointer tiff))
  (strip int)
  (buf   int)
  (size  int))


(defun read-raw-strip (tif strip buf size)
  (let ((num (tiff-read-raw-strip tif strip buf size)))
    (if (= num -1)
        (error "Error reading TIFF raw strip!")
        (values buf num))))


;; extern int TIFFReadRawTile(TIFF *tif, unsigned int tile,
;;                             unsigned char *buf, unsigned int size);
(def-foreign-function (tiff-read-raw-tile (:name (fname "TIFFReadRawTile"))
					  (:return-type int))
  (tif   (:pointer tiff))
  (tile  int)
  (buf   (:array))
  (size  int))

(defun read-raw-tile (tif tile buf size)
  (let ((num (tiff-read-raw-tile tif tile buf size)))
    (if (= num -1)
        (error "Error reading TIFF raw tile!")
        (values buf num))))


;; extern int TIFFReadScanline(TIFF *tif, unsigned char *buf, unsigned int row,
;;                             unsigned int sample);
(def-foreign-function (tiff-read-scanline (:name (fname "TIFFReadScanline"))
					  (:return-type int))
  (tif    (:pointer tiff))
  (buf    (:array))
  (row    int)
  (sample int))

(defun read-scanline (tif buf row sample)
  (if (= (tiff-read-scanline tif buf row sample)
         -1)
      (error "Error reading TIFF scanline!")
      buf))


;; extern void TIFFReverseBits(unsigned char *data, int nbytes);
(def-foreign-function (reverse-bits (:name (fname "TIFFReverseBits")))
  (data   (:array))
  (nbytes int))

;; extern int TIFFScanlineSize(TIFF *tif);
(def-foreign-function (scanline-size (:name (fname "TIFFScanlineSize"))
				     (:return-type int))
  (tif (:pointer tiff)))


;; extern int TIFFSetDirectory(TIFF *tif, int n);
(def-foreign-function (ff-tiff-set-directory (:name (fname "TIFFSetDirectory"))
					    (:return-type int))
  (tif (:pointer tiff))
  (n   int))

(defun tiff-set-directory (tif n) (plusp (ff-tiff-set-directory tif n)))

(defun set-directory (tif n)
  (if (tiff-set-directory tif n)
      (values)
      (error "Error setting TIFF directory to number ~D!" n)))


;; extern int TIFFSetField(TIFF *tif, int tag, ...ap...);
;; Because of the varargs this function is seperated into several
;; ones, see below.


;; extern unsigned long TIFFStripSize(TIFF *tif);
(def-foreign-function (strip-size (:name (fname "TIFFStripSize"))
				  (:return-type unsigned-int))
  (tif (:pointer tiff)))


;; extern void TIFFSwabArrayOfLong(unsigned long *, unsigned long);
(def-foreign-function (swap-array-of-long (:name (fname "TIFFSwabArrayOfLong")))
  (data   (:array))
  (nwords int))

;; extern int tiff_file_p(char* name);
(def-foreign-function (ff-tiff-file-p (:name (fname "tiff_file_p"))
				      (:return-type int))
  (name :string))

(defun tiff-file-p (name) (plusp (ff-tiff-file-p name)))


;; extern unsigned long TIFFTileSize(TIFF *tif);
(def-foreign-function (tile-size (:name (fname "TIFFTileSize"))
				 (:return-type unsigned-int))
  (tif (:pointer tiff)))


;; extern int TIFFWriteDirectory(TIFF *tif);
(def-foreign-function (ff-tiff-write-directory (:name (fname "TIFFWriteDirectory"))
					       (:return-type int))
  (tif (:pointer tiff)))

(defun tiff-write-directory (tif) (ff-tiff-write-directory tif))

(defun write-directory (tif)
  (if (tiff-write-directory tif)
      (values)
      (error "Error writing TIFF directory!")))


;; extern int TIFFWriteEncodedStrip(TIFF *tif, unsigned int strip,
;;                                  unsigned char *buf, unsigned int size);
(def-foreign-function (tiff-write-encoded-strip (:name (fname "TIFFWriteEncodedStrip"))
						(:return-type int))
  (tif   (:pointer tiff))
  (strip int)
  (buf   (:array))
  (size  int))

(defun write-encoded-strip (tif strip buf size)
  (if (/= (tiff-write-encoded-strip tif strip buf size)
          size)
      (error "Error writing TIFF encoded strip!")
      (values)))


;; extern int TIFFWriteEncodedTile(TIFF *tif, unsigned int tile,
;;                                  unsigned char *buf, unsigned int size);
(def-foreign-function (tiff-write-encoded-tile (:name (fname "TIFFWriteEncodedTile"))
					       (:return-type int))
  (tif   (:pointer tiff))
  (tile  int)
  (buf   (:array))
  (size  int))

(defun write-encoded-tile (tif tile buf size)
  (if (/= (tiff-write-encoded-tile tif tile buf size)
          size)
      (error "Error writing TIFF encoded tile!")
      (values)))


;; extern int TIFFWriteRawStrip(TIFF *tif, unsigned int strip,
;;                              unsigned char* buf, unsigned int size);
(def-foreign-function (tiff-write-raw-strip (:name (fname "TIFFWriteRawStrip"))
					    (:return-type int))
  (tif   (:pointer tiff))
  (strip int)
  (buf   (:array))
  (size  int))

(defun write-raw-strip (tif strip buf size)
  (if (/= (tiff-write-raw-strip tif strip buf size)
          size)
      (error "Error writing TIFF raw strip!")
      (values)))


;; extern int TIFFWriteRawTile(TIFF *tif, unsigned int tile,
;;                              unsigned char* buf, unsigned int size);
(def-foreign-function (tiff-write-raw-tile (:name (fname "TIFFWriteRawTile"))
					   (:return-type int))
  (tif   (:pointer tiff))
  (tile  int)
  (buf   (:array))
  (size  int))

(defun write-raw-tile (tif tile buf size)
  (if (/= (tiff-write-raw-tile tif tile buf size)
          size)
      (error "Error writing TIFF raw tile!")
      (values)))


;; extern int TIFFWriteScanline(TIFF *tif, unsigned char *buf,
;;                              unsigned int row, unsigned int sample);
(def-foreign-function (tiff-write-scanline (:name (fname "TIFFWriteScanline"))
					   (:return-type int))
  (tif    (:pointer tiff))
  (buf    (:array))
  (row    int)
  (sample int))

(defun write-scanline (tif buf row sample)
  (if (= (tiff-write-scanline tif buf row sample)
         -1)
      (error "Error writing TIFF scanline!")
      (values)))


;;; ==============================
;;; Functions for setting TIFFTAGS
;;; ==============================

;; extern int tiff_set_bitspersample (TIFF *tif, int bitspersample);
(def-foreign-function (ff-tiff-set-bitspersample (:name (fname "tiff_set_bitspersample"))
						 (:return-type int))
  (tif (:pointer tiff))
  (bitspersample int))

(defun tiff-set-bitspersample (tif bitspersample) (ff-tiff-set-bitspersample tif bitspersample))

;(declaim (inline set-bitspersample))
(defun set-bitspersample (tif bitspersample)
  (unless (tiff-set-bitspersample tif bitspersample)
    (error "Error setting BitsPerSample tag!"))
  bitspersample)

				
;; extern int tiff_set_colormap (TIFF *tif,
;;                               short *redcolormap,
;;                               short *greencolormap,
;;                               short *bluecolormap);
(def-foreign-function (ff-tiff-set-colormap (:name (fname "tiff_set_colormap"))
					    (:return-type int))
  (tif (:pointer tiff))
  (redcolormap (:array))
  (greencolormap (:array))
  (bluecolormap (:array)))

(defun tiff-set-colormap (tif redcolormap greencolormap bluecolormap)
  (ff-tiff-set-colormap tif redcolormap greencolormap bluecolormap))

;(declaim (inline set-colormap))
(defun set-colormap (tif red green blue)
  (unless (tiff-set-colormap tif red green blue)
    (error "Error setting ColorMap tag!"))
  (values red green blue))


;; extern int tiff_set_compression (TIFF *tif, int compression);
(def-foreign-function (ff-tiff-set-compression (:name (fname "tiff_set_compression"))
					       (:return-type int))
  (tif (:pointer tiff))
  (compression int))

(defun tiff-set-compression (tif compression) (ff-tiff-set-compression tif compression))

;(declaim (inline set-compression))
(defun set-compression (tif compression)
  (unless (tiff-set-compression tif compression)
    (error "Error setting Compression tag!"))
  compression)

				
;; extern int tiff_set_datetime (TIFF *tif, char *datetime);
(def-foreign-function (ff-tiff-set-datetime (:name (fname "tiff_set_datetime"))
					    (:return-type int))
  (tif (:pointer tiff))
  (datetime :string))

(defun tiff-set-datetime (tif datetime) (ff-tiff-set-datetime tif datetime))

;(declaim (inline set-datetime))
(defun set-datetime (tif datetime)
  (unless (tiff-set-datetime tif datetime)
    (error "Error setting DateTime tag!"))
  datetime)


;; extern int tiff_set_documentname (TIFF *tif, char *documentname);
(def-foreign-function (ff-tiff-set-documentname (:name (fname "tiff_set_documentname"))
						(:return-type int))
  (tif (:pointer tiff))
  (documentname :string))

(defun tiff-set-documentname (tif documentname) (ff-tiff-set-documentname tif documentname))

;(declaim (inline set-documentname))
(defun set-documentname (tif documentname)
  (unless (tiff-set-documentname tif documentname)
    (error "Error setting DocumentName tag!"))
  documentname)


;; extern int tiff_set_fillorder (TIFF *tif, int fillorder);
(def-foreign-function (ff-tiff-set-fillorder (:name (fname "tiff_set_fillorder"))
					     (:return-type int))
  (tif (:pointer tiff))
  (fillorder int))

(defun tiff-set-fillorder (tif fillorder) (ff-tiff-set-fillorder tif fillorder))

;(declaim (inline set-fillorder))
(defun set-fillorder (tif fillorder)
  (unless (tiff-set-fillorder tif fillorder)
    (error "Error setting FillOrder tag!"))
  fillorder)

				
;; extern int tiff_set_hostcomputer (TIFF *tif, char *hostcomuter);
(def-foreign-function (ff-tiff-set-hostcomputer (:name (fname "tiff_set_hostcomputer"))
						(:return-type int))
  (tif (:pointer tiff))
  (hostcomputer :string))

(defun tiff-set-hostcomputer (tif hostcomputer) (ff-tiff-set-hostcomputer tif hostcomputer))

;(declaim (inline set-hostcomputer))
(defun set-hostcomputer (tif hostcomputer)
  (unless (tiff-set-hostcomputer tif hostcomputer)
    (error "Error setting HostComputer tag!"))
  hostcomputer)

				
;; extern int tiff_set_imagedescription (TIFF *tif, char *imagedescription);
(def-foreign-function (ff-tiff-set-imagedescription (:name (fname "tiff_set_imagedescription"))
						    (:return-type int))
  (tif (:pointer tiff))
  (imagedescription :string))

(defun tiff-set-imagedescription (tif imagedescription) (ff-tiff-set-imagedescription tif imagedescription))

;(declaim (inline set-imagedescription))
(defun set-imagedescription (tif imagedescription)
  (unless (tiff-set-imagedescription tif imagedescription)
    (error "Error setting ImageDescription tag!"))
  imagedescription)

				
;; extern int tiff_set_imagelength (TIFF *tif, long imagelength);
(def-foreign-function (ff-tiff-set-imagelength (:name (fname "tiff_set_imagelength"))
					       (:return-type int))
  (tif (:pointer tiff))
  (imagelength int))

(defun tiff-set-imagelength (tif imagelength) (ff-tiff-set-imagelength tif imagelength))

;(declaim (inline set-imagelength))
(defun set-imagelength (tif imagelength)
  (unless (tiff-set-imagelength tif imagelength)
    (error "Error setting ImageLength tag!"))
  imagelength)

				
;; extern int tiff_set_imagewidth (TIFF *tif, long imagewidth);
(def-foreign-function (ff-tiff-set-imagewidth (:name (fname "tiff_set_imagewidth"))
					      (:return-type int))
  (tif (:pointer tiff))
  (imagewidth int))

(defun tiff-set-imagewidth (tif imagewidth) (ff-tiff-set-imagewidth tif imagewidth))

;(declaim (inline set-imagewidth))
(defun set-imagewidth (tif imagewidth)
  (unless (tiff-set-imagewidth tif imagewidth)
    (error "Error setting ImageWidth tag!"))
  imagewidth)

				
;; extern int tiff_set_make (TIFF *tif, char *make);
(def-foreign-function (ff-tiff-set-make (:name (fname "tiff_set_make"))
					(:return-type int))
  (tif (:pointer tiff))
  (make :string))

(defun tiff-set-make (tif make) (ff-tiff-set-make tif make))

;(declaim (inline set-make))
(defun set-make (tif make)
  (unless (tiff-set-make tif make)
    (error "Error setting Make tag!"))
  make)


;; extern int tiff_set_model (TIFF *tif, char *model);
(def-foreign-function (ff-tiff-set-model (:name (fname "tiff_set_model"))
					 (:return-type int))
  (tif (:pointer tiff))
  (model :string))

(defun tiff-set-model (tif model) (ff-tiff-set-model tif model))

;(declaim (inline set-model))
(defun set-model (tif model)
  (unless (tiff-set-model tif model)
    (error "Error setting Model tag!"))
  model)


;; extern int tiff_set_newsubfiletype (TIFF *tif, long subfiletype);
(def-foreign-function (ff-tiff-set-newsubfiletype (:name (fname "tiff_set_newsubfiletype"))
						  (:return-type int))
  (tif (:pointer tiff))
  (subfiletype int))

(defun tiff-set-newsubfiletype (tif subfiletype) (ff-tiff-set-newsubfiletype tif subfiletype))

;(declaim (inline set-subfiletype))
(defun set-newsubfiletype (tif subfiletype)
  (unless (tiff-set-newsubfiletype tif subfiletype)
    (error "Error setting NewSubFileType tag!"))
  subfiletype)
				

;; extern int tiff_set_pagename (TIFF *tif, char *pagename);
(def-foreign-function (ff-tiff-set-pagename (:name (fname "tiff_set_pagename"))
					    (:return-type int))
  (tif (:pointer tiff))
  (pagename :string))

(defun tiff-set-pagename (tif pagename) (ff-tiff-set-pagename tif pagename))

;(declaim (inline set-pagename))
(defun set-pagename (tif pagename)
  (unless (tiff-set-pagename tif pagename)
    (error "Error setting PageName tag!"))
  pagename)

				
;; extern int tiff_set_pagenumber (TIFF *tif, int currentpage,
;;                                            int wholenumber);
(def-foreign-function (ff-tiff-set-pagenumber (:name (fname "tiff_set_pagenumber"))
					      (:return-type int))
  (tif (:pointer tiff))
  (currentpage int)
  (wholenumber int))

(defun tiff-set-pagenumber (tif currentpage wholenumber) (ff-tiff-set-pagenumber tif currentpage wholenumber))

;(declaim (inline set-pagenumber))
(defun set-pagenumber (tif currentpage wholenumber)
  (unless (tiff-set-pagenumber tif currentpage wholenumber)
    (error "Error setting PageNumber tag!"))
  (values currentpage wholenumber))


;; extern int tiff_set_photometric (TIFF *tif, int photometric);
(def-foreign-function (ff-tiff-set-photometric (:name (fname "tiff_set_photometric"))
					       (:return-type int))
  (tif (:pointer tiff))
  (photometric int))

(defun tiff-set-photometric (tif photometric) (ff-tiff-set-photometric tif photometric))

;(declaim (inline set-photometric))
(defun set-photometric (tif photometric)
  (unless (tiff-set-photometric tif photometric)
    (error "Error setting PhotometricInterpretation tag!"))
  photometric)

				
;; extern int tiff_set_planarconfig (TIFF *tif, int planarconfig);
(def-foreign-function (ff-tiff-set-planarconfig (:name (fname "tiff_set_planarconfig"))
						(:return-type int))
  (tif (:pointer tiff))
  (planarconfig int))

(defun tiff-set-planarconfig (tif planarconfig) (ff-tiff-set-planarconfig tif planarconfig))

;(declaim (inline set-planarconfig))
(defun set-planarconfig (tif planarconfig)
  (unless (tiff-set-planarconfig tif planarconfig)
    (error "Error setting PlanarConfig tag!"))
  planarconfig)

				
;; extern int tiff_set_samplesperpixel (TIFF *tif, int samplesperpixel);
(def-foreign-function (ff-tiff-set-samplesperpixel (:name (fname "tiff_set_samplesperpixel"))
						   (:return-type int))
  (tif (:pointer tiff))
  (samplesperpixel int))

(defun tiff-set-samplesperpixel (tif samplesperpixel) (ff-tiff-set-samplesperpixel tif samplesperpixel))

;(declaim (inline set-samplesperpixel))
(defun set-samplesperpixel (tif samplesperpixel)
  (unless (tiff-set-samplesperpixel tif samplesperpixel)
    (error "Error setting SamplesPerPixel tag!"))
  samplesperpixel)


;; extern int tiff_set_software (TIFF *tif, char *software);
(def-foreign-function (ff-tiff-set-software (:name (fname "tiff_set_software"))
					    (:return-type int))
  (tif (:pointer tiff))
  (software :string))

(defun tiff-set-software (tif software) (ff-tiff-set-software tif software))

;(declaim (inline set-software))
(defun set-software (tif software)
  (unless (tiff-set-software tif software)
    (error "Error setting Software tag!"))
  software)

				
;;; ==============================
;;; Functions for getting TIFFTAGS
;;; ==============================

;; extern unsigned short tiff_get_bitspersample (TIFF *tif);
(def-foreign-function (get-bitspersample (:name (fname "tiff_get_bitspersample"))
					 (:return-type unsigned-int))
  (tif (:pointer tiff)))

				
;; extern int tiff_get_colormap (TIFF *tif,
;;                               short *redcolormap,
;;                               short *greencolormap,
;;                               short *bluecolormap);
(def-foreign-function (ff-tiff-get-colormap (:name (fname "tiff_get_colormap"))
					    (:return-type int))
  (tif (:pointer tiff))
  (redcolormap (:array))
  (greencolormap (:array))
  (bluecolormap (:array)))

(defun tiff-get-colormap (tif redcolormap greencolormap bluecolormap)
  (ff-tiff-get-colormap tif redcolormap greencolormap bluecolormap))

(defun get-colormap (tif)
  (let ((colormap-size (expt 2 (get-bitspersample tif))))
    (let ((red 
	   (obv::allocate-array colormap-size :element-type '(unsigned-byte 16)))
          (green
	   (obv::allocate-array colormap-size :element-type '(unsigned-byte 16)))
          (blue 
	   (obv::allocate-array colormap-size :element-type '(unsigned-byte 16))))
      (if (tiff-get-colormap tif red green blue)
          (values red green blue)
          (values nil nil nil)))))


;; extern unsigned short tiff_get_compression (TIFF *tif);
(def-foreign-function (get-compression (:name (fname "tiff_get_compression"))
				       (:return-type unsigned-int))
  (tif (:pointer tiff)))

;; extern char *tiff_get_datetime (TIFF *tif);
(def-foreign-function (get-datetime (:name (fname "tiff_get_datetime"))
				    (:return-type :simple-string))
  (tif (:pointer tiff)))
				

;; extern char *tiff_get_documentname (TIFF *tif);
(def-foreign-function (get-documentname (:name (fname "tiff_get_documentname"))
					(:return-type :simple-string))
  (tif (:pointer tiff)))


;; extern unsigned short tiff_get_fillorder (TIFF *tif);
(def-foreign-function (get-fillorder (:name (fname "tiff_get_fillorder"))
				     (:return-type unsigned-int))
  (tif (:pointer tiff)))
				

;; extern char *tiff_get_hostcomputer (TIFF *tif);
(def-foreign-function (get-hostcomputer (:name (fname "tiff_get_hostcomputer"))
					(:return-type :simple-string))
  (tif (:pointer tiff)))

				
;; extern char *tiff_get_imagedescription (TIFF *tif);
(def-foreign-function (get-imagedescription (:name (fname "tiff_get_imagedescription"))
					    (:return-type :simple-string))
  (tif (:pointer tiff)))


;; extern int tiff_get_imagelength (TIFF *tif, long *imagelength);
(def-foreign-function (ff-tiff-get-imagelength (:name (fname "tiff_get_imagelength"))
					       (:return-type int))
  (tif (:pointer tiff))
  (imagelength-ptr (:array int (1))))

(defun tiff-get-imagelength (tif)
  (let ((imagelength-ptr (make-array '(1) :element-type '(signed-byte 32))))
    (values (plusp (ff-tiff-get-imagelength tif imagelength-ptr))
	    (aref imagelength-ptr 0))))
				
;(declaim (inline get-imagelength))
(defun get-imagelength (tif)
  (multiple-value-bind (success imagelength)
      (tiff-get-imagelength tif)
    (if success
        imagelength
        (error "Error getting ImageLength tag!"))))


;; extern int tiff_get_imagewidth (TIFF *tif, long *imagewidth);
(def-foreign-function (ff-tiff-get-imagewidth (:name (fname "tiff_get_imagewidth"))
					      (:return-type int))
  (tif (:pointer tiff))
  (imagewidth-ptr (:array int (1))))

(defun tiff-get-imagewidth (tif)
  (let ((imagewidth-ptr (make-array '(1) :element-type '(signed-byte 32))))
    (values (plusp (ff-tiff-get-imagewidth tif imagewidth-ptr))
	    (aref imagewidth-ptr 0))))
    				
;(declaim (inline get-imagewidth))
(defun get-imagewidth (tif)
  (multiple-value-bind (success imagewidth)
      (tiff-get-imagewidth tif)
    (if success
        imagewidth
        (error "Error getting ImageLength tag!"))))


;; extern char *tiff_get_make (TIFF *tif);
(def-foreign-function (get-make (:name (fname "tiff_get_make"))
				(:return-type :simple-string))
  (tif (:pointer tiff)))


;; extern char *tiff_get_model (TIFF *tif);
(def-foreign-function (get-model (:name (fname "tiff_get_model"))
				 (:return-type :simple-string))
  (tif (:pointer tiff)))
				

;; extern long int tiff_get_newsubfiletype (TIFF *tif);
(def-foreign-function (get-newsubfiletype (:name (fname "tiff_get_newsubfiletype"))
					  (:return-type int))
  (tif (:pointer tiff)))
				

;; extern char *tiff_get_pagename (TIFF *tif);
(def-foreign-function (get-pagename (:name (fname "tiff_get_pagename"))
				    (:return-type :simple-string))
  (tif (:pointer tiff)))
				

;; extern int tiff_get_pagenumber (TIFF *tif, int *currentpage,
;;                                            int *wholenumber);
(def-foreign-function (ff-tiff-get-pagenumber (:name (fname "tiff_get_pagenumber"))
					      (:return-type int))
  (tif (:pointer tiff))
  (currentpage-ptr (:array int (1)))
  (wholenumber-ptr (:array int (1))))

(defun tiff-get-pagenumber (tif)
  (let ((currentpage-ptr (make-array '(1) :element-type '(signed-byte 32)))
	(wholenumber-ptr (make-array '(1) :element-type '(signed-byte 32))))
    (values (plusp (ff-tiff-get-pagenumber tif currentpage-ptr wholenumber-ptr))
	    (aref currentpage-ptr 0)
	    (aref wholenumber-ptr 0))))

;(declaim (inline get-pagenumber))
(defun get-pagenumber (tif)
  (multiple-value-bind (success current whole)
      (tiff-get-pagenumber tif)
    (if success
        (values current whole)
        (values nil nil))))


;; extern int tiff_get_photometric (TIFF *tif, int *photometric);
(def-foreign-function (ff-tiff-get-photometric (:name (fname "tiff_get_photometric"))
					       (:return-type int))
  (tif (:pointer tiff))
  (photometric-ptr (:array int (1))))

(defun tiff-get-photometric (tif)
  (let ((photometric-ptr (make-array '(1) :element-type '(signed-byte 32))))
    (values (plusp (ff-tiff-get-photometric tif photometric-ptr))
	    (aref photometric-ptr 0))))
  
;(declaim (inline get-photometric))
(defun get-photometric (tif)
  (multiple-value-bind (success photometric)
      (tiff-get-photometric tif)
    (if success
        photometric
        (error "Error getting PhotometricInterpretation tag!"))))


;; extern unsigned short tiff_get_planarconfig (TIFF *tif);
(def-foreign-function (get-planarconfig (:name (fname "tiff_get_planarconfig"))
					(:return-type unsigned-int))
  (tif (:pointer tiff)))


;; extern unsigned short tiff_get_samplesperpixel (TIFF *tif);
(def-foreign-function (get-samplesperpixel (:name (fname "tiff_get_samplesperpixel"))
					   (:return-type unsigned-int))
  (tif (:pointer tiff)))
				

;; extern char *tiff_get_software (TIFF *tif);
(def-foreign-function (get-software (:name (fname "tiff_get_software"))
				    (:return-type :simple-string))
  (tif (:pointer tiff)))
				

;;; ===============================
;;; Reading and writing TIFF images
;;; ===============================

(def-foreign-function (ff-tiff-get-image-plane (:name (fname "tiff_get_image_plane"))
					       (:return-type int))
  (tif (:pointer tiff))
  (image (:array))
  (image-length int)
  (scanline-size int)
  (sample int))

(defun tiff-get-image-plane (tif image image-length scanline-size sample)
  (ff-tiff-get-image-plane tif image image-length scanline-size sample))
				
(def-foreign-function (ff-tiff-put-image-plane (:name (fname "tiff_put_image_plane"))
					       (:return-type int))
  (tif (:pointer tiff))
  (image (:array))
  (image-length int)
  (scanline-size int)
  (sample int))

(defun tiff-put-image-plane (tif image image-length scanline-size sample)
  (ff-tiff-put-image-plane tif image image-length scanline-size sample))
				 

(defun read-image-plane (tif &optional (sample 0))
  "If the TIFF image is in chunky format (PlanarConfiguration = 1) then
the complete image is read.  If SamplesPerPixel is greater than 1 this means
that all samples are put into one array element.
If the TIFF image is in planar format (PlanarConfiguration = 2) then only 
the plane given by SAMPLE is read.
The image array and the number of samples read will be returned."
  (let ((samples-per-pixel (get-samplesperpixel tif))
        (bits-per-sample (get-bitspersample tif))
        (image-width (get-imagewidth tif))
        (image-length (get-imagelength tif))
        (scanline-size (scanline-size tif))
        (planar-config (get-planarconfig tif)))
    (let* ((bits-per-pixel (if (= planar-config planarconfig-contig)
                               (* bits-per-sample samples-per-pixel)
                               bits-per-sample))
           (bits-per-scanline (* 8 scanline-size))
           (padding-bits (- bits-per-scanline
                            (* image-width bits-per-pixel)))
           (element-type (list 'unsigned-byte bits-per-pixel))
           (upgraded-bits-per-pixel (second (upgraded-array-element-type element-type))))
      (cond (;; Test the proper packing of arrays ...
             (/= bits-per-pixel upgraded-bits-per-pixel)
             (error "Can't make arrays of element-type ~S."
                    element-type))

	    (;; Test the propper padding ...
             ;; *** This should never happen -- CS
             (not (zerop (mod padding-bits bits-per-pixel)))
             (error "The number of padding bits is not a multiple~%~
                     of the number of bits per pixel!~%~
                     Seems to be a strange TIFF-file."))

            (t
             (let* ((padding-pixels (/ padding-bits bits-per-pixel))
                    (image
                     (obv::allocate-array
		      (list image-length
			    (+ image-width padding-pixels))
                              :element-type element-type)))
               (read-image-plane-into tif image sample)))))))


(defun read-image-plane-into (tif image &optional (sample 0))
  "If the TIFF image is in chunky format (PlanarConfiguration = 1) then
the complete image is read.
If the TIFF image is in planar format (PlanarConfiguration = 2) then only 
the plane given by SAMPLE is read.
The given image array and the number of samples read will be returned."
   (let* ((bits-per-sample (get-bitspersample tif))
	 (samples-per-pixel (get-samplesperpixel tif))
	 (image-length (get-imagelength tif))
 	 (scanline-size (scanline-size tif))
 	 (planar-config (get-planarconfig tif))
 	 (fillorder (get-fillorder tif))
 	 (number-of-bytes (* scanline-size image-length))
 	 (number-of-words (ceiling (/ number-of-bytes 4))))
    (cond ((tiff-get-image-plane tif
			         image
			         image-length
			         scanline-size
			         sample)
 	   (when (= bits-per-sample 1)
 	     (when (equal fillorder fillorder-msb2lsb)
 	       ;; The bytes used for implementing Common Lisp bit arrays are
                ;; interpreted in lsb-to-msb order, i.e. the bit vector
                ;; #*10000000 is the integer 1 instead of 128.
                ;; Therefore, we have to reverse the bit order when the data
                ;; in the TIFF file was written as msb2lsb.
                ;; *** Although otherwise stated in the "NOTES" section of the
                ;;     TIFFReadScanline (3T) manual page the TIFF library
                ;;     doesn't perform any bit reversal.  So, we have to 
                ;;     reverse only if the fillorder tag field is set to
                ;;     msb2lsb.  If the library would do the reversing to
                ;;     msb2lsb then we would have to reverse here in any
                ;;     case!
 	       #+(or LCL4.0 LispWorks)
 	       (reverse-bits image number-of-bytes))
 	     ;; In Lucid Common Lisp running on a Sun SPARC the
 	     ;; interpretation of a four-byte word as a bit array
 	     ;; corresponds to a little-endian byte order, therefore
 	     ;; we have to swap the bytes here.
 	     #+LCL4.0
 	     (swap-array-of-long image number-of-words))
	   (values image
		   (if (= planar-config planarconfig-contig)
		       samples-per-pixel
		     1)))
          (t
           (error "Can't read TIFF image plane.")))))


(defun write-image-plane (tif image &optional (sample 0))
  (let ((image-length (array-dimension image 0))
	(image-width  (array-dimension image 1))
	(bits-per-pixel (second (tiff-array-element-type image))))
    (let ((scanline-bit-size (* bits-per-pixel image-width)))
      (when (/= (mod scanline-bit-size 8) 0)
	(error "Can't write TIFF image plane.~%~
                The size of an image line must be a multiple of a byte!"))
      (when (= bits-per-pixel 1)
	(let* ((number-of-bytes (ceiling (/ (* image-length image-width) 8)))
	       (number-of-words (ceiling (/ (* image-length image-width) 32))))
	  ;; The bytes used for implementing Common Lisp bit arrays are
	  ;; interpreted in lsb-to-msb order, i.e. the bit vector
	  ;; #*10000000 is the integer 1 instead of 128.
	  ;; Before writing the data to a TIFF file, we reverse the
	  ;; bit order here to msb2lsb (which is the default). 
	  #+(or LCL4.0 LispWorks)
	  (reverse-bits image number-of-bytes)
	  ;; In Lucid Common Lisp running on a Sun SPARC the
	  ;; interpretation of a four-byte word as a bit array
	  ;; corresponds to a little-endian byte order, therefore
	  ;; we have to swap the bytes here.
	  #+LCL4.0
	  (swap-array-of-long image number-of-words)))
      (if (not (tiff-put-image-plane tif
				     image
				     image-length
				     (ceiling (/ scanline-bit-size 8))
				     sample))
	  (error "Can't write TIFF image plane.")))))


;;; =====================================
;;; Separating contiguous multiple planes
;;; =====================================

(def-foreign-function (tiff-separate-image-planes (:name (fname "tiff_separate_image_planes")))
  (contig (:array))
  (red    (:array))
  (green  (:array))
  (blue   (:array))
  (total-size int))

;(declaim (inline separate-image-planes))
(defun separate-image-planes (contig red green blue)
  (tiff-separate-image-planes contig 
			      red 
			      green 
			      blue
			      (* (array-dimension red 0)
				 (array-dimension red 1))))


;;; ===========================
;;; Loading necessary libraries
;;; ===========================

;;;
;;; Link libtiff.a
;;;

(if (probe-file obv::*tiff-library*)
    (progn
      (obv::load-tiff-library obv::*tiff-library*)
      (obv::load-c-libraries)
      (obv::load-tiff-library obv::*tiff-library*))
    (error "TIFF library ~a not found - check your site-paths.lisp file."
	   obv::*tiff-library*))



;;;; ************************* End of File ************************************
