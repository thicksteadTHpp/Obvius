/********************************************************************/
/* This file contains some C functions used for a Common Lisp       */
/* foreign function interface to Sam Leffler's TIFF library.        */
/* It is based on version 3.00 of this library and it complies      */
/* to TIFF Rev. 6.0.                                                */
/* You can obtain a copy of the library via anonymous ftp from      */
/* ucbvax.berkeley.edu:/pub/tiff/                                   */
/*                                                                  */
/* Written by Harald Lange and Carsten Schroeder                    */
/* University of Hamburg, Dept. of Computer Science, Germany        */
/* September 1992                                                   */
/*                                                                  */
/* Feel free to use it for non-commercial purposes.                 */
/*                                                                  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "tiffio.h"


#define MAXINT                   4294967295  /* 2^32 - 1 */
#define MAX_STRING_LENGTH        256
#define DEFAULT_BITSPERSAMPLE    1
#define DEFAULT_COMPRESSION      1
#define DEFAULT_FILLORDER        1
#define DEFAULT_PLANARCONFIG     1 /* Single Image Plane */
#define DEFAULT_ROWSPERSTRIP     MAXINT
#define DEFAULT_SAMPLESPERPIXEL  1
#define DEFAULT_SUBFILETYPE      0


int tiff_file_p (name) 
     char 	*name;
{
     FILE *file;
     int result;
     struct {
	  short int byte_order;
	  char intel_version;
	  char motorola_version;
     } header;

     if (!(file=fopen(name, "r"))) 
	  result = 0;
     else {
	  if (fread(&header, sizeof(header), 1, file)) {
	       if (header.byte_order == TIFF_BIGENDIAN) 
		    result = (header.motorola_version == TIFF_VERSION);
	       else if (header.byte_order == TIFF_LITTLEENDIAN) 
		    result = (header.intel_version == TIFF_VERSION);
	       else 
		    result = 0;
	  }
	  else
	       result = 0;

	  fclose(file);
     }
    
     return( result );
}


/* Print Information of TIFF-File *tif on stdout */
void tiff_print_dir(tif)
     TIFF	*tif;
{
     TIFFPrintDirectory(tif, stdout, 0);
}

/* ============================== */
/* Functions for setting TIFFTAGS */
/* ============================== */

int tiff_set_bitspersample (tif, bitspersample)
     TIFF 	*tif;
     int	bitspersample;
{
     /* Number of arguments = SamplesPerPixel */
     return TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE,
			 (unsigned short) bitspersample);
}

int tiff_set_colormap (tif, 
		       redcolormap, greencolormap, bluecolormap) 
     TIFF *tif;
     short *redcolormap;
     short *greencolormap;
     short *bluecolormap;
{
     /* Number of elements = 3*(2**BitsPerSample) */
     return TIFFSetField(tif, TIFFTAG_COLORMAP,
			 redcolormap,
			 greencolormap,
			 bluecolormap);
}

int tiff_set_compression (tif, compression) 
     TIFF *tif;
     int compression;
{
     return TIFFSetField(tif, TIFFTAG_COMPRESSION,
			 (unsigned short) compression);
}

int tiff_set_datetime (tif, datetime) 
     TIFF *tif;
     char *datetime;
{
     return TIFFSetField(tif, TIFFTAG_DATETIME, datetime);
}

int tiff_set_documentname (tif, documentname) 
     TIFF *tif;
     char *documentname;
{
     return TIFFSetField(tif, TIFFTAG_DOCUMENTNAME, documentname);
}

int tiff_set_fillorder (tif, fillorder) 
     TIFF *tif;
     int fillorder;
{
     return TIFFSetField(tif, TIFFTAG_FILLORDER, (unsigned short) fillorder);
}

int tiff_set_hostcomputer (tif, hostcomputer) 
     TIFF *tif;
     char *hostcomputer;
{
     return TIFFSetField(tif, TIFFTAG_HOSTCOMPUTER, 
			 hostcomputer);
}

int tiff_set_imagedescription (tif, imagedescription) 
     TIFF *tif;
     char *imagedescription;
{
     return TIFFSetField(tif, TIFFTAG_IMAGEDESCRIPTION, 
			 imagedescription);
}

int tiff_set_imagelength (tif, imagelength) 
     TIFF *tif;
     long imagelength;
{
     return TIFFSetField(tif, TIFFTAG_IMAGELENGTH,
			 (unsigned long) imagelength);
}

int tiff_set_imagewidth (tif, imagewidth) 
     TIFF *tif;
     long imagewidth;
{
     return TIFFSetField(tif, TIFFTAG_IMAGEWIDTH,
			 (unsigned long) imagewidth);
}

int tiff_set_make (tif, make) 
     TIFF *tif;
     char *make;
{
     return TIFFSetField(tif, TIFFTAG_MAKE, make);
}

int tiff_set_model (tif, model) 
     TIFF *tif;
     char *model;
{
     return TIFFSetField(tif, TIFFTAG_MODEL, model);
}

int tiff_set_newsubfiletype (tif, subfiletype) 
     TIFF *tif;
     long subfiletype;
{
     return TIFFSetField(tif, TIFFTAG_SUBFILETYPE, subfiletype);
}
  
int tiff_set_pagename (tif, pagename) 
     TIFF *tif;
     char *pagename;
{
     return TIFFSetField(tif, TIFFTAG_PAGENAME, pagename);
}

int tiff_set_pagenumber (tif, currentpage, wholenumber) 
     TIFF *tif;
     int currentpage;
     int wholenumber;
{
     return TIFFSetField(tif, TIFFTAG_PAGENUMBER,
			 (unsigned short) currentpage,
			 (unsigned short) wholenumber);
}

int tiff_set_photometric (tif, photometric) 
     TIFF *tif;
     int photometric;
{
     return TIFFSetField(tif, TIFFTAG_PHOTOMETRIC,
			 (unsigned short) photometric);
}

int tiff_set_planarconfig (tif, planarconfig) 
     TIFF *tif;
     int planarconfig;
{
     return TIFFSetField(tif, TIFFTAG_PLANARCONFIG,
			 (unsigned short) planarconfig);
}

int tiff_set_samplesperpixel (tif, samplesperpixel) 
     TIFF *tif;
     int samplesperpixel;
{
     return TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL,
			 (unsigned short) samplesperpixel);
}

int tiff_set_software (tif, software) 
     TIFF *tif;
     char *software;
{
     return TIFFSetField(tif, TIFFTAG_SOFTWARE, software);
}

/* ============================== */
/* Functions for getting TIFFTAGS */
/* ============================== */

unsigned short tiff_get_bitspersample (tif) 
     TIFF *tif;
{
     unsigned short tmp_short;

     if (!TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &tmp_short))
	  return (unsigned short) DEFAULT_BITSPERSAMPLE;
     else
	  return tmp_short;
}

int tiff_get_colormap (tif,
		       redcolormap, greencolormap, bluecolormap) 
     TIFF *tif;
     unsigned short *redcolormap;
     unsigned short *greencolormap;
     unsigned short *bluecolormap;
{
     /* Number of elements = 3*(2**BitsPerPixel) */

     return TIFFGetField(tif, TIFFTAG_COLORMAP,
			 &redcolormap, 
			 &greencolormap, 
			 &bluecolormap);
}

int tiff_get_compression (tif) 
     TIFF *tif;
{
     unsigned short tmp_short;

     if (!TIFFGetField(tif, TIFFTAG_COMPRESSION, &tmp_short))
	  return (unsigned short) DEFAULT_COMPRESSION;
     else
	  return tmp_short;
}

char *tiff_get_datetime (tif) 
     TIFF *tif;
{
     char *tmp_string;

     if (!TIFFGetField(tif, TIFFTAG_DATETIME, &tmp_string))
	  return NULL;
     else
	  return tmp_string;
}

char *tiff_get_documentname (tif) 
     TIFF *tif;
{
     char *tmp_string; 

     if (!TIFFGetField(tif, TIFFTAG_DOCUMENTNAME, &tmp_string)) 
	  return NULL;
     else
	  return tmp_string;
}

unsigned short tiff_get_fillorder (tif, fillorder) 
     TIFF *tif;
     int *fillorder;
{
     unsigned short tmp_short;

     if (!TIFFGetField(tif, TIFFTAG_FILLORDER, &tmp_short))
	  return (unsigned short) DEFAULT_FILLORDER;
     else
	  return tmp_short;
}

char *tiff_get_hostcomputer (tif) 
     TIFF *tif;
{
     char *tmp_string; 

     if (!TIFFGetField(tif, TIFFTAG_HOSTCOMPUTER, &tmp_string)) 
	  return NULL;
     else
	  return tmp_string;
}

char *tiff_get_imagedescription (tif) 
     TIFF *tif;
{
     char *tmp_string; 

     if (!TIFFGetField(tif, TIFFTAG_IMAGEDESCRIPTION, &tmp_string))
	  return NULL;
     else
	  return tmp_string;
}

int tiff_get_imagelength (tif, imagelength) 
     TIFF *tif;
     long *imagelength;
{
     int return_value;
     unsigned long tmp_long;

     if (return_value=TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &tmp_long))
	  *imagelength = (long) tmp_long;
     return return_value;
}

int tiff_get_imagewidth (tif, imagewidth) 
     TIFF *tif;
     long *imagewidth;
{
     int return_value;
     unsigned long tmp_long;

     if (return_value=TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &tmp_long))
	  *imagewidth = (long) tmp_long;
     return return_value;
}

char *tiff_get_make (tif) 
     TIFF *tif;
{
     int return_value;
     char *tmp_string;

     if (!(return_value=TIFFGetField(tif, TIFFTAG_MAKE, &tmp_string))) 
	  return NULL;
     else
	  return tmp_string;
}

char *tiff_get_model (tif) 
     TIFF *tif;
{
     char *tmp_string;

     if (!TIFFGetField(tif, TIFFTAG_MODEL, &tmp_string))
	  return NULL;
     else
	  return tmp_string;
}

long int tiff_get_newsubfiletype (tif) 
     TIFF *tif;
{
     long int subfiletype;

     if (!TIFFGetField(tif, TIFFTAG_SUBFILETYPE, &subfiletype))
	  return (long int) DEFAULT_SUBFILETYPE;
     else
	  return subfiletype;
}
  
char *tiff_get_pagename (tif) 
     TIFF *tif;
{
     char *tmp_string;

     if (!TIFFGetField(tif, TIFFTAG_PAGENAME, &tmp_string))
	  return NULL;
     else
	  return tmp_string;
}

int tiff_get_pagenumber (tif, currentpage, wholenumber) 
     TIFF *tif;
     int *currentpage;
     int *wholenumber;
{
     /* Number of arguments = 2 */
     int return_value;
     unsigned short tmp_short_1, tmp_short_2;

     if (return_value=TIFFGetField(tif, TIFFTAG_PAGENUMBER,
				   &tmp_short_1, &tmp_short_2)) {
	  *currentpage = (int) tmp_short_1;
	  *wholenumber = (int) tmp_short_2;
     }
     return return_value;
}

int tiff_get_photometric (tif, photometric) 
     TIFF *tif;
     int *photometric;
{
     int return_value;
     unsigned short tmp_short;

     if (return_value=TIFFGetField(tif, TIFFTAG_PHOTOMETRIC,
				   &tmp_short))
	  *photometric = (int) tmp_short;

     return return_value;
}

unsigned short tiff_get_planarconfig (tif) 
     TIFF *tif;
{
     unsigned short tmp_short;

     if (!TIFFGetField(tif, TIFFTAG_PLANARCONFIG,  &tmp_short))
	  return (unsigned short) DEFAULT_PLANARCONFIG;
     else
	  return tmp_short;
}

unsigned long tiff_get_rowsperstrip (tif) 
     TIFF *tif;
{
     unsigned long rowsperstrip;

     if (!TIFFGetField(tif, TIFFTAG_ROWSPERSTRIP, &rowsperstrip))
	  return (unsigned long) DEFAULT_ROWSPERSTRIP;
     else
	  return rowsperstrip;
}

unsigned short tiff_get_samplesperpixel (tif) 
     TIFF *tif;
{
     unsigned short tmp_short;

     if (!TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &tmp_short))
	  return (unsigned short) DEFAULT_SAMPLESPERPIXEL;
     else
	  return tmp_short;
}

char *tiff_get_software (tif) 
     TIFF *tif;
{
     char *tmp_string;

     if (!TIFFGetField(tif, TIFFTAG_SOFTWARE, &tmp_string))
	  return NULL;
     else
	  return tmp_string;
}

/* ===================================================== */
/* Functions for reading and writing whole images planes */
/* ===================================================== */

int tiff_get_image_plane (tif, 
			  image, image_length,
			  scanline_size, sample) 
     TIFF *tif; 
     unsigned char *image; 
     int image_length;
     int scanline_size;
     int sample;
{
     long row;

     for (row=0; row < image_length; row++, image+=scanline_size) {
	  if (TIFFReadScanline(tif, image, row, sample) == -1) {
	       fprintf(stderr, "Error reading line %i!\n", row);
	       return 0;
	  }
     }
  
     return 1;
}

int tiff_put_image_plane (tif,
			  image, image_length,
			  scanline_size, sample) 
     TIFF *tif;
     unsigned char *image;
     int image_length;
     int scanline_size;
     int sample;
{
     short planar_config;
     long row;

     if (!TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &planar_config)) {
	  fprintf(stderr, "TIFFTAG_PLANARCONFIG is not set\n");
	  return 0;
     }

     for (row=0; row < image_length; row++, image+=scanline_size) {
	  if (!TIFFWriteScanline(tif, image, row, sample)) {
	       fprintf(stderr, "Error writing row %i!\n", row);
	       return 0;
	  }
     }
   
     return 1;
}

/* ===================================== */
/* Separating contiguous multiple planes */
/* ===================================== */

void tiff_separate_image_planes (contig,
				 red, green, blue,
				 total_size) 
     unsigned char *contig;
     unsigned char *red;
     unsigned char *green;
     unsigned char *blue;
     int total_size;
{
     long n;

     for (n=0; n < total_size; n++) {
	  *red++   = *contig++;
	  *green++ = *contig++;
	  *blue++  = *contig++;
     }
}

/********************************************************************/
