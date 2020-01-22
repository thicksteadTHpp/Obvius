############################################################################

INCLUDES = /usr/openwin/include
BINARY = ../bin/obv_gl.so

#all: $(BINARY)

CC = gcc
CFLAGS =  -fPIC -U__STRICT_ANSI__ -std=c99 -ltiff
# CFLAGS = -O4 -fsingle  ## Optimize on Sun-4/SPARC
# CFLAGS = -O4 -f68881   ## Sun-3 with standard Floating point co-processor
# CFLAGS = -ffpa         ## Sun-3 with fpa (Weitek) board
# CFLAGS = +x            ## HP Bobcats
# CFLAGS = -O -f          ## DECstations
# gcc -O -c $? -I$(INCLUDES)

CONVOLVE_SRC = 	hex-convolve.c p-convolve.c convolve.c wrap.c edges.c 
FORTRAN_SRC = 	svd.c stepit.c
GL_SRC =	gl-obv-blt.c 
TIFF_SRC =	tiff-access.c
ALL_SRC  =  	$(CONVOLVE_SRC)  $(TIFF_SRC) $(GL_SRC)  \
		matrix.c byteswap.c color.c dither.c  fft.c \
		imops.c 3d-ops.c surface-plot.c \
	        zoom.c warper.c chartohex.c 

OBJECTS=    $(ALL_SRC:.c=.o)
###########################################################################
##     cc -O4 -fsingle -c $? -I$(INCLUDES)

#$(BINARY): $()
#	$(CC) $(CFLAGS) -c $?
#	ar vru $(BINARY) *.o
#	ranlib $(BINARY)
##	/bin/rm -fr *.o

$(CONVOLVE_SRC): convolve.h
	$(CC) $(CFLAGS) -c $(CONVOLVE_SRC)
	touch $(CONVOLVE_SRC)

#$(FORTRAN_SRC): f2c.h
#	$(CC) $(CFLAGS) -c $(FORTRAN_SRC)
#	touch $(FORTRAN_SRC)

.c.o: $(ALL_SRC)
	$(CC) $(CFLAGS) -c $? -o $@

#obj: $(ALL_SRC)
#	$(CC) $(CFLAGS) -c  $(ALL_SRC)  -o $(OBJECTS)

lib: $(OBJECTS)
#	$(CC) $(CFLAGS) -c $? -o obj.o
	$(CC) $(CFLAGS) --shared $? -o $(BINARY)

clean:
	rm *.o
