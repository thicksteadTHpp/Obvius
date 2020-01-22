/* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: byteswap.c
;;;  Author: Sokolov/Simoncelli
;;;  Description:
;;;  Creation Date:
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

byteswap(a, size)
  register unsigned long *a;
  int size;
  {
  register unsigned long temp, *end = a + size;
  while (a < end)
      {
      temp = *a;
      *a++ = temp<<24 | temp>>24 | temp<<8 & 0xff<<16 | temp>>8 & 0xff<<8;
      }
  }
  
swapcopy(from, to, size)
  register unsigned long *from, *to;
  int size;
  {
  register unsigned long temp, *end = to+size;
  while(to<end)
      {
      temp = *from++;
      *to++ =  temp<<24 | temp>>24 | temp<<8 & 0xff<<16 | temp>>8 & 0xff<<8;
      }
  }
  
#define MAX_MASK (1 << (8 * sizeof(int) - 1))
#define HALF_VAL (1 << (4 * sizeof(int)))

/* Copy arr into res, reversing the order of the bits in each int.
   arr cannot be equal to res */
copy_swapped_32bit(arr,res,size)
  register unsigned int *arr, *res;
  int size;
  {
  register unsigned int temp, mask1, mask2;
  register unsigned int *arr_end;

  for (arr_end = arr+size; arr<arr_end; arr++, res++)
      {
      temp = 0;
      for (mask1=1, mask2=MAX_MASK; mask1<HALF_VAL; mask1<<=1, mask2>>=1)
	  {
	  if (mask1 & *arr) temp |= mask2;
	  if (mask2 & *arr) temp |= mask1;
	  }
      *res = temp;
      }
  }


/* Local Variables: */
/* buffer-read-only: t */
/* End: */
