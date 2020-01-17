;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  gl-device-constants.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package 'gl)

;;;
;;; Device name space partitioning
;;;
;;;  0x0000 -> 0x0fff	devices defined by SGI
;;;    0x0001 -> 0x00ff	    buttons
;;;    0x0100 -> 0x01ff	    valuators
;;;    0x0200 -> 0x02ff	    pseudo devices
;;;    0x0300 -> 0x0eff	    reserved
;;;    0x0f00 -> 0x0fff	    additional buttons
;;;
;;;  0x1000 -> 0x7fff	devices defined by users
;;;    0x1000 -> 0x2fff	    buttons
;;;    0x3000 -> 0x3fff	    valuators
;;;    0x4000 -> 0x7fff	    pseudo devices
;;;
;;;  0x8000 -> 0xffff	can not be used
;;;

;;; macros to test valuator and button numbers

;;; #define ISBUTTON(b)	(((b) >= BUTOFFSET) && ((b) < VALOFFSET) || \
;;;			 ((b) >= BUT2OFFSET) && ((b) < USERVALOFFSET))
(defmacro isbutton (b)
  `(or (and (>= ,b butoffset) (< ,b valoffset))
    (and (>= ,b but2offset) (< ,b uservaloffset))))
    
;;; #define ISVALUATOR(b)	(((b) >= VALOFFSET) && ((b) < PSEUDOFFSET) || \
;;; 			         ((b) >= USERVALOFFSET) && ((b) < USERPSEUDOFFSET))
(defmacro isvaluator (b)
  `(or (and (>= ,b valoffset) (< ,b pseudoffset))
    (and (>= ,b uservaloffset) (< ,b userpseudoffset))))

;;; #define ISTIMER(t)	(((t) >= TIMOFFSET) && ((t) < (TIMCOUNT+TIMOFFSET)))
(defmacro istimer (tt) `(and (>= ,tt timoffset) (< ,tt (+ timcount timoffset))))

;;; #define ISDIAL(t)	(((t) >= DIAL0) && ((t) <= DIAL8))
(defmacro isdial (tt) `(and (>= ,tt dial0) (<= ,tt dial8)))

;;; #define ISLPEN(t)	(((t) == LPENX) || ((t) == LPENY))
(defmacro islpen (tt) `(or (= ,tt lpenx) (= ,tt lpeny)))

;;; #define ISLPENBUT(t)	((t) == LPENBUT)
(defmacro islpenbut(tt) `(= ,tt lpenbut))

;;; #define ISBPADBUT(t)	(((t) >= BPAD0) && ((t) <= BPAD3))
(defmacro isbpadbut (tt) `(and (>= ,tt bpad0) (<= ,tt bpad3)))

;;; #define ISSW(t)		(((t) >= SW0) && ((t) <= SW31))
(defmacro issw (tt) `(and (>= ,tt sw0) (<= ,tt sw31)))

;;; #define ISSTDKEYBD(t)	(((t) >= BUT0) && ((t) <= MAXKBDBUT))
(defmacro isstdkeybd (tt) `(and (>= ,tt but0) (<= ,tt maxkbdbut)))

;;; #define ISXKEYBD(t)	(((t) >= XKBDOFFSET) && ((t) < (XKBDCOUNT+XKBDOFFSET)))
(defmacro isxkeybd (tt) `(and (>= ,tt xkbdoffset) (< ,tt (+ xkbdcount xkbdoffset))))

;;; #define ISKEYBD(t)	(ISSTDKEYBD(t) || ISXKEYBD(t))
(defmacro iskeybd (tt) `(or (isstdkeybd ,tt) (isxkeybd ,tt)))

;;; #define ISSBALL(t)	(((t) >= SBTX) && ((t) <= SBPERIOD))
(defmacro issball (tt) `(and (>= ,tt sbtx) (<= ,tt sbperiod)))

;;; #define ISSBALLBUT(t)	(((t) >= SBPICK) && ((t) <= SBBUT8))
(defmacro issballbut (tt) `(and (<= ,tt sbpick) (>= ,tt sbbut8)))


(def-exported-constant "NULLDEV"      0)
(def-exported-constant "BUTOFFSET"    1)
(def-exported-constant "VALOFFSET"    256)		;; 0x0100
(def-exported-constant "PSEUDOFFSET"  512)		;; 0x0200
(def-exported-constant "BUT2OFFSET"	3840)		;; 0x0f00

(def-exported-constant "TIMOFFSET"	515)
(def-exported-constant "XKBDOFFSET"	143)

(def-exported-constant "BUTCOUNT"	255)
(def-exported-constant "VALCOUNT"	256)

(def-exported-constant "TIMCOUNT"	4)
(def-exported-constant "XKBDCOUNT"	28)

(def-exported-constant "USERBUTOFFSET"	4096)		;; 0x1000
(def-exported-constant "USERVALOFFSET"	12288)		;; 0x3000
(def-exported-constant "USERPSEUDOFFSET"	16384)		;; 0x4000

;;;
;;; Button definitions for the base US keyboards
;;;
;;;                    button         button      kbd
;;;                    number         offset      hex  key
;;;                    ======       ===========   ===  =====
;;;
(def-exported-constant "BUT0"		 1)	;; 0+BUTOFFSET,   0, "break" (83-key)
(def-exported-constant "BUT1"		 2)	;; 1+BUTOFFSET,   1, "setup" (83-key)
(def-exported-constant "BUT2"		 3)	;; 2+BUTOFFSET,   2, "left ctrl" 	
(def-exported-constant "BUT3"		 4)	;; 3+BUTOFFSET,   3, "caps lock" 	
(def-exported-constant "BUT4"		 5)	;; 4+BUTOFFSET,   4, "right shift" 	
(def-exported-constant "BUT5"		 6)	;; 5+BUTOFFSET,   5, "left shift" 	
(def-exported-constant "BUT6"		 7)	;; 6+BUTOFFSET,   6, "escape"		
(def-exported-constant "BUT7"		 8)	;; 7+BUTOFFSET,   7, "1"		
(def-exported-constant "BUT8"		 9)	;; 8+BUTOFFSET,   8, "tab"		
(def-exported-constant "BUT9"		10)	;; 9+BUTOFFSET,   9, "Q"		
(def-exported-constant "BUT10"	11)	;; 10+BUTOFFSET,  A, "A"		
(def-exported-constant "BUT11"	12)	;; 11+BUTOFFSET,  B, "S"		
(def-exported-constant "BUT12"	13)	;; 12+BUTOFFSET,  C, "no scroll" (83-key)
(def-exported-constant "BUT13"	14)	;; 13+BUTOFFSET,  D, "2"		
(def-exported-constant "BUT14"	15)	;; 14+BUTOFFSET,  E, "3"		
(def-exported-constant "BUT15"	16)	;; 15+BUTOFFSET,  F, "W"		
(def-exported-constant "BUT16"	17)	;; 16+BUTOFFSET, 10, "E"		
(def-exported-constant "BUT17"	18)	;; 17+BUTOFFSET, 11, "D"		
(def-exported-constant "BUT18"	19)	;; 18+BUTOFFSET, 12, "F"		
(def-exported-constant "BUT19"	20)	;; 19+BUTOFFSET, 13, "Z"		
(def-exported-constant "BUT20"	21)	;; 20+BUTOFFSET, 14, "X"		
(def-exported-constant "BUT21"	22)	;; 21+BUTOFFSET, 15, "4"		
(def-exported-constant "BUT22"	23)	;; 22+BUTOFFSET, 16, "5"		
(def-exported-constant "BUT23"	24)	;; 23+BUTOFFSET, 17, "R"		
(def-exported-constant "BUT24"	25)	;; 24+BUTOFFSET, 18, "T"		
(def-exported-constant "BUT25"	26)	;; 25+BUTOFFSET, 19, "G"		
(def-exported-constant "BUT26"	27)	;; 26+BUTOFFSET, 1A, "H"		
(def-exported-constant "BUT27"	28)	;; 27+BUTOFFSET, 1B, "C"		
(def-exported-constant "BUT28"	29)	;; 28+BUTOFFSET, 1C, "V"		
(def-exported-constant "BUT29"	30)	;; 29+BUTOFFSET, 1D, "6"		
(def-exported-constant "BUT30"	31)	;; 30+BUTOFFSET, 1E, "7"		
(def-exported-constant "BUT31"	32)	;; 31+BUTOFFSET, 1F, "Y"		
(def-exported-constant "BUT32"	33)	;; 32+BUTOFFSET, 20, "U"		
(def-exported-constant "BUT33"	34)	;; 33+BUTOFFSET, 21, "J"		
(def-exported-constant "BUT34"	35)	;; 34+BUTOFFSET, 22, "K"		
(def-exported-constant "BUT35"	36)	;; 35+BUTOFFSET, 23, "B"		
(def-exported-constant "BUT36"	37)	;; 36+BUTOFFSET, 24, "N"		
(def-exported-constant "BUT37"	38)	;; 37+BUTOFFSET, 25, "8"		
(def-exported-constant "BUT38"	39)	;; 38+BUTOFFSET, 26, "9"		
(def-exported-constant "BUT39"	40)	;; 39+BUTOFFSET, 27, "I"		
(def-exported-constant "BUT40"	41)	;; 40+BUTOFFSET, 28, "O"		
(def-exported-constant "BUT41"	42)	;; 41+BUTOFFSET, 29, "L"		
(def-exported-constant "BUT42"	43)	;; 42+BUTOFFSET, 2A, ";"		
(def-exported-constant "BUT43"	44)	;; 43+BUTOFFSET, 2B, "M"		
(def-exported-constant "BUT44"	45)	;; 44+BUTOFFSET, 2C, ","		
(def-exported-constant "BUT45"	46)	;; 45+BUTOFFSET, 2D, "0"		
(def-exported-constant "BUT46"	47)	;; 46+BUTOFFSET, 2E, "-"		
(def-exported-constant "BUT47"	48)	;; 47+BUTOFFSET, 2F, "P"		
(def-exported-constant "BUT48"	49)	;; 48+BUTOFFSET, 30, "["		
(def-exported-constant "BUT49"	50)	;; 49+BUTOFFSET, 31, "'"		
(def-exported-constant "BUT50"	51)	;; 50+BUTOFFSET, 32, "return"		
(def-exported-constant "BUT51"	52)	;; 51+BUTOFFSET, 33, "."		
(def-exported-constant "BUT52"	53)	;; 52+BUTOFFSET, 34, "/"
(def-exported-constant "BUT53"	54)	;; 53+BUTOFFSET, 35, "="		
(def-exported-constant "BUT54"	55)	;; 54+BUTOFFSET, 36, "`"		
(def-exported-constant "BUT55"	56)	;; 55+BUTOFFSET, 37, "]"		
(def-exported-constant "BUT56"	57)	;; 56+BUTOFFSET, 38, "\"		
(def-exported-constant "BUT57"	58)	;; 57+BUTOFFSET, 39, num pad "1"	
(def-exported-constant "BUT58"	59)	;; 58+BUTOFFSET, 3A, num pad "0"	
(def-exported-constant "BUT59"	60)	;; 59+BUTOFFSET, 3B, "line feed" (83-key) 
(def-exported-constant "BUT60"	61)	;; 60+BUTOFFSET, 3C, "back space"	
(def-exported-constant "BUT61"	62)	;; 61+BUTOFFSET, 3D, "delete"		
(def-exported-constant "BUT62"	63)	;; 62+BUTOFFSET, 3E, num pad "4"	
(def-exported-constant "BUT63"	64)	;; 63+BUTOFFSET, 3F, num pad "2"	
(def-exported-constant "BUT64"	65)	;; 64+BUTOFFSET, 40, num pad "3"	
(def-exported-constant "BUT65"	66)	;; 65+BUTOFFSET, 41, num pad "."	
(def-exported-constant "BUT66"	67)	;; 66+BUTOFFSET, 42, num pad "7"	
(def-exported-constant "BUT67"	68)	;; 67+BUTOFFSET, 43, num pad "8"	
(def-exported-constant "BUT68"	69)	;; 68+BUTOFFSET, 44, num pad "5"	
(def-exported-constant "BUT69"	70)	;; 69+BUTOFFSET, 45, num pad "6"	
(def-exported-constant "BUT70"	71)	;; 70+BUTOFFSET, 46, num pad "pf2" (83-key) 
(def-exported-constant "BUT71"	72)	;; 71+BUTOFFSET, 47, num pad "pf1" (83-key) 
(def-exported-constant "BUT72"	73)	;; 72+BUTOFFSET, 48, "left arrow"	
(def-exported-constant "BUT73"	74)	;; 73+BUTOFFSET, 49, "down arrow"	
(def-exported-constant "BUT74"	75)	;; 74+BUTOFFSET, 4A, num pad "9"	
(def-exported-constant "BUT75"	76)	;; 75+BUTOFFSET, 4B, num pad "-"	
(def-exported-constant "BUT76"	77)	;; 76+BUTOFFSET, 4C, num pad "," (83-key) 
(def-exported-constant "BUT77"	78)	;; 77+BUTOFFSET, 4D, num pad "pf4" (83-key) 
(def-exported-constant "BUT78"	79)	;; 78+BUTOFFSET, 4E, num pad "pf3" (83-key) 
(def-exported-constant "BUT79"	80)	;; 79+BUTOFFSET, 4F, "right arrow"	
(def-exported-constant "BUT80"	81)	;; 80+BUTOFFSET, 50, "up arrow"		
(def-exported-constant "BUT81"	82)	;; 81+BUTOFFSET, 51, num pad "enter"	
(def-exported-constant "BUT82"	83)	;; 82+BUTOFFSET, 52, "space"		
(def-exported-constant "MAXKBDBUT"	83)	;; BUT82 

;; Mouse buttons, etc. 
(def-exported-constant "BUT100"	101)	;; 100+BUTOFFSET, Mouse button 1	
(def-exported-constant "BUT101"	102)	;; 101+BUTOFFSET, Mouse button 2	
(def-exported-constant "BUT102"	103)	;; 102+BUTOFFSET, Mouse button 3	
(def-exported-constant "BUT103"	104)	;;		  Light Pen Button	
(def-exported-constant "BUT104"	105)	;;		  Bitpad Button 0	
(def-exported-constant "BUT105"	106)	;;		  Bitpad Button 1	
(def-exported-constant "BUT106"	107)	;;		  Bitpad Button 2	
(def-exported-constant "BUT107"	108)	;;		  Bitpad Button 3	
(def-exported-constant "BUT108"	109)	;;		  Light Pen Valid	
(def-exported-constant "BUT109"	110)	;;		  UNUSED		

;; Button box definitions 
(def-exported-constant "BUT110"	111)	;; 110+BUTOFFSET, Button box switch 0	
(def-exported-constant "BUT111"	112)	;; 111+BUTOFFSET, Button box switch 1	
(def-exported-constant "BUT112"	113)	;; 112+BUTOFFSET, Button box switch 2	
(def-exported-constant "BUT113"	114)	;; 113+BUTOFFSET, Button box switch 3	
(def-exported-constant "BUT114"	115)	;; 114+BUTOFFSET, Button box switch 4	
(def-exported-constant "BUT115"	116)	;; 115+BUTOFFSET, Button box switch 5	
(def-exported-constant "BUT116"	117)	;; 116+BUTOFFSET, Button box switch 6	
(def-exported-constant "BUT117"	118)	;; 117+BUTOFFSET, Button box switch 7	
(def-exported-constant "BUT118"	119)	;; 118+BUTOFFSET, Button box switch 8	
(def-exported-constant "BUT119"	120)	;; 119+BUTOFFSET, Button box switch 9	
(def-exported-constant "BUT120"	121)	;; 120+BUTOFFSET, Button box switch 10	
(def-exported-constant "BUT121"	122)	;; 121+BUTOFFSET, Button box switch 11	
(def-exported-constant "BUT122"	123)	;; 122+BUTOFFSET, Button box switch 12	
(def-exported-constant "BUT123"	124)	;; 123+BUTOFFSET, Button box switch 13	
(def-exported-constant "BUT124"	125)	;; 124+BUTOFFSET, Button box switch 14	
(def-exported-constant "BUT125"	126)	;; 125+BUTOFFSET, Button box switch 15	
(def-exported-constant "BUT126"	127)	;; 126+BUTOFFSET, Button box switch 16	
(def-exported-constant "BUT127"	128)	;; 127+BUTOFFSET, Button box switch 17	
(def-exported-constant "BUT128"	129)	;; 128+BUTOFFSET, Button box switch 18	
(def-exported-constant "BUT129"	130)	;; 129+BUTOFFSET, Button box switch 19	
(def-exported-constant "BUT130"	131)	;; 130+BUTOFFSET, Button box switch 20	
(def-exported-constant "BUT131"	132)	;; 131+BUTOFFSET, Button box switch 21	
(def-exported-constant "BUT132"	133)	;; 132+BUTOFFSET, Button box switch 22	
(def-exported-constant "BUT133"	134)	;; 133+BUTOFFSET, Button box switch 23	
(def-exported-constant "BUT134"	135)	;; 134+BUTOFFSET, Button box switch 24	
(def-exported-constant "BUT135"	136)	;; 135+BUTOFFSET, Button box switch 25	
(def-exported-constant "BUT136"	137)	;; 136+BUTOFFSET, Button box switch 26	
(def-exported-constant "BUT137"	138)	;; 137+BUTOFFSET, Button box switch 27	
(def-exported-constant "BUT138"	139)	;; 138+BUTOFFSET, Button box switch 28	
(def-exported-constant "BUT139"	140)	;; 139+BUTOFFSET, Button box switch 29	
(def-exported-constant "BUT140"	141)	;; 140+BUTOFFSET, Button box switch 30	
(def-exported-constant "BUT141"	142)	;; 141+BUTOFFSET, Button box switch 31	

;;; Button definitions for the extended keyboard.  Although current keyboards
;;; are 101 or 102 keys, there are 112 positions and so that many values are
;;; reserved.
;;;
;;;                           button         button      kbd
;;;                           number         offset      hex  key
;;;                           ======       ===========   ===  ===== 
(def-exported-constant "BUT142"	143)	;; 142+BUTOFFSET, 53 "left ALT"		
(def-exported-constant "BUT143"	144)	;; 143+BUTOFFSET, 54 "right ALT"	
(def-exported-constant "BUT144"	145)	;; 144+BUTOFFSET, 55 "right ctrl"	
(def-exported-constant "BUT145"	146)	;; 145+BUTOFFSET, 56 "F1"		
(def-exported-constant "BUT146"	147)	;; 146+BUTOFFSET, 57 "F2"		
(def-exported-constant "BUT147"	148)	;; 147+BUTOFFSET, 58 "F3"		
(def-exported-constant "BUT148"	149)	;; 148+BUTOFFSET, 59 "F4"		
(def-exported-constant "BUT149"	150)	;; 149+BUTOFFSET, 5A "F5"		
(def-exported-constant "BUT150"	151)	;; 150+BUTOFFSET, 5B "F6"		
(def-exported-constant "BUT151"	152)	;; 151+BUTOFFSET, 5C "F7"		
(def-exported-constant "BUT152"	153)	;; 152+BUTOFFSET, 5D "F8"		
(def-exported-constant "BUT153"	154)	;; 153+BUTOFFSET, 5E "F9"		
(def-exported-constant "BUT154"	155)	;; 154+BUTOFFSET, 5F "F10"		
(def-exported-constant "BUT155"	156) 	;; 155+BUTOFFSET, 60 "F11"		
(def-exported-constant "BUT156"	157)	;; 156+BUTOFFSET, 61 "F12"		
(def-exported-constant "BUT157"	158)	;; 157+BUTOFFSET, 62 "print screen"	
(def-exported-constant "BUT158"	159)	;; 158+BUTOFFSET, 63 "scroll lock"	
(def-exported-constant "BUT159"	160)	;; 159+BUTOFFSET, 64 "pause"		
(def-exported-constant "BUT160"	161)	;; 160+BUTOFFSET, 65 "insert"		
(def-exported-constant "BUT161"	162)	;; 161+BUTOFFSET, 66 "home"		
(def-exported-constant "BUT162"	163)	;; 162+BUTOFFSET, 67 "page up"		
(def-exported-constant "BUT163"	164)	;; 163+BUTOFFSET, 68 "end"		
(def-exported-constant "BUT164"	165)	;; 164+BUTOFFSET, 69 "page down"	
(def-exported-constant "BUT165"	166)	;; 165+BUTOFFSET, 6A "num lock"		
(def-exported-constant "BUT166"	167)	;; 166+BUTOFFSET, 6B num pad "/"	
(def-exported-constant "BUT167"	168)	;; 167+BUTOFFSET, 6C num pad "*"	
(def-exported-constant "BUT168"	169)	;; 168+BUTOFFSET, 6D num pad "+"	

;;;
;;; BUT169 (=170) through BUT179 (=180) are reserved for the remainder of
;;; the 112 key positions.
 

;;;
;;; BUT181 through BUT189 are used for the nine buttons of the Space Ball.
;;;
;;; Codes through 255 inclusive are reserved for future use by SGI.
 
(def-exported-constant "BUT181"	182)	;; 181+BUTOFFSET, space ball button 0	
(def-exported-constant "BUT182"	183)	;; 182+BUTOFFSET, space ball button 1	
(def-exported-constant "BUT183"	184)	;; 183+BUTOFFSET, space ball button 2	
(def-exported-constant "BUT184"	185)	;; 184+BUTOFFSET, space ball button 3	
(def-exported-constant "BUT185"	186)	;; 185+BUTOFFSET, space ball button 4	
(def-exported-constant "BUT186"	187)	;; 186+BUTOFFSET, space ball button 5	
(def-exported-constant "BUT187"	188)	;; 187+BUTOFFSET, space ball button 6	
(def-exported-constant "BUT188"	189)	;; 188+BUTOFFSET, space ball button 7	
(def-exported-constant "BUT189"	190)	;; 189+BUTOFFSET, space ball button 8	

;;; other buttons 

(def-exported-constant "MOUSE1"	101)	;; BUT100 
(def-exported-constant "MOUSE2"	102)	;; BUT101 
(def-exported-constant "MOUSE3"	103)	;; BUT102 
(def-exported-constant "LEFTMOUSE"	103)	;; BUT102 
(def-exported-constant "MIDDLEMOUSE"	102)	;; BUT101 
(def-exported-constant "RIGHTMOUSE"	101)	;; BUT100 
(def-exported-constant "LPENBUT"	104)	;; LIGHT PEN BUTTON 
(def-exported-constant "BPAD0"	105)	;; BITPAD BUTTON 0 
(def-exported-constant "BPAD1"	106)	;; BITPAD BUTTON 1 
(def-exported-constant "BPAD2"	107)	;; BITPAD BUTTON 2 
(def-exported-constant "BPAD3"	108)	;; BITPAD BUTTON 3 
(def-exported-constant "LPENVALID"	109)	;; LIGHT PEN VALID 

;;; button box 

(def-exported-constant "SWBASE"	111)	;; BUT110 
(def-exported-constant "SW0"		111)	;; SWBASE 
(def-exported-constant "SW1"		112)	;; SWBASE+1 
(def-exported-constant "SW2"		113)	;; SWBASE+2 
(def-exported-constant "SW3"		114)	;; SWBASE+3 
(def-exported-constant "SW4"		115)	;; SWBASE+4 
(def-exported-constant "SW5"		116)	;; SWBASE+5 
(def-exported-constant "SW6"		117)	;; SWBASE+6 
(def-exported-constant "SW7"		118)	;; SWBASE+7 
(def-exported-constant "SW8"		119)	;; SWBASE+8 
(def-exported-constant "SW9"		120)	;; SWBASE+9 
(def-exported-constant "SW10"		121)	;; SWBASE+10 
(def-exported-constant "SW11"		122)	;; SWBASE+11 
(def-exported-constant "SW12"		123)	;; SWBASE+12 
(def-exported-constant "SW13"		124)	;; SWBASE+13 
(def-exported-constant "SW14"		125)	;; SWBASE+14 
(def-exported-constant "SW15"		126)	;; SWBASE+15 
(def-exported-constant "SW16"		127)	;; SWBASE+16 
(def-exported-constant "SW17"		128)	;; SWBASE+17 
(def-exported-constant "SW18"		129)	;; SWBASE+18 
(def-exported-constant "SW19"		130)	;; SWBASE+19 
(def-exported-constant "SW20"		131)	;; SWBASE+20 
(def-exported-constant "SW21"		132)	;; SWBASE+21 
(def-exported-constant "SW22"		133)	;; SWBASE+22 
(def-exported-constant "SW23"		134)	;; SWBASE+23 
(def-exported-constant "SW24"		135)	;; SWBASE+24 
(def-exported-constant "SW25"		136)	;; SWBASE+25 
(def-exported-constant "SW26"		137)	;; SWBASE+26 
(def-exported-constant "SW27"		138)	;; SWBASE+27 
(def-exported-constant "SW28"		139)	;; SWBASE+28 
(def-exported-constant "SW29"		140)	;; SWBASE+29 
(def-exported-constant "SW30"		141)	;; SWBASE+30 
(def-exported-constant "SW31"		142)	;; SWBASE+31 

;;; space ball buttons 

(def-exported-constant "SBBASE"	182)	;; BUT181 
(def-exported-constant "SBPICK"	182)	;; SBBASE 
(def-exported-constant "SBBUT1"	183)	;; SBBASE+1 
(def-exported-constant "SBBUT2"	184)	;; SBBASE+2 
(def-exported-constant "SBBUT3"	185)	;; SBBASE+3 
(def-exported-constant "SBBUT4"	186)	;; SBBASE+4 
(def-exported-constant "SBBUT5"	187)	;; SBBASE+5 
(def-exported-constant "SBBUT6"	188)	;; SBBASE+6 
(def-exported-constant "SBBUT7"	189)	;; SBBASE+7 
(def-exported-constant "SBBUT8"	190)	;; SBBASE+8 

;;; standard keyboard 

(def-exported-constant "AKEY"		11)	;; BUT10 
(def-exported-constant "BKEY"		36)	;; BUT35 
(def-exported-constant "CKEY"		28)	;; BUT27 
(def-exported-constant "DKEY"		18)	;; BUT17 
(def-exported-constant "EKEY"		17)	;; BUT16 
(def-exported-constant "FKEY"		19)	;; BUT18 
(def-exported-constant "GKEY"		26)	;; BUT25 
(def-exported-constant "HKEY"		27)	;; BUT26 
(def-exported-constant "IKEY"		40)	;; BUT39 
(def-exported-constant "JKEY"		34)	;; BUT33 
(def-exported-constant "KKEY"		35)	;; BUT34 
(def-exported-constant "LKEY"		42)	;; BUT41 
(def-exported-constant "MKEY"		44)	;; BUT43 
(def-exported-constant "NKEY"		37)	;; BUT36 
(def-exported-constant "OKEY"		41)	;; BUT40 
(def-exported-constant "PKEY"		48)	;; BUT47 
(def-exported-constant "QKEY"		10)	;; BUT9 
(def-exported-constant "RKEY"		24)	;; BUT23 
(def-exported-constant "SKEY"		12)	;; BUT11 
(def-exported-constant "TKEY"		25)	;; BUT24 
(def-exported-constant "UKEY"		33)	;; BUT32 
(def-exported-constant "VKEY"		29)	;; BUT28 
(def-exported-constant "WKEY"		16)	;; BUT15 
(def-exported-constant "XKEY"		21)	;; BUT20 
(def-exported-constant "YKEY"		32)	;; BUT31 
(def-exported-constant "ZKEY"		20)	;; BUT19 
(def-exported-constant "ZEROKEY"	46)	;; BUT45 
(def-exported-constant "ONEKEY"	8)	;; BUT7 
(def-exported-constant "TWOKEY"	14)	;; BUT13 
(def-exported-constant "THREEKEY"	15)	;; BUT14 
(def-exported-constant "FOURKEY"	22)	;; BUT21 
(def-exported-constant "FIVEKEY"	23)	;; BUT22 
(def-exported-constant "SIXKEY"	30)	;; BUT29 
(def-exported-constant "SEVENKEY"	31)	;; BUT30 
(def-exported-constant "EIGHTKEY"	38)	;; BUT37 
(def-exported-constant "NINEKEY"	39)	;; BUT38 
(def-exported-constant "BREAKKEY"	1)	;; BUT0 
(def-exported-constant "SETUPKEY"	2)	;; BUT1 
(def-exported-constant "CTRLKEY"	3)	;; BUT2 
(def-exported-constant "LEFTCTRLKEY"	CTRLKEY)	;; BUT2 
(def-exported-constant "CAPSLOCKKEY"	4)	;; BUT3 
(def-exported-constant "RIGHTSHIFTKEY"	5)	;; BUT4 
(def-exported-constant "LEFTSHIFTKEY"	6)	;; BUT5 
(def-exported-constant "NOSCRLKEY"	13)	;; BUT12 
(def-exported-constant "ESCKEY"	7)	;; BUT6 
(def-exported-constant "TABKEY"	9)	;; BUT8 
(def-exported-constant "RETKEY"	51)	;; BUT50 
(def-exported-constant "SPACEKEY"	83)	;; BUT82 
(def-exported-constant "LINEFEEDKEY"	60)	;; BUT59 
(def-exported-constant "BACKSPACEKEY"	61)	;; BUT60 
(def-exported-constant "DELKEY"	62)	;; BUT61 
(def-exported-constant "SEMICOLONKEY"	43)	;; BUT42 
(def-exported-constant "PERIODKEY"	52)	;; BUT51 
(def-exported-constant "COMMAKEY"	45)	;; BUT44 
(def-exported-constant "QUOTEKEY"	50)	;; BUT49 
(def-exported-constant "ACCENTGRAVEKEY"	55)	;; BUT54 
(def-exported-constant "MINUSKEY"	47)	;; BUT46 
(def-exported-constant "VIRGULEKEY"	53)	;; BUT52 
(def-exported-constant "BACKSLASHKEY"	57)	;; BUT56 
(def-exported-constant "EQUALKEY"	54)	;; BUT53 
(def-exported-constant "LEFTBRACKETKEY"	49)	;; BUT48 
(def-exported-constant "RIGHTBRACKETKEY"	56)	;; BUT55 
(def-exported-constant "LEFTARROWKEY"	73)	;; BUT72 
(def-exported-constant "DOWNARROWKEY"	74)	;; BUT73 
(def-exported-constant "RIGHTARROWKEY"	80)	;; BUT79 
(def-exported-constant "UPARROWKEY"	81)	;; BUT80 
(def-exported-constant "PAD0"		59)	;; BUT58 
(def-exported-constant "PAD1"		58)	;; BUT57 
(def-exported-constant "PAD2"		64)	;; BUT63 
(def-exported-constant "PAD3"		65)	;; BUT64 
(def-exported-constant "PAD4"		63)	;; BUT62 
(def-exported-constant "PAD5"		69)	;; BUT68 
(def-exported-constant "PAD6"		70)	;; BUT69 
(def-exported-constant "PAD7"		67)	;; BUT66 
(def-exported-constant "PAD8"		68)	;; BUT67 
(def-exported-constant "PAD9"		75)	;; BUT74 
(def-exported-constant "PADPF1"	72)	;; BUT71 
(def-exported-constant "PADPF2"	71)	;; BUT70 
(def-exported-constant "PADPF3"	79)	;; BUT78 
(def-exported-constant "PADPF4"	78)	;; BUT77 
(def-exported-constant "PADPERIOD"	66)	;; BUT65 
(def-exported-constant "PADMINUS"	76)	;; BUT75 
(def-exported-constant "PADCOMMA"	77)	;; BUT76 
(def-exported-constant "PADENTER"	82)	;; BUT81 

;;; the extended keyboard 

(def-exported-constant "LEFTALTKEY" 		143)
(def-exported-constant "RIGHTALTKEY" 		144)
(def-exported-constant "RIGHTCTRLKEY" 	145)
(def-exported-constant "F1KEY" 		146)
(def-exported-constant "F2KEY" 		147)
(def-exported-constant "F3KEY" 		148)
(def-exported-constant "F4KEY" 		149)
(def-exported-constant "F5KEY" 		150)
(def-exported-constant "F6KEY" 		151)
(def-exported-constant "F7KEY" 		152)
(def-exported-constant "F8KEY" 		153)
(def-exported-constant "F9KEY" 		154)
(def-exported-constant "F10KEY"		155)
(def-exported-constant "F11KEY"		156)
(def-exported-constant "F12KEY"		157)
(def-exported-constant "PRINTSCREENKEY"	158)
(def-exported-constant "SCROLLLOCKKEY"	159)
(def-exported-constant "PAUSEKEY"		160)
(def-exported-constant "INSERTKEY"		161)
(def-exported-constant "HOMEKEY"		162)
(def-exported-constant "PAGEUPKEY" 		163)
(def-exported-constant "ENDKEY"		164)
(def-exported-constant "PAGEDOWNKEY"		165)
(def-exported-constant "NUMLOCKKEY"		166)
(def-exported-constant "PADVIRGULEKEY" 	167)
(def-exported-constant "PADASTERKEY" 		168)
(def-exported-constant "PADPLUSKEY" 		169)

;;; 
;;; By rights, we should define symbolic entries here for all of the new
;;; characters brought to us by ISO 8859-1.  In fact, since there is no
;;; current convention to avoid making new symbols that are unique, the
;;; danger of collison with existing user symbols is too high. 
 

;;; valuators 

(def-exported-constant "SGIRESERVED"	256)	;; 0+VALOFFSET 
(def-exported-constant "DIAL0"	257)	;; 1+VALOFFSET 
(def-exported-constant "DIAL1"	258)	;; 2+VALOFFSET 
(def-exported-constant "DIAL2"	259)	;; 3+VALOFFSET 
(def-exported-constant "DIAL3"	260)	;; 4+VALOFFSET 
(def-exported-constant "DIAL4"	261)	;; 5+VALOFFSET 
(def-exported-constant "DIAL5"	262)	;; 6+VALOFFSET 
(def-exported-constant "DIAL6"	263)	;; 7+VALOFFSET 
(def-exported-constant "DIAL7"	264)	;; 8+VALOFFSET 
(def-exported-constant "DIAL8"	265)	;; 9+VALOFFSET 
(def-exported-constant "MOUSEX"	266)	;; 10+VALOFFSET 
(def-exported-constant "MOUSEY"	267)	;; 11+VALOFFSET 
(def-exported-constant "LPENX"	268)	;; 12+VALOFFSET 
(def-exported-constant "LPENY"	269)	;; 13+VALOFFSET 
(def-exported-constant "BPADX"	270)	;; 14+VALOFFSET 
(def-exported-constant "BPADY"	271)	;; 15+VALOFFSET 
(def-exported-constant "CURSORX"	272)	;; 16+VALOFFSET 
(def-exported-constant "CURSORY"	273)	;; 17+VALOFFSET 
(def-exported-constant "GHOSTX"	274)	;; 18+VALOFFSET 
(def-exported-constant "GHOSTY"	275)	;; 19+VALOFFSET 

;; Space Ball valuators 

(def-exported-constant "SBTX"		276)	;; 20+VALOFFSET 
(def-exported-constant "SBTY"		277)	;; 21+VALOFFSET 
(def-exported-constant "SBTZ"		278)	;; 22+VALOFFSET 
(def-exported-constant "SBRX"	 	279)	;; 23+VALOFFSET 
(def-exported-constant "SBRY"		280)	;; 24+VALOFFSET 
(def-exported-constant "SBRZ"		281)	;; 25+VALOFFSET 
(def-exported-constant "SBPERIOD"	282)	;; 26+VALOFFSET 

;; timers 

(def-exported-constant "TIMER0"	515)	;; 0+TIMOFFSET 
(def-exported-constant "TIMER1"	516)	;; 1+TIMOFFSET 
(def-exported-constant "TIMER2"	517)	;; 2+TIMOFFSET 
(def-exported-constant "TIMER3"	518)	;; 3+TIMOFFSET 

;; misc devices 

(def-exported-constant "KEYBD"	513)	;; keyboard 
(def-exported-constant "RAWKEYBD"	514)	;; raw keyboard for keyboard manager 
(def-exported-constant "VALMARK"	523)	;; valuator mark 
(def-exported-constant "REDRAW"	528)	;; used by port manager to signal redraws 
(def-exported-constant "INPUTCHANGE"	534)	;; input connected or disconnected 
(def-exported-constant "QFULL"	535)	;; queue was filled 
(def-exported-constant "QREADERROR"	538)	;; qread error 
(def-exported-constant "WINFREEZE"	539)	;; user wants process in this win to shut up 
(def-exported-constant "WINTHAW"	540)	;; user wants process in this win to go again 
(def-exported-constant "REDRAWICONIC"	541)	;; used to signal redraw as an icon 
(def-exported-constant "WINQUIT"	542)	;; signal from user that app is to go away 
(def-exported-constant "DEPTHCHANGE"	543)	;; window stacking order changed 
(def-exported-constant "WINSHUT"	546)	;; window shutdown 
(def-exported-constant "DRAWOVERLAY"	547)	;; overlay planes have been damaged 

(def-exported-constant "MENUBUTTON"	RIGHTMOUSE) ;; the button used by dopup 


;;;
;;; obsolete symbols 
;;; 

;; (def-exported-constant	DEVICEDEF	__GL_DEVICE_H__

(def-exported-constant "WINCLOSE"		537)	;; window close 
(def-exported-constant "KEYBDFNAMES"		544)	;; function key names 
(def-exported-constant "KEYBDFSTRINGS"	545)	;; function key strings 
(def-exported-constant "MAXSGIDEVICE"		20000)

;; these events only occur when using the mex window manager (3K Series) 
(def-exported-constant "GERROR"	524)	;; errors device 
(def-exported-constant "WMSEND"	529)	;; data in proc's shmem 
(def-exported-constant "WMREPLY"	530)	;; reply from port manager 
(def-exported-constant "WMGFCLOSE"	531)	;; graphport is no longer being used 
(def-exported-constant "WMTXCLOSE"	532)	;; textport is no longer being used 
(def-exported-constant "MODECHANGE"	533)	;; the display mode has changed 
(def-exported-constant "PIECECHANGE"	536)	;; change in the window pieces 



