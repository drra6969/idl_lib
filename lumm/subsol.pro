pro subsol, year, doy, sbsllat, sbsllon, time=time, utsec=utsec

; Find subsolar geographic latitude and longitude from date and time.
; Based on formulas in Astronomical Almanac for the year 1996, p. C24.
; (U.S. Government Printing Office, 1994).
; Usable for years 1601-2100, inclusive.  According to the Almanac, 
; results are good to at least 0.01 degree latitude and 0.025 degree 
; longitude between years 1950 and 2050.  Accuracy for other years 
; has not been tested.  Every day is assumed to have exactly
; 86400 seconds; thus leap seconds that sometimes occur on December
; 31 are ignored:  their effect is below the accuracy threshold of
; the algorithm.
;
; After Fortran code by: 961026 A. D. Richmond, NCAR
;
; Input:
;       year: calender year (e.g., 1994).  1600 < IYR < 2101
;       doy:  day of the year (1 = January 1; 365 [366 in leap year] = 
;	      December 31).  
;       Specify UT time either in the form of time=[hour, minute, second]
;       or as UTsec=seconds, with seconds counting from 0:00:00 UT on
;       the specified day.
;
; Output:
;       sbsllat, sbsllon: geographic latitude and longitude of the subsolar
;                         point in degrees (lon from -180 to +180)

; check parameters
if not (n_elements(utsec) gt 0 or n_elements(time) eq 3) then begin
   print, 'Need to specify UT either as time=[hour, minute, second]'+ $
        ' or as UTsec=seconds'
   return
endif

; Number of years from 2000 to IYR (negative if IYR < 2000):
YR = year - 2000

; NLEAP (final) = number of leap days from (2000 January 1) to (IYR January 1)
;                 (negative if IYR is before 1997)
NLEAP = (year-1601)/4
NLEAP = NLEAP - 99
if year le 1900 then begin
   if year le 1600 THEN begin
      print, 'SUBSOLR INVALID BEFORE 1601: INPUT YEAR = ', year      
      return
   ENDIF
   NCENT = (year-1601)/100
   NCENT = 3 - NCENT 
   NLEAP = NLEAP + NCENT
ENDIF
IF (year ge 2101) THEN begin
   print, 'SUBSOLR INVALID AFTER 2100:  INPUT YEAR = ', year
   return
ENDIF

; L0 = Mean longitude of Sun at 12 UT on January 1 of IYR:
;     L0 = 280.461 + .9856474*(365*(YR-NLEAP) + 366*NLEAP) 
;	   - (ARBITRARY INTEGER)*360.
;        = 280.461 + .9856474*(365*(YR-4*NLEAP) + (366+365*3)*NLEAP) 
;	   - (ARBITRARY INTEGER)*360.
;        = (280.461 - 360.) + (.9856474*365 - 360.)*(YR-4*NLEAP) 
;	   + (.9856474*(366+365*3) - 4*360.)*NLEAP,
;  where ARBITRARY INTEGER = YR+1.  This gives:
L0 = -79.549 + (-.238699*(YR-4*NLEAP) + 3.08514E-2*NLEAP)

; G0 = Mean anomaly at 12 UT on January 1 of IYR:
;     G0 = 357.528 + .9856003*(365*(YR-NLEAP) + 366*NLEAP) 
;	   - (ARBITRARY INTEGER)*360.
;        = 357.528 + .9856003*(365*(YR-4*NLEAP) + (366+365*3)*NLEAP) 
;	   - (ARBITRARY INTEGER)*360.
;        = (357.528 - 360.) + (.9856003*365 - 360.)*(YR-4*NLEAP) 
;	   + (.9856003*(366+365*3) - 4*360.)*NLEAP,
;  where ARBITRARY INTEGER = YR+1.  This gives:
G0 = -2.472 + (-.2558905*(YR-4*NLEAP) - 3.79617E-2*NLEAP)

; Universal time in seconds:
if n_elements(utsec) gt 0 then ut = utsec else $
     UT = FLOAT(time(0)*3600 + time(1)*60) + time(2)

; Days (including fraction) since 12 UT on January 1 of IYR:
DF = (UT/86400. - 1.5) + doy

; Addition to Mean longitude of Sun since January 1 of IYR:
LF = .9856474*DF

; Addition to Mean anomaly since January 1 of IYR:
GF = .9856003*DF

; Mean longitude of Sun:
L = L0 + LF

; Mean anomaly:
G = G0 + GF
GRAD = G*!dtor

; Ecliptic longitude:
LAMBDA = L + 1.915*SIN(GRAD) + .020*SIN(2.*GRAD)
LAMRAD = LAMBDA*!dtor
SINLAM = SIN(LAMRAD)

; Days (including fraction) since 12 UT on January 1 of 2000:
N = DF + FLOAT(365*YR + NLEAP)

; Obliquity of ecliptic:
EPSILON = 23.439 - 4.E-7*N
EPSRAD = EPSILON*!dtor

; Right ascension:
ALPHA = ATAN(COS(EPSRAD)*SINLAM,COS(LAMRAD))*!radeg

; Declination:
DELTA = ASIN(SIN(EPSRAD)*SINLAM)*!radeg

; Subsolar latitude:
SBSLLAT = DELTA

; Equation of time (degrees):
ETDEG = L - ALPHA
NROT = round(ETDEG/360.)
ETDEG = ETDEG - FLOAT(360*NROT)

; Apparent time (degrees):
APTIME = UT/240. + ETDEG
;          Earth rotates one degree every 240 s.

; Subsolar longitude:
SBSLLON = 180. - APTIME
NROT = round(SBSLLON/360.)
SBSLLON = SBSLLON - FLOAT(360*NROT)

RETURN
END
