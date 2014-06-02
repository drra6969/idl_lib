function month_name, m, short=short
;+
; NAME:
;       month_name
;
; PURPOSE:
;       return the name for a given month
;
; use as:
;       print, month_name(n [,/short])
; with:
;       n: an integer between 1 and 12
;       if keyword "short" is set, return 3 letter abbreviation
; example:
;       print, month_name(4)
;       gives the output: April
;
; HISTORY:
;
;       Thu Oct 5 17:09:28 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;-

month=['January', 'February', 'March', 'April', 'May', $
       'June', 'July', 'August', 'September', 'October', $
       'November', 'December']
smonth=['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', $
        'AUG', 'SEP', 'OCT', 'NOV', 'DEC']

if keyword_set(short) then return, smonth(m-1) else return, month(m-1)
end
