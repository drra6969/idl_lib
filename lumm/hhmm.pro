function hhmm, hour, second=second, nocolon=nocolon
;+
; NAME:
;          hhmm
;
; PURPOSE:
;          return a string hh:mm for input time in decimal hours
;
; use as:
;          print, hhmm(12.25)
; returns:
;          12:15
;
; accepts parameters (see dec_hour for the reverse function)
;          /second to include the seconds in the output string
;                  for example: 12:25:09
;          /nocolon to avaoid the colons in the output string
;                  for example: 122509 or 0215
; input string may be single floating point or array
;
;       Thu Aug 17 11:55:30 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;-

n = n_elements(hour)

hh = fix(hour)
if keyword_set(second) then mm = fix(hour*60 mod 60)  $
                       else mm = round(hour*60 mod 60)
ss = round(hour*3600 mod 60)

if keyword_set(second) then format = '(i2,":",i2.2,":",i2.2)'  $
                       else format = '(i2,":",i2.2)'
if keyword_set(nocolon) then  $
     if keyword_set(second) then format = '(i2.2,i2.2,i2.2)'  $
                            else format = '(i2.2,i2.2)'

if keyword_set(second) then begin
   if n gt 1 then $
        return, string(reform([transpose(hh), transpose(mm), transpose(ss)],  $
                              3*n), format=format) $
   else return, string(hh,mm,ss,format=format)
endif else begin
   if n gt 1 then $
        return, string(reform([transpose(hh), transpose(mm)], 2*n), $
                       format=format) $
   else return, string(hh,mm,format=format)
endelse

return, ''
end
