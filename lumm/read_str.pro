function read_str, a, separator=separator, number=number
;+
; NAME:
;         read_str
;
; PURPOSE:
;         read numbers from a string
;
; INPUTS:
;      read_str, a, separator=separator, number=number   
;      with a:         string
;           separator: a string containing the separator that
;                      separates the numbers in the string (default: ' ')
;           number:    the number of floating point numbers in the
;                      string, if htere are more than 100
;
; OUTPUTS:
;      array of floating point numbers
;
; EXAMPLE:
;       a="1.0 2.0 3.0"
;       print, read_str(a)
;       1.0     2.0     3.0
;
; MODIFICATION HISTORY:
;
;       Wed Aug 16 16:48:42 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;-

if not keyword_set(separator) then separator=' '
if not keyword_set(number) then number=100

bb=intarr(number)
bb(0)=0
i=0

repeat begin
i=i+1
bb(i)=strpos(a, separator, bb(i-1)+1) ; position of separators
endrep until bb(i) lt 0
nblank=i

ff=fltarr(nblank)
for i=0,nblank-1 do ff(i)=float(strmid(a, bb(i), strlen(a)-bb(i)))

return, ff
end

    
