function dec_hour, time
;+
; NAME:
;        dec_hour
;
; PURPOSE:
;        return the decimal hour for an input string 'hh:mm' or
;        'hh:mm:ss.sss' 
;
; use as:
;          print, dec_hour('12:15')
; returns:
;          12.2500
;
; accepts no parameters (see hhmm for the reverse function)
; input string may be single string or array of strings
;
;       Thu Aug 17 11:55:30 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;-

n=n_elements(time)

if n eq 1 then begin
    cc=strpos(time, ':')                ; find first ':'
    hh=float(strmid(time, 0, cc))
    mm=float(strmid(time, cc+1, 2))
    cc=strpos(time, ':', cc+1)          ; find second ':'
    if cc gt 0 then ss=float(strmid(time, cc+1, strlen(time)-cc)) $
               else ss=0.
endif else begin 
    hh=fltarr(n)
    mm=hh
    ss=hh
    for i=0,n-1 do begin
        cc=strpos(time(i), ':')                ; find first ':'
        hh(i)=float(strmid(time(i), 0, cc))
        mm(i)=float(strmid(time(i), cc+1, 2))
        cc=strpos(time(i), ':', cc+1)          ; find second ':'
        if cc gt 0 then ss(i)=float( $
               strmid(time(i), cc+1, strlen(time(i))-cc))
    endfor
endelse

return, hh+mm/60.+ss/3600.
end
