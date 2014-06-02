function erfc, x

; (1-erf(x)) * exp(x^2)
; see NBS Handbook

a1=.254829592
a2=-.284496736
a3=1.421413741
a4=-1.453152027
a5=1.061405429
p=.3275911
t=1./(1.+p*x)
return, ((((a5*t+a4)*t+a3)*t+a2)*t+a1)*t
end

function erf, x

; error function for x > 0 

e=1.-erfc(x)*exp(-x*x)
zero=where(x le 0, i)
if i gt 0 then e(zero)=0.
return, e
end

function chapman, z, sza, scaleheight, shadow=shadow, zmin=zmin

; determine the Chapman function.  See Brasseur and Solomon page 106
; and mostly Swider and Gardner, 1967.
; function erfc and erf come from NBS Handbook. Note: erf(x) agrees with
; IDL's errorf(x) for x>0.
; input: 
;      z: altitude above ground (in km)
;      sza: solar zenith angle (degrees)
;      scaleheight: optional, if omitted use 8 km
;      zmin: altitude that defines total extinction

; NOTE: this routine can only deal with one altitude at a time, but
; accepts an array of solar zenith angles

; NOTE the file chapman1.pro does the same thing, but uses a different
; method for sza > 90....  The results are slightly different.  I am
; not quite satisfied with either way....

re=6371.   ;earth radius
if n_elements(scaleheight) eq 0 then scaleheight=8.
if n_elements(z) gt 1 then begin
   print, 'function chapman>> altitude must NOT be array!'
   return, 0
endif

if max(sza) gt 180 or min(sza) lt 0 then begin
    print, 'solar zenith angle must be between 0 and 180'
    return, 0
endif

x=(re+z)/scaleheight
ss=sza*!pi/180.
w=x*(1.-sin(ss))
sw=sqrt(w)
; altitude that defines the shadow (0 for surface shadow)
if n_elements(zmin) eq 0 then zmin=0.  

i90=where(sza le 90, n90)
i180=where(90 lt sza and sza lt 180, n180)

is=size(sza)
if is(0) gt 0 then begin
    chap=fltarr(is(1))
    shadow=fltarr(is(1))
endif else begin
    chap=fltarr(1)
    shadow=fltarr(1)
endelse

if n90 gt 0 then begin
    chap(i90)=-x*cos(ss(i90)) + sqrt(1.+2.*x-w(i90)) * $
              (sw(i90)+.5*sqrt(!pi)*erfc(sw(i90)))
    shadow(i90)=0.
endif

if n180 gt 0 then begin
    shadow(i180)=(re+zmin)/sin(ss(i180))-re
    v=(z+re)*sin(ss(i180))-re
    for i=0,n180-1 do chap(i180(i))= $
         2.*chapman(v(i), 90, scaleheight)- $
         chapman(v(i), 180-sza(i180(i)), scaleheight)
    i0=where(v le zmin, ii0)
    if ii0 gt 0 then chap(i180(i0))=0.
endif
;print, chap

return, chap
end
