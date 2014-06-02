function get_Pcond, energy, flux, silent=silent
;+
; NAME:
;        get_Pcond
;
; PURPOSE:
;        interpolate a given energy and energy flux to obtain the
;        Pedersen conductance 
;
;        this method of interpolation yields smooth results, and
;        doesn't blow up for values outside the range of data (except
;        for extreme values).  
;
; CALLING SEQUENCE:
;        pcond=get_Pcond(energy, flux, /silent)
; 
; INPUTS:
;        energy: characteristic energy in keV
;        flux:   enegry flux in mW/m2
;
; KEYWORDS:
;        silent: supress warning messages
;
; OUTPUTS:
;        pcond:  Pedesen conductance (same dimentsion as the input
;                arrays energy and flux)
;
; MODIFICATION HISTORY:
;
;       Thu Oct 5 16:09:17 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;
;-

; Curves from ionchem runs
E0= [0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.37, 2.0, 3.0, 4.0, $
     5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
;SigmaP= [1.3, 1.5, 1.8, 2.1, 2.3, 2.5, 3.4, 3.3, 3.2, $
;     3.1, 3.0, 2.8, 2.6, 2.5, 2.4]
SigmaP=[1.18, 1.47, 1.73, 1.99, 2.20, 2.41, 2.94, 3.30, 3.36, 3.20, $
        3.02, 2.84, 2.68, 2.54, 2.41, 2.29]

; check the input
if not keyword_set(silent) then begin
    if min(energy) le 0 then print, 'get_Pcond>> zero or negative energy ' + $
      'encountered'  
    if min(flux) le 0 then print, 'get_Pcond>> zero or negative energy ' + $
      'flux encountered'  
    if max(energy) gt 2*max(E0) then print, 'get_Pcond>> energy should be ' + $
      'less than ',2*max(E0),' keV - some unreliable results may exist'
endif

; do logarithmic interpolations
e=alog(e0)
c=alog(SigmaP)

; make a dense grid by smoothing the input values
spline_p, e, c, ee, cc

; use this to allow extrapolation to lower energies....
E0_min=0.1
; E0_min=min(E0)

; use the smooth values for linear interpolation of the log values
c=exp(interpol(cc, ee, alog(energy>E0_min)))

cond=c*(flux>0)^0.55

; find and remove "bad" values (whatever that might mean)
bad=where(energy lt E0_min, ibad)
if ibad gt 0 then cond(bad)=0.

return, cond
end
