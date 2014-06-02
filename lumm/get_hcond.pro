function get_Hcond, energy, flux, silent=silent

;+
; NAME:
;        get_Hcond
;
; PURPOSE:
;        interpolate a given energy and energy flux to obtain the
;        Hall conductance 
;
;        this method of interpolation yields smooth results, and
;        doesn't blow up for values outside the range of data (except
;        for extreme values).  
;
; CALLING SEQUENCE:
;        hcond=get_hcond(energy, flux, /silent)
; 
; INPUTS:
;        energy: characteristic energy in keV
;        flux:   enegry flux in mW/m2
;
; KEYWORDS:
;        silent: supress warning messages
;
; OUTPUTS:
;        hcond:  Hall conductance (same dimentsion as the input
;                arrays energy and flux)
;
; MODIFICATION HISTORY:
;
;       Thu Oct 5 16:09:17 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;
;-

; NOTE: the input array must not have a maximum to prevent double 
; valued solutions.

; Curves from ionchem runs
E0= [0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.37, 2.0, 3.0, 4.0, $
     5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
;SigmaH= [0.5, 0.9, 1.1, 1.5, 1.8, 2.0, 4.9, 6.8, 8.1, $
;         8.9, 9.5, 9.8, 10.1, 10.2, 10.3]
SigmaH=[0.388, 0.598, 0.824, 1.11, 1.37, 1.66, 2.75, 4.20, 5.88, 7.11, $
        7.87, 8.40, 8.79, 9.06, 9.19, 9.29]

; check the input
if not keyword_set(silent) then begin
    if min(energy) le 0 then print, 'get_Hcond>> zero or negative energy ' + $
      'encountered'  
    if min(flux) le 0 then print, 'get_Hcond>> zero or negative energy ' + $
      'flux encountered'  
    if max(energy) gt 2*max(E0) then print, 'get_Hcond>> energy should be ' + $
      'less than ',2*max(E0),' keV - some unreliable results may exist'
endif

; do logarithmic interpolations
e=alog(e0)
c=alog(SigmaH)

; make a dense grid by smoothing the input values
spline_p, e, c, ee, cc

; use this to allow extrapolation to lower energies....
E0_min=0.1
; E0_min=min(E0)

; use the smooth values for linear interpolation of the log values
c=exp(interpol(cc, ee, alog(energy>E0_min)))

cond=c*(flux>0)^0.55

; find and remove "bad" values.... (whatever that might be)
bad=where(energy lt E0_min, ibad)
if ibad gt 0 then cond(bad)=0.

return, cond
end
