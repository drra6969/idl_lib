;+
; NAME:
;        energy_flux
;        uses function: get_energy
;
; PURPOSE:
;        take auroral brightness and interpolate characteristic energy
;        and energy fluxes
;
; CALLING SEQUENCE:
;        energy_flux, energy, flux, [i3914=i3914,] [i4278=i4278,] $
;                                   [i4709=i4709,] $
;                                   [i8446=i8446,] [i6300=i6300,] $
;                                   [/silent]
; 
; INPUTS:
;        one of the three blue emissions: 3914, 4709, or 4278 and one of the
;        two red emissions: 6300 or 8446. Any combination is allowed
;        as long as there is one red and one blue.  Brightness must be
;        in units of Rayleigh.  The blue and red brightness can be
;        given as arrays or as individual numbers.  The returned
;        output will have the same array dimension as the input.  If
;        the keyword /silent is specified, any error messages will be
;        surpressed. 
;
; OUTPUTS:
;        characteristic energy (in keV) and energy flux (in mW/m2)
;
; EXAMPLE:
;        IDL>energy_flux, e, f, i3914=2000, i6300=100
;        IDL>print, e, f
;             10.0785
;             2.43902
;
; MODIFICATION HISTORY:
;
;       Tue Aug 8 13:46:38 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;-

; function get_energy is needed by the program energy_flux

function get_energy, ratio, line=line

; interpolate a given red to blue (4278) ratio to characteristic
; energy. 
; parameter line allows to use different brightness ratios.
; allowed values are: line=6300(default), line=8446
;
; this method of interpolation yields smooth results, and doesn't
; blow up for values outside the range of data (except for extreme
; values).  

; Curves 
E0= [0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 2.0, 3.0, 5.0, 7.0]
if not keyword_set(line) or line eq 6300 then $
  r= [13., 8.2, 5.3, 3.0, 2.0, 1.2, 0.43, 0.22, 0.11, 0.075] $  ; 6300/blue
else $
  r= [4.2, 2.9, 2.1, 1.3, 1.0, 0.71, 0.38, 0.25, 0.15, 0.09]    ; 8446/blue

; do logarithmic interpolations
e=alog(e0)
c=alog(r) 

; make a dense grid by smoothing the input values
spline_p, e, c, ee, cc

; use the smooth values for linear interpolation of the log values
e=exp(interpol(ee, cc, alog(ratio)))

; set "bad" values to zero..... (this might need some more work)
bad=where(e lt min(E0) or e gt 2*max(E0), ibad)
if ibad gt 0 then e(bad)=0.

return, e
end

; now comes the calling (main) program

pro energy_flux, energy, flux, i3914=i3914, i4278=i4278, i8446=i8446, $
                 i4709=i4709, i6300=i6300, silent=silent

; check whether we have the right combination of input brightnesses

if not (keyword_set(i4278) or keyword_set(i3914) or $
        keyword_set(i4709)) then begin
    if not keyword_set(silent) then print, '>>> energy_flux:  Need at ' + $
      'least one of the 4278 or 3914 emissions'  
    energy=0.
    flux=0.
    return
endif
if not (keyword_set(i6300) or keyword_set(i8446)) then begin
    if not keyword_set(silent) then print, '>>> energy_flux:  Need at ' + $
      'least one of the 6300 or 8446 emissions' 
    energy=0.
    flux=0.
    return
endif

; find out which blue emissions we have and set flux calibration
; hte get_energy routine needs blue as 4278, so apply the Frank-Condon
; factors as needed to calculate the "equivalent" 4278 brightness and
; keep this in array "blue"

R_per_erg=250.
threshold=150.
if keyword_set(i4278) then blue=i4278 else $
   if keyword_set(i4709) then blue=i4709*5. else $
   if keyword_set(i3914) then blue=i3914*0.3

; get_energy uses the ratio of either i6300/i4278 or i8446/i6300.  The
; two possible ratios have a different non-linear dependence on
; E_char, so the keyword "line" is needed to tell get_energy which
; ratio to use.

if keyword_set(i8446) then begin
    red=i8446
    line=8446
endif
if keyword_set(i6300) then begin
    red=i6300
    line=6300
endif

; make sure that we have compatible data arrays

if n_elements(red) ne n_elements(blue) then begin
    if not keyword_set(silent) then print, '>>> energy_flux:  The ' + $
      'number of red and blue data points have to be equal' 
    if not keyword_set(silent) then print, '>>> energy_flux:  here we ' + $
      'have', strcompress(n_elements(blue)), ' blue and', $
      strcompress(n_elements(red)), ' red points' 
    energy=0.
    flux=0.
    return
endif

; use only data above the specified threshold for the blue brightness

good=where(blue gt threshold and red gt 0, i)
if i le 0 then begin 
    if not keyword_set(silent) then print, 'no useful data above ' + $
      '(blue) threshold of ', threshold, ' R' 
    flux=blue*0.
    energy=flux
    return
endif

; turn red/blue into energy

energy=fltarr(n_elements(blue))
energy(good)=get_energy(red(good)/float(blue(good)), line=line)

; flux is proportional to blue

R_to_flux=1./R_per_erg
flux=fltarr(n_elements(blue))
flux(good)=blue(good)*R_to_flux

; return single value instead of array with one element, if there was
; only one value

if n_elements(blue) eq 1 then begin
    flux=flux(0)
    energy=energy(0)
endif

return
end
