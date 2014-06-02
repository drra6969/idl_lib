pro msis, f107=f107, ap=ap, glat=glat, glon=glon, hour=hour, $
          yyddd=yyddd, zmin=zmin, zmax=zmax, mlyr=mlyr, $
          altitude=z, rho=rho, N2den=N2den, O2den=O2den, Oden=Oden, $
          Nden=Nden, Heden=Heden, Hden=Hden, Arden=Arden, temperature=T,  $
          plot=plot
;+
; NAME:
;         msis
;
; PURPOSE:
;         get an MSIS atmosphere
;
; CALLING SEQUENCE:
;         msis, f107=f107, ap=ap, glat=glat, glon=glon, hour=hour, $
;               yyddd=yyddd, zmin=zmin, zmax=zmax, mlyr=mlyr, $
;               altitude=z, rho=rho, N2den=N2den, O2den=O2den, Oden=Oden, $
;               Nden=N, Heden=Heden, Hden=Hden, Arden=Arden, temperature=T, $
;               /plot
; 
; INPUTS:
;         if no input keywords are given, the program assumes
;         reasonable defaults. If altitude is not given, it will be
;         generated with mlyr [141] log-spaced layers. Altitude is
;         expected in km. 
;
; OPTIONAL INPUTS:
;         all inputs are given through keywords and are self
;         explantatory (see fortran version of MSIS ofr more details) 
;
; KEYWORD PARAMETERS:
;         if keyword /plot is set, a plot of densities (N2, O2, O) and
;         one with the neutral temperature is made.
;
; OUTPUTS:
;         Output is returned on the specified keywords: 
;         altitude in km (if not given as input)
;         rho in g/cm^3
;         N2den, O2den, and Oden in cm^-3
;         temperature in Kelvin.
;
; EXAMPLE:
;         msis, /plot
;         generates plots of densities and temperature from 80 to 500 km 
;         altitude
;
; MODIFICATION HISTORY:
;
;       Thu Oct 5 17:05:37 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;-

if n_elements(zmax) eq 0 then zmax=500.
if n_elements(zmin) eq 0 then zmin=80.
if n_elements(Hden) eq 0 then plot_H = 1 else plot_H = 0
if n_elements(Heden) eq 0 then plot_He = 1 else plot_He = 0

; altitude grid, z in km
if zmin le 0 then zmin = 1.     ;zero or negative altitudes won't work
if n_elements(z) eq 0 then begin
  if n_elements(mlyr) eq 0 then mlyr=141
  za=alog(zmax/float(zmin))/(mlyr-1.)
  zb=alog(float(zmin)) - za
  z=exp(za*(indgen(mlyr)+1)+zb)
endif
mlyr=n_elements(z)

if n_elements(f107) eq 0 then f107=150.
f107av=f107
if n_elements(ap) eq 0 then ap=110.
if n_elements(glat) eq 0 then glat=64.9
if n_elements(glon) eq 0 then glon=212.
if n_elements(hour) eq 0 then hour=24
if n_elements(yyddd) eq 0 then yyddd=90003

spawn, '~lumm/idl/msis', unit=unit
printf, unit, mlyr
printf, unit, z*1e5   ; z in cm
printf, unit, yyddd, hour, glat, glon, f107av, f107, ap

rho=fltarr(mlyr)
N2den=fltarr(mlyr)
O2den=fltarr(mlyr)
Oden=fltarr(mlyr)
Nden=fltarr(mlyr)
Heden=fltarr(mlyr)
Hden=fltarr(mlyr)
Arden=fltarr(mlyr)
T=fltarr(mlyr)
readf, unit, rho, N2den, O2den, Oden, Nden, Heden, Hden, Arden, T
free_lun, unit

if keyword_set(plot) then begin
  p_save=!p.multi
  !p.multi=[0,2,1]
  title='MSIS-90, lat/lon=(' + $
           strcompress(string(glat, form="(f6.2)"), /rem)  + $
           ', ' + strcompress(string(glon, form="(f6.2)"), /rem) + $
           '), F!i10.7!n=' + $
           strcompress(string(f107, form="(f6.2)"), /rem) + $
           ', day=' + strcompress(yyddd, /rem) + $
           ', hour=' + strcompress(hour, /rem)
  plot, N2den, z, xtitle='number density (cm!e-3!n)', /xlog, $
           ytitle='altitude (km)', ymar=[4,4], ysty=16
  oplot, O2den, z, line=3
  oplot, Oden, z, line=2
  if plot_H eq 1 then oplot, Hden, z, line=4
  if plot_He eq 1 then oplot, Heden, z, line=4
  plot, T, z, xtitle='temperature (K)', ytitle='altitude (km)', $
        ymar=[4,4], ysty=16
  xyouts, .5, .96, title, align=0.5, /norm
  !p.multi=p_save
endif

return
end
