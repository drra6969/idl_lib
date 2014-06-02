function column, rho, z

; calculate the column density

ntop=n_elements(z)

col=fltarr(ntop)
ntop=ntop-1
col(ntop)=rho(ntop)*(z(ntop)-z(ntop-1))/alog(rho(ntop-1)/rho(ntop))

for i=ntop-1, 0, -1 do col(i)=col(i+1) + $
  (rho(i+1)-rho(i))*(z(i+1)-z(i))/alog(rho(i+1)/rho(i)) 

return, col
end



pro param_edep, e_0, f_e, edep=edep, altitude=z, plot=plot, ion=ion, $
                proton=proton, zmin=zmin, zmax=zmax, _extra=_extra

; plot the parameterized energy deposition using the parameterizations
; from the TIEGCM. See the UARS report UAG-R-318, April 92 for details.
; e_0: characteristic energy (keV)
; f_0: energy flux (erg/cm2/s) (default: f_0=1)
; plot: make plot if set
; ion: ionization rate instead of energy deposition rate
; proton: assume proton precipitation (default: electron)
; zmin, zmax: altitude limits (km)
; altitude: altitude output (can also be input - if an array is
;           provided, this array of altitudes is used, and zmin/zmax
;           are neglected. See msis.pro for more info.
; edep: energy deposition on output

if not keyword_set(f_e) then f_e=1.
if not keyword_set(e_0) then begin
 print, 'use as:'
 print, 'param_edep, e_0 [, f_e], [edep=edep], [altitude=z], [/plot], $'
 print, '            [/ion], [/proton], [zmin=zmin], [zmax=zmax], $'
 print, '            [_extra=_extra]'
 return
endif

; get an msis atmosphere

msis, altitude=z, rho=rho, zmin=zmin, zmax=zmax

; set range (= column density) (z is in km....)

r=column(rho, z*1e5)

; y in eq (25)

if keyword_set(proton) then begin
    a=0.00271d0
    b=1.72
    y=(r/a)^(1./b)/(e_0/1000.)  ; e_0 in keV
endif else begin
    a=4.6d-6
    b=1.65
    y=((r/a)^(1./b))/e_0        ; e_0 in keV
endelse

; j in eq 26
format='(f10.2)'
if keyword_set(proton) then begin
    c1=0.12718*2
    c2=4.9119
    c3=-1.8429
    c4=0.99336
    c5=0.52472*2
    c6=1.5565
    c7=-0.85732
    c8=1.4116
    title='Proton Precipitation'
endif else begin
    c1=3.233
    c2=2.56588
    c3=-2.2541
    c4=0.7297198
    c5=1.106907
    c6=1.71349
    c7=-1.8835444
    c8=0.86472135
    title='Electron Precipitation'
endelse
title=title + ' - E!i0!n='+ $
      strcompress(string(e_0, form=format), /rem)+' keV, F!iE!n='+ $
      strcompress(string(f_e, form=format), /rem)+' erg cm!e-2!ns!e-1!n'
j=c1*(y^c2)*exp(c3*(y^c4)) + c5*(y^c6)*exp(c7*(y^c8))

; edep in eq 22

edep=rho*f_e/(2*r)*j
if keyword_set(ion) then begin
    edep=edep/1.6022e-12/35
    xtitle='Ionization Rate (cm!e-3!ns!e-1!n)'
endif else xtitle='Energy Deposition Rate (erg cm!e-3!ns!e-1!n)'

if keyword_set(plot) then $
    plot_oi, edep, z, title=title, $
             xtitle=xtitle, $
             ytitle='Altitude (km)', ysty=16, _extra=_extra

return
end

