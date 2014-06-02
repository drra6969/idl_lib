function column_den, den, zz

; calculate column density (vertical column)

kk = n_elements(den)
col_den = fltarr(kk)
z = zz*1e5                      ;convert from km to cm

; top level
col_den(kk-1) = den(kk-1)/alog(den(kk-2)/den(kk-1))*(z(kk-1)-z(kk-2))

; rest of altitude grid
dz = (z(1:kk-1)-z(0:kk-2))
dden = den(1:kk-1)-den(0:kk-2)
logden = alog(den(1:kk-1)/den(0:kk-2))
for k=kk-2,0,-1 do col_den(k) = col_den(k+1)+dz(k)/logden(k)*dden(k)

return, col_den
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro get_density, f107, sza, zmin, zmax, z

common densities, N2den, O2den, Oden, N2col, O2col, Ocol,  $
                  N2col_sl, O2col_sl, Ocol_sl

; get a neutral atmosphere and column densities

mlyr = 100
msis, f107=f107, alt=z, zmin=zmin, zmax=zmax, mlyr=mlyr,  $
     N2den=N2den, O2den=O2den, Oden=Oden, rho=rho

; get slanted path column density
; for a level stratified atmosphere use chap=1./cos(sza*!dtor)
; Chapman function needs scaleheight =dz*(-N/dN) = -dz/d(log(N))
dz = (z(1:mlyr-1)-z(0:mlyr-2))*1e5  ;convert to cm
drho = -alog(rho(1:mlyr-1)/rho(0:mlyr-2))
scaleheight = [dz(0)/drho(0),dz/drho]*1e-5 ;convert back to km

chap = fltarr(mlyr)
for i=0,mlyr-1 do chap(i) = chapman(z(i), sza, scaleheight(i), zmin=zmin)

; NOTE: chapman returns 0 (instead of infinity) for shadow
; see note in get_ionrate
N2col = column_den(N2den, z)
N2col_sl = N2col*chap
O2col = column_den(O2den, z)
O2col_sl = O2col*chap
Ocol = column_den(Oden, z)
Ocol_sl = Ocol*chap

return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function sol_flux, f107

; parameterization of solar flux (seven wavelength intervals)

f1 = [5.2782, 3.236, 3.4064, 7.7625, 1.3588, 7.5532, 14.0433]
f2 = [11.5875, 13.3147, 30.4884, 14.3956, 5.6088, 20.0777, 37.0244]
return, (f1 + (f2-f1)*(f107-67.)/176.)*1e9

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro euv_ion, f107, EUV_N2, EUV_O2, EUV_O

; get dayside EUV ionization rate

; for each of the seven wavelengths we get a contribution:
; sigma.. is extinction cross section, sigma..p is ionization cross section
sigmaO = [.73, 6.54, 9.32, 9.72, 10.90, 11.98, 3.19]*1e-18
sigmaN2 = [.63, 6.46, 11.45, 11.60, 18.65, 23.52, 29.17]*1e-18
sigmaO2 = [2.07, 8.51, 15., 16., 18.59, 24.22, 15.62]*1e-18
sigmaOp = [.11, 6.62, 9.25, 9.72, 10.90, 11.98, 3.23]*1e-18
sigmaN2p = [.06, 6.58, 11.42, 11.60, 18.64, 23.28, 2.6]*1e-18
sigmaO2p = [3.43, 8.65, 14.92, 16., 18.61, 24.11, 8.]*1e-18

flux = sol_flux(f107)
get_ionrate, flux, sigmaN2, sigmaO2, sigmaO,  $
                 sigmaN2p, sigmaO2p, sigmaOp,  $
                 EUV_N2, EUV_O2, EUV_O, /slant

; secondary enhancement factor
sec_enhancement = 1.3
EUV_N2 = EUV_N2*sec_enhancement
EUV_O2 = EUV_O2*sec_enhancement
EUV_O = EUV_O*sec_enhancement

return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function sol_xray, f107

; solar X-ray flux

flux = [0.5, 0.4, 0.1]
efficiency = [3., 5., 7.]
wavelength = [70., 50., 35.]    ; Angstrom
energy = wavelength/(12400.*1.602e-12) ;actually 1/energy

return, flux*(1.+7.*(f107-67)/176.)*energy*efficiency
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro xray_ion, f107, x_N2, x_O2, x_O

; there are three x-ray wavelengths
sigmaN2 = [5.45e-19, 2.35e-19, 0.967e-19]
sigmaO2 = [9.28e-19, 4.4e-19,  1.64e-19]
sigmaO = [4.64e-19, 2.0e-19,  0.87e-19]
sigmaN2p = [5.45e-19, 2.35e-19, 0.967e-19]
sigmaO2p = [9.28e-19, 4.4e-19,  1.64e-19]
sigmaOp = [4.64e-19, 2.0e-19,  0.87e-19]

flux = sol_xray(f107)
get_ionrate, flux, sigmaN2, sigmaO2, sigmaO,  $
                 sigmaN2p, sigmaO2p, sigmaOp,  $
                 x_N2, x_O2, x_O, /slant

return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro geocorona_ion, f107, geo_N2, geo_O2, geo_O

; three wavelengths (H, and He): 1026, 584, 304 A

sigmaN2 = [0., 23.11, 11.61]*1e-18
sigmaO2 = [1.6, 22., 16.]*1e-18
sigmaO = [0., 10.24, 8.4]*1e-18
sigmaN2p = [0., 23.11, 11.61]*1e-18
sigmaO2p = [1., 22., 16.]*1e-18
sigmaOp = [0., 10.24, 8.4]*1e-18
flux = [1.e+7, 5.e+7, 5.e+7]    ;still have to find a f107 dependence

get_ionrate, flux, sigmaN2, sigmaO2, sigmaO,  $
                 sigmaN2p, sigmaO2p, sigmaOp,  $
                 geo_N2, geo_O2, geo_O

return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro get_ionrate, flux, sigmaN2, sigmaO2, sigmaO,  $
                 sigmaN2p, sigmaO2p, sigmaOp,  $
                 N2ionr, O2ionr, Oionr, slant=slant

common densities, N2den, O2den, Oden, N2col, O2col, Ocol,  $
                  N2col_sl, O2col_sl, Ocol_sl

; get the ionization rate from given densities, extinction and
; ionization cross sections.  The sza dependence is taken into
; account via the slanted path.  Note that the sigma.. and flux
; must have the same array dimensions.

; set up arrays
ll = n_elements(flux)
if n_elements(sigmaN2) ne ll or n_elements(sigmaN2p) ne ll then begin
   print, 'get_ionrate>> array flux and cross sections must ' + $
        'have same dimension'
   stop
endif
kk = n_elements(N2den)
N2ionr = fltarr(ll,kk)
O2ionr = fltarr(ll,kk)
Oionr = fltarr(ll,kk)

; determine column densities (slanted path or vertical for night side)
if keyword_set(slant) then begin
   col_O = Ocol_sl
   col_N2 = N2col_sl
   col_O2 = O2col_sl
endif else begin
   col_O = Ocol
   col_N2 = N2col
   col_O2 = O2col
endelse

; get ionization rates for each wavelength
; extinction is flux*exp(-tau) with tau : optical depth along the
; selected path
; since slant path is zero (instead of infinity) for shadow, we need
; to deal with this case as a special case (tau0)
for l=0,ll-1 do begin
   tau = sigmaO(l)*col_O+sigmaN2(l)*col_N2+sigmaO2(l)*col_O2
   tau0 = where(tau le 0, found_one)
   exptau = exp(-tau)
   if found_one gt 0 then exptau(tau0) = 0.
   N2ionr(l,*) = N2den*sigmaN2p(l)*flux(l)*exptau
   O2ionr(l,*) = O2den*sigmaO2p(l)*flux(l)*exptau
   Oionr(l,*) = Oden*sigmaO2p(l)*flux(l)*exptau
endfor

; total ionrate is sum over wavelengths
N2ionr = total(N2ionr, 1)
O2ionr = total(O2ionr, 1)
Oionr = total(Oionr, 1)

return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro solar_ion, f107=f107, sza=sza, zmin=zmin, zmax=zmax, _extra=_extra

; driver to calculate and plot the solar ionization rate

if n_elements(f107) eq 0 then f107 = 100
if n_elements(sza) eq 0 then sza = 0.
if n_elements(zmin) eq 0 then zmin = 80.
if n_elements(zmax) eq 0 then zmax = 500.

; set up neutral densities etc, and get various components of the
; ionization rate
get_density, f107, sza, zmin, zmax, z
euv_ion, f107, EUV_N2, EUV_O2, EUV_O
xray_ion, f107, x_N2, x_O2, x_O
geocorona_ion, f107, geo_N2, geo_O2, geo_O

; now make some plots

!p.multi = [0,2,2]
plot, EUV_N2+X_N2+geo_N2, z, /xlog, _extra=_extra,  $
     title='N!i2!n ionization rates', ytit='Altitude (km)'
oplot, EUV_N2, z, line=1
oplot, X_N2, z, line=2
oplot, geo_N2, z, line=3

plot, EUV_O2+X_O2+geo_O2, z, /xlog, _extra=_extra,  $
     title='O!i2!n ionization rates', ytit='Altitude (km)'
oplot, EUV_O2, z, line=1
oplot, X_O2, z, line=2
oplot, geo_O2, z, line=3

plot, EUV_O+X_O+geo_O, z, /xlog, _extra=_extra,  $
     title='O ionization rates', ytit='Altitude (km)'
oplot, EUV_O, z, line=1
oplot, X_O, z, line=2
oplot, geo_O, z, line=3

total_ion = EUV_N2+X_N2+geo_N2 + EUV_O2+X_O2+geo_O2 + EUV_O+X_O+geo_O
plot, total_ion, z, /xlog, _extra=_extra, title='Total ionization rate', $
     ytit='Altitude (km)'
oplot, EUV_N2+EUV_O2+EUV_O, z, line=1
oplot, x_N2+x_O2+x_O, z, line=2
oplot, geo_N2+geo_O2+geo_O, z, line=3

return
end
