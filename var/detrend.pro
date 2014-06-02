Pro detrend, time,signal,arrayout

;linear least square fit
arrayout=FLTARR(N_ELEMENTS(signal))
yfit=0
yband=0
sigma=0
a0=0
dsignal=0
lfit=POLY_FIT(time,signal,1,yfit,yband,sigma,a0)
;signal after extracting the linear least square fit from it
arrayout=signal-yfit

return
end
