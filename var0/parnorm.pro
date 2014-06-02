PRO parnorm
;----------------------------------------
COMMON procommon, nsat,startime,itot,pi,ntmax, $
                  rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd, $
                  xsi,ysi,zsi,vxsi,vysi,vzsi, $
                  time,bxs,bys,bzs,vxs,vys,vzs,rhos,ps,bs,ptots, $
                  jxs,jys,jzs,xs,ys,zs,cutalong
COMMON units, nnorm,bnorm,vnorm,pnorm,lnorm,tnorm,tempnorm,jnorm,$
              nfac,bfac,vfac,pfac,lfac,tfac,tempfac,jfac
COMMON ref, br,vr,rhor,pr,babr,ptotr,pbr,tempr,beta,t, $
            jr,er,rsat,vrsat,index,starttime,xtit,withps,$
            satchoice,withunits

; determines: Normalization nnorm,bnorm,vnorm,pnorm,lnorm,tnorm,$
;              rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd,$
;              xsat0,ysat0,zsat0,vxsat,vysat,vzsat,$
;              time,bxs,bys,bzs,vxs,vys,vzs,rhos,ps,bs,ptots,xs,ys,zs
; UNCOMMENT FOR DIFFERENT NORMALISATION  
;--------------------------------------
     nnorm = 7.5                    ; for cm^(-3)
     bnorm = 50.0                    ; for nT
     lnorm = 400.0                    ; for km
     vnorm = 21.8*bnorm/sqrt(nnorm)  ; for km/s
     pnorm = 0.01*bnorm^2.0/8.0/pi   ; for nPa
     tnorm = lnorm/vnorm              ; for s
;     boltz = 1.38*10^(-23)           ; 
     tempnorm = pnorm*10./1.38/1.16/nnorm  ; for keV
     jnorm=10000.*bnorm/4./pi/lnorm  ; in 10^-9 A/m^2
    if withunits eq 'y' then begin 
      bfac=bnorm & vfac=vnorm & lfac=lnorm & nfac=nnorm & pfac=pnorm
      tempfac=tempnorm & jfac=jnorm
      tfac=tnorm & xtit='time (s)'
      if (satchoice eq '2' or satchoice eq '3') then begin
         tfac=lnorm 
         xtit=cutalong+' (km)'
      endif
    endif else begin
      bfac=1. & vfac=1. & lfac=1. & nfac=1. & pfac=1.
      tempfac=1. & jfac=1.
      tfac=1.  & xtit='time'
      if (satchoice eq '2' or satchoice eq '3') then xtit=cutalong
    endelse   
    print,'In parnorm, tfac:', tfac   

    t     = tfac*time(*)
    rsat  = lfac*[xsi(index),ysi(index),zsi(index)] 
    vrsat = vfac*[vxsi(index),vysi(index),vzsi(index)]
    br    = bfac*[bxs(index,*),bys(index,*),bzs(index,*)]
    vr    = vfac*[vxs(index,*),vys(index,*),vzs(index,*)]
    rhor  = nfac*rhos(index,*)
    pr    = pfac*ps(index,*)
    babr  = bfac*bs(index,*)
    ptotr = pfac*ptots(index,*)
    pbr   = pfac*bs(index,*)*bs(index,*)
    tempr = tempfac*ps(index,*)/rhos(index,*)
    beta  = ps(index,*)/bs(index,*)^2
    if (satchoice eq '2' or satchoice eq '3') then $
      jr = jfac*[jxs(index,*),jys(index,*),jzs(index,*)]

return
end

