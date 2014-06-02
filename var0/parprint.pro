PRO parprint
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

; declare some variable to record and print boundary values
  bmsp=fltarr(3) & bmsh=bmsp & vmsp=bmsp & vmsh=bmsp 
  bmsp(0)=bfac*bxbd(0) & bmsp(1)=bfac*bybd(0) & bmsp(2)=bfac*bzbd(0) 
  bmsh(0)=bfac*bxbd(1) & bmsh(1)=bfac*bybd(1) & bmsh(2)=bfac*bzbd(1) 
  vmsp(0)=0.0 & vmsp(1)=0.0 & vmsp(2)=0.0
  vmsh(0)=vfac*(vxbd(1)-vxbd(0))
  vmsh(1)=vfac*(vybd(1)-vybd(0))
  vmsh(2)=vfac*(vzbd(1)-vzbd(0))
  rhotmsp=fltarr(2) & rhotmsh=rhotmsp
  rhotmsp(0)=nfac*rhobd(0) & rhotmsp(1)=tempfac*pbd(0)/rhobd(0)
  rhotmsh(0)=nfac*rhobd(1) & rhotmsh(1)=tempfac*pbd(1)/rhobd(1)
  
satindex:
; CHOOSE SATELLITE INDEX
;------------------------
  print,'Normalisation for : '
  print, $
  '  No density ! magn. field!  velocity  !  pressure  !length units! time'
  print, $
  '   cm**(-3)  !     nT     !    km/s    !   nPascal  !    km      !   s'
  print, format='(f10.2,f12.2,f13.2,f13.4,f13.2,f12.4)',$
        nnorm,bnorm,vnorm,pnorm,lnorm,tnorm
  print,'Initial asymptotic values at xmin (1. row, magnetosphere) ',$
        'and xmax (2. row, magnetosheath) for: '
  print, $
  '     rho      p        vx       vy       vz       bx       by       bz'
  print, format='(8f9.3)',$
       rhobd(0),pbd(0),vxbd(0),vybd(0),vzbd(0),bxbd(0),bybd(0),bzbd(0)
  print, format='(8f9.3)',$
       rhobd(1),pbd(1),vxbd(1),vybd(1),vzbd(1),bxbd(1),bybd(1),bzbd(1)
  print,' and for magnetospheric (rotated into GSM) coordinates: '
  print, $
  '     rho      p        vx       vy       vz       bx       by       bz'
  print, format='(8f9.3)',$
       rhobd(0),pbd(0),vxbd(0),vybd(0),vzbd(0),bxbd(0),bybd(0),bzbd(0)
  print, format='(8f9.3)',$
       rhobd(1),pbd(1),vxbd(1),vybd(1),vzbd(1),bxbd(1),bybd(1),bzbd(1)
  print, 'Initial satellite locations and velocity:'
  print, 'sat  !    x    !    y    !    z    !    vx   !    vy   !    vz   '
  for i=0,nsat-1 do $
    print, format='(i3,6f10.2)',$
           i,xsi(i),ysi(i),zsi(i),vxsi(i),vysi(i),vzsi(i)
  print, $
'Satellite velocity is relative to restframe of the magnetosphere (at xmin)!!!'
  print, $
  ' Add vy,vz at xmin to obtain the probe velocity in the simulation frame.'
  print, 'Plasma velocity is recorded in the satellite frame!'
  print, 'Parameters are in simulation coordinates!'

end
