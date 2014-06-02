PRO efieldsimh, n,ress

COMMON units, nnorm,bnorm,vnorm,pnorm,lnorm,tnorm,tempnorm,jnorm,$
              nfac,bfac,vfac,pfac,lfac,tfac,tempfac,jfac
COMMON ref, br,vr,rhor,pr,babr,ptotr,pbr,tempr,beta,t,$
            jr,er,rsat,vrsat,index,starttime,xtit,withps,$
            satchoice,withunits
  
  ex=fltarr(3,n) & ey=ex & ez=ex
  ex = vr(2,*)*br(1,*) - vr(1,*)*br(2,*)+ress(*)*jr(0,*)
  ey = vr(0,*)*br(2,*) - vr(2,*)*br(0,*)+ress(*)*jr(1,*)
  ez = vr(1,*)*br(0,*) - vr(0,*)*br(1,*)+ress(*)*jr(2,*)
print,'Eta J:',ress(*)*jr(2,*)
  if withunits eq 'y' then begin
    c = 0.001             ; to obtain mV/m
    ex = c*ex
    ey = c*ey
    ez = c*ez
  endif
  er = [ex,ey,ez]

return
end

