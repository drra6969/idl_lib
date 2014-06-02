Pro plotvars, coordstrn,base

COMMON ref, br,vr,rhor,pr,babr,ptotr,pbr,tempr,beta,t, $
            jr,er,rsat,vrsat,index,starttime,xtit,withps,$
            satchoice,withunits
COMMON pltvar, vplot,bplot

  vplot=vr 
  bplot=br
  vplot=base#vplot & bplot=base#bplot
 return
end
