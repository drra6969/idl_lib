Pro hclplotvar, coordstrn,base

COMMON bfield, nfgm1,timem1,statsm1,b1,varbt1,varbb1, $
               nfgm2,timem2,statsm2,b2,varbt2,varbb2, $
               nfgm3,timem3,statsm3,b3,varbt3,varbb3, $
               nfgm4,timem4,statsm4,b4,varbt4,varbb4
COMMON bulk, n1,v1,t1,p1,n3,v3,t3,p3,n4,v4,t4,p4
COMMON pltvar, vplot1,vplot3,vplot4,bplot1,bplot2,bplot3,bplot4

  vplot1=v1 & vplot3=v3 & vplot4=v4
  bplot1=b1 & bplot2=b2 & bplot3=b3 & bplot4=b4
  if coordstrn ne 'GSM' then begin
    vplot1=base#vplot1 & vplot3=base#vplot3 & vplot4=base#vplot4
    bplot1=base#bplot1 & bplot2=base#bplot2
    bplot3=base#bplot3 & bplot4=base#bplot4
  endif
 return
end