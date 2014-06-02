PRO sorteigen, e0,e1,e2,out0,out1,out2


 proj=fltarr(3) & sign0=1 & sign2=1 
 
 proj(0) = total(e0*out0) & proj(1) = total(e0*out1) & proj(2) = total(e0*out2)
 maxp=max([abs(proj(0)),abs(proj(1)),abs(proj(2))],i)
 if proj(i) lt 0 then sign0=-1
 if i eq 0 then out0=sign0*out0
 if i eq 1 then out1=sign0*out1
 if i eq 2 then out2=sign0*out2

 proj(0) = total(e2*out0) & proj(1) = total(e2*out1) & proj(2) = total(e2*out2)
 maxp=max([abs(proj(0)),abs(proj(1)),abs(proj(2))],k)
 if proj(k) lt 0 then sign2=-1
 if k eq 0 then out0=sign2*out0
 if k eq 1 then out1=sign2*out1
 if k eq 2 then out2=sign2*out2
 
 if (i ne 0) and (k ne 0) then  out0=sign0*sign2*out0
 if (i ne 1) and (k ne 1) then  out1=sign0*sign2*out1
 if (i ne 2) and (k ne 2) then  out2=sign0*sign2*out2

return
end
