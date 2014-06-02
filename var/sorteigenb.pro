PRO sorteigenb, e1,e2,e3


 sign0=1 & sign2=1 
 max0=max([abs(e1(0)),abs(e1(1)),abs(e1(2))],i)
 if e1(i) lt 0 then sign0=-1
 max1=max([abs(e3(0)),abs(e3(1)),abs(e3(2))],i)
 if e3(i) lt 0 then sign2=-1
 
 e1=e1*sign0 & e2=e2*sign0*sign2 & e3=e3*sign2
 
 tt = crossp(e1,e2) 

return
end
