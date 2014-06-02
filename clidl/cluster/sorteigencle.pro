PRO sorteigencle, e1,e2,e3


 sign0=1 & sign2=1 
 if e1(1) gt 0 then sign0=-1
 if e3(2) lt 0 then sign2=-1
 
 e1=e1*sign0 & e2=e2*sign0*sign2 & e3=e3*sign2
 
 tt = crossp(e1,e2) 

return
end
