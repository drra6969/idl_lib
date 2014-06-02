Pro hindbd,min,max,n,time,its,ite

   its=0 & while ((time(its) lt min) and (its lt n-1)) do its=its+1
   ite=n-1 & while ((time(ite) gt max) and (ite gt 0)) do ite=ite-1
   if ite lt its then ite=its+3 
 return
end