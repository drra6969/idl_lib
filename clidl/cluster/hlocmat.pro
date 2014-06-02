Pro hlocmat, isc,scdr1,scdr2,scdr3,scdr4,volrat,critvol,rr

   if ( (scdr3(0,isc) ne 0.0) or (scdr3(1,isc) ne 0.0) $
                         or (scdr3(1,isc) ne 0.0) )  then begin 
     print, 'SC 3 is not reference system!!!!!!!!'
     return
   endif  
   rr=fltarr(3,3) & rin=rr
   rin(0,*)=scdr1(*,isc) & rin(1,*)=scdr2(*,isc) & rin(2,*)=scdr4(*,isc)
;   print,rin
   rr=invert(rin)
;   print,rr
 return
end