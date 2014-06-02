PRO readtr, fnumber, xt,yt,zt

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, x,y, xf,yf, xn,yn, iox,ioy, $
                    ioxf,ioyf, run, time

   dumm = '' & istep=1 & nfl=1 & tt=0.0 & maxt=500

   name='magdtraj'
   openr, 2, name
   readf, 2, dumm
;     print, dumm
   readf, 2, dumm
;     print, dumm
   readf, 2, nfl
;     print, 'nfl',nfl
   timefl=fltarr(maxt)
   xtfl=fltarr(nfl,maxt) & ytfl=fltarr(nfl,maxt) & ztfl=fltarr(nfl,maxt)
   xt=fltarr(nfl) & yt=fltarr(nfl) & zt=fltarr(nfl)
   
   i=0
   while not eof(2) do begin
     readf, 2, dumm
;     print, dumm
     readf, 2, dumm
;     print, dumm
     readf, 2, istep,tt
;     print, istep, tt
     timefl(i)=tt
     readf, 2, dumm & readf, 2, xt
;     print, 'X' & print, xt
     readf, 2, dumm & readf, 2, yt
;     print, 'Y' & print, yt
     readf, 2, dumm & readf, 2, zt
;     print, 'Z' & print, zt
     xtfl(*,i)=xt(*) & ytfl(*,i)=yt(*) & ztfl(*,i)=zt(*)
     itot=i
     if i lt maxt then i=i+1 else stop, ' to many records '
   endwhile
   close,2
   xt=xtfl(*,fnumber) & yt=ytfl(*,fnumber) & zt=ztfl(*,fnumber)
   print, 'fnumber:',fnumber
   print, 'X' & print, xt
   print, 'Y' & print, yt
   print, 'Z' & print, zt

return
end

