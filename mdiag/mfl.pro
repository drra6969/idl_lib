  time = fltarr(1001)
  flux=time & df=time & dfi=time & dfr=time
  flint=time
  dumm3 = strarr(3)
  dumm=''

  t = fltarr(1001)
  dumm4 = strarr(4)
  funny=''
  fm=t & fk=t & ft=t & fb=t & fbi=t & fbr=t
  rfm=t & rfk=t & rft=t & rfb=t & rfbi=t & rfbr=t

  contin='' & withps='' & again='y' & fall=''

  openr, 2, 'magdfl1'
  readf, 2, dumm3
  print, dumm3

  flint(0)=0
  i=0
  while not eof(2) do begin
;    readf, 2, format='(f8.3,a1,4(f12.4,a1))', $
;              times, dumm, fluxs, dumm, dfs, dumm, dfis, dumm, dfrs, dumm

    readf, 2, times, fluxs, dfs, dfis, dfrs 

    time(i)=times & flux(i)=fluxs & df(i)=dfs & dfi(i)=dfis & dfr(i)=dfrs
    if i ge 1 then $
       flint(i)=flint(i-1)+0.5*(df(i)+df(i-1))*(time(i)-time(i-1))

;    print, times, fluxs
    itot=i
    if i lt 1000 then i=i+1 else stop, ' to many records '
  endwhile
  close, 2

  openr, 4, 'magdfl2'
  readf, 4, dumm4
  print, dumm4

  i=0
  while not eof(4) do begin
;    readf, 4, format='(f8.3,6(a1,f10.4))',$
;                     ts,dumm, fms,dumm, fks,dumm, fts,dumm, $
;                     fbs,dumm, fbis,dumm, fbrs
    readf, 4, ts, fms, fks, fts, fbs, fbis, fbrs
;    print, ts, fms, fks, fts, fbs, fbis, fbrs
;    readf, 4, format='(a7,5(f10.4,a1),f10.4)', $
;        funny,rfms,dumm,rfks,dumm,rfts,dumm,rfbs,dumm,rfbis,dumm,rfbrs
    readf, 4, rfms,rfks,rfts,rfbs,rfbis,rfbrs
                    
;    print, funny, rfms,dumm, rfks, rfts, rfbs, rfbis, rfbrs

    t(i)=ts & fm(i)=fms & fk(i)=fks & ft(i)=fts  
    fb(i)=fbs & fbi(i)=fbis & fbr(i)=fbrs

    rfm(i)=rfms & rfk(i)=rfks & rft(i)=rfts  
    rfb(i)=rfbs & rfbi(i)=rfbis & rfbr(i)=rfbrs

    itot=i
    if i lt 1000 then i=i+1 else stop, ' to many records '
  endwhile
  close, 4

     print, 'Which case?'
     read, fall

     while (again eq 'y') do begin

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='fl.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif

; first page
  !P.REGION=[0.,0.,1.0,1.25]
  !P.CHARSIZE=1.0
  print, 'plot first page?'
  read, contin
 if (contin eq '' or contin eq 'y') then begin
   !P.MULTI=[0,0,2]

  !P.POSITION=[0.15,0.1,0.7,0.35]
  amax = [[max(df(1:itot))], [max(dfi(1:itot))], [max(dfr(1:itot))]]
  amin = [[min(df(1:itot))], [min(dfi(1:itot))], [min(dfr(1:itot))]]
  plot, time(1:itot), df(1:itot),$
        title='!7 Magn. Flux Transfer Rate, ideal and nonideal Contr.',$
        xtitle='time', font=2, yrange=[min(amin), max(amax)]
  oplot, time(1:itot), dfi(1:itot), line=1
  oplot, time(1:itot), dfr(1:itot), line=2

  !P.POSITION=[0.15,0.5,0.7,0.75]
  amax = [[max(flux(1:itot))], [max(flint(1:itot))]]
  amin = [[min(flux(1:itot))], [min(flint(1:itot))]]
  plot, time(1:itot), flux(1:itot),$
        title='Total Flux Transfer',$
        xtitle='time', font=2, yrange=[min(amin), max(amax)]
  oplot, time(1:itot), flint(1:itot), line=2
    xpos=t(itot)+0.05*t(itot)
    ypos=0.5*(min(amin)+max(amax))
    xyouts,xpos,ypos,fall,font=3

 endif


 print, 'continue with next plot'
 read, contin
 if (contin eq '' or contin eq 'y') then begin

  !P.CHARSIZE=2.0
  !P.MULTI=[0,0,4]
  !P.POSITION=[0.15,0.05,0.7,0.23]
  amax = [[max(rfk(1:itot))], [max(rft(1:itot))], [max(rfb(1:itot))]]
  amin = [[min(rfk(1:itot))], [min(rft(1:itot))], [min(rfb(1:itot))]]
  plot, time(1:itot), rft(1:itot),$
        title='!7 Ratio Energy Fluxes to Magnet. Flux',font=2,$ 
        xtitle='time', yrange=[-0.5,0.5]
;        xtitle='time', yrange=[min(amin), max(amax)]
  oplot, time(1:itot), rfk(1:itot), line=1
  oplot, time(1:itot), rfb(1:itot), line=2

  !P.POSITION=[0.15,0.28,0.7,0.46]
  plot, time(1:itot), rfm(1:itot),$
        title='Ratio of Mass to Magnetic Flux',font=3,$
        yrange=[0.0,0.5]
;        yrange=[min(rfm(1:itot)), max(rfm(1:itot))]

  !P.POSITION=[0.15,0.51,0.7,0.69]
  amax = [[max(fk(1:itot))], [max(ft(1:itot))], [max(fb(1:itot))]]
  amin = [[min(fk(1:itot))], [min(ft(1:itot))], [min(fb(1:itot))]]
  plot, time(1:itot), ft(1:itot),$
        title='Energy Fluxes',$
        font=3, yrange=[min(amin), max(amax)]
  oplot, time(1:itot), fk(1:itot), line=1
  oplot, time(1:itot), fb(1:itot), line=2

  !P.POSITION=[0.15,0.74,0.7,0.92]
  plot, time(1:itot), fm(1:itot),$
        title='Mass Flux', font=3, $
        yrange=[min(fm(1:itot)), max(fm(1:itot))]
    xpos=t(itot)+0.05*t(itot)
    ypos=0.5*(min(fm(1:itot))+max(fm(1:itot)))
    xyouts,xpos,ypos,fall,font=3

   endif


   print, 'continue with next plot'
   read, contin
   if (contin eq '' or contin eq 'y') then begin

    !P.MULTI=[0,0,1]
    !P.CHARSIZE=2.0
    !P.POSITION=[0.1,0.3,0.75,0.6]
    shear=[30,60,90,120,150]
    pr2=[.5, .21, .23, .26, .29]
    pr3=[.5, .19, .2, .22, .3]
    shl2=[150]
    prl2=[.35]
    shl3=[150]
    prl3=[.39]
;    shl2=[150]
;    prl2=[.32]
;    shl3=[150]
;    prl3=[.31]


    plot, shear, pr2,psym=1,$
        title='!7 Mass Flux / Magnetic Flux',$
        font=2,xtitle='Magn. Shear',$ 
        xrange=[0,180],yrange=[0, 0.5]
    oplot,shear, pr3,psym=2 
    oplot,shl2,prl2,psym=3
    oplot,shl2,prl2,psym=3

   endif

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile

end

