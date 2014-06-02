  time = fltarr(1001,5)
  flux=time & df=time & dfi=time & dfr=time
  flint=time
  dumm3 = strarr(3)
  dumm=''

  t = fltarr(1001,5)
  dumm4 = strarr(4)
  funny=''
  fm=t & fk=t & ft=t & fb=t & fbi=t & fbr=t
  rfm=t & rfk=t & rft=t & rfb=t & rfbi=t & rfbr=t
  contin='' & withps='' & again='y' & fall=''
  fnumber=1 & nop=1 & itot=intarr(5)

  print, 'how many plots (n=1 digit<=5)'
  read, nop
  
 for iop=0, nop-1  do  begin 
  print, 'what filenumber'
  read, fnumber
  openr, 2, 'magdfl1'+string(fnumber,'(i1)')
  readf, 2, dumm3
  print, dumm3

  flint(0,iop)=0
  i=0
  while not eof(2) do begin
    readf, 2, times, fluxs, dfs, dfis, dfrs 

    time(i,iop)=times & flux(i,iop)=fluxs 
    df(i,iop)=dfs & dfi(i,iop)=dfis & dfr(i,iop)=dfrs
    if i ge 1 then   flint(i,iop)=flint(i-1,iop) $
             +0.5*(df(i,iop)+df(i-1,iop))*(time(i,iop)-time(i-1,iop))

    itot(iop)=i
    if i lt 1000 then i=i+1 else stop, ' to many records '
  endwhile
  close, 2

  openr, 4, 'magdfl2'+string(fnumber,'(i1)')
  readf, 4, dumm4
  print, dumm4

  i=0
  while not eof(4) do begin
    readf, 4, ts, fms, fks, fts, fbs, fbis, fbrs
    readf, 4, rfms,rfks,rfts,rfbs,rfbis,rfbrs

    t(i,iop)=ts & fm(i,iop)=fms & fk(i,iop)=fks & ft(i,iop)=fts  
    fb(i,iop)=fbs & fbi(i,iop)=fbis & fbr(i,iop)=fbrs

    rfm(i,iop)=rfms & rfk(i,iop)=rfks & rft(i,iop)=rfts  
    rfb(i,iop)=rfbs & rfbi(i,iop)=rfbis & rfbr(i,iop)=rfbrs

    itot(iop)=i
    if i lt 1000 then i=i+1 else stop, ' to many records '
  endwhile
  close, 4
 endfor

     while (again eq 'y') do begin
       print, 'which dataset; 1 <=input<= ',nop
       read, iop
       iop=iop-1
       print, 'Which case?'
       read, fall
     

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
  amax = [[max(df(1:itot(iop),iop))], $
         [max(dfi(1:itot(iop),iop))], [max(dfr(1:itot(iop),iop))]]
  amin = [[min(df(1:itot(iop),iop))], $
         [min(dfi(1:itot(iop),iop))], [min(dfr(1:itot(iop),iop))]]
  plot, time(1:itot(iop),iop), df(1:itot(iop),iop),$
        title='!7 Magn. Flux Transfer Rate, ideal and nonideal Contr.',$
        xtitle='time', font=2, yrange=[min(amin), max(amax)]
  oplot, time(1:itot(iop),iop), dfi(1:itot(iop),iop), line=1
  oplot, time(1:itot(iop),iop), dfr(1:itot(iop),iop), line=2

  !P.POSITION=[0.15,0.5,0.7,0.75]
  amax = [[max(flux(1:itot(iop),iop))], [max(flint(1:itot(iop),iop))]]
  amin = [[min(flux(1:itot(iop),iop))], [min(flint(1:itot(iop),iop))]]
  plot, time(1:itot(iop),iop), flux(1:itot(iop),iop),$
        title='Total Flux Transfer',$
        xtitle='time', font=2, yrange=[min(amin), max(amax)]
  oplot, time(1:itot(iop),iop), flint(1:itot(iop),iop), line=2
    xpos=t(itot(iop))+0.05*t(itot(iop))
    ypos=0.5*(min(amin)+max(amax))
    xyouts,xpos,ypos,fall,font=3

 endif


 print, 'continue with next plot'
 read, contin
 if (contin eq '' or contin eq 'y') then begin

  !P.CHARSIZE=2.0
  !P.MULTI=[0,0,4]
  !P.POSITION=[0.15,0.05,0.7,0.23]
  amax = [[max(rfk(1:itot(iop),iop))], $
         [max(rft(1:itot(iop),iop))], [max(rfb(1:itot(iop),iop))]]
  amin = [[min(rfk(1:itot(iop),iop))], $
         [min(rft(1:itot(iop),iop))], [min(rfb(1:itot(iop),iop))]]
  plot, time(1:itot(iop),iop), rft(1:itot(iop),iop),$
        title='!7 Ratio Energy Fluxes to Magnet. Flux',font=2,$ 
        xtitle='time', yrange=[-0.5,0.5]
;        xtitle='time', yrange=[min(amin), max(amax)]
  oplot, time(1:itot(iop),iop), rfk(1:itot(iop),iop), line=1
  oplot, time(1:itot(iop),iop), rfb(1:itot(iop),iop), line=2

  !P.POSITION=[0.15,0.28,0.7,0.46]
  plot, time(1:itot(iop),iop), rfm(1:itot(iop),iop),$
        title='Ratio of Mass to Magnetic Flux',font=3,$
        yrange=[0.0,0.5]
;        yrange=[min(rfm(1:itot)), max(rfm(1:itot))]

  !P.POSITION=[0.15,0.51,0.7,0.69]
  amax = [[max(fk(1:itot(iop),iop))], $
         [max(ft(1:itot(iop),iop))], [max(fb(1:itot(iop),iop))]]
  amin = [[min(fk(1:itot(iop),iop))], $
         [min(ft(1:itot(iop),iop))], [min(fb(1:itot(iop),iop))]]
  plot, time(1:itot(iop),iop), ft(1:itot(iop),iop),$
        title='Energy Fluxes',$
        font=3, yrange=[min(amin), max(amax)]
  oplot, time(1:itot(iop),iop), fk(1:itot(iop),iop), line=1
  oplot, time(1:itot(iop),iop), fb(1:itot(iop),iop), line=2

  !P.POSITION=[0.15,0.74,0.7,0.92]
  plot, time(1:itot(iop),iop), fm(1:itot(iop),iop),$
        title='Mass Flux', font=3, $
        yrange=[min(fm(1:itot(iop),iop)), max(fm(1:itot(iop),iop))]
    xpos=t(itot(iop),iop)+0.05*t(itot(iop),iop)
    ypos=0.5*(min(fm(1:itot(iop),iop))+max(fm(1:itot(iop),iop)))
    xyouts,xpos,ypos,fall,font=3

   endif



     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile

end

