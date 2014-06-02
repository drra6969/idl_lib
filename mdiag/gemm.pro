  nof=3
  print, 'how many files'
  read, nof
  t = fltarr(1001,nof)  &  itot=intarr(nof)
  fl=fltarr(1001,nof) & er=fl & fll=fl & err=fl
  ers=fl & errs=fl 
  ti=fltarr(1001) & dfl=ti & dfll=ti & efl=ti & efll=ti 
  efls=ti & eflls=ti

  no=1 & nomax=1 & xoo=0 & xox=0 & nox=1 & noo=1 & 
  ts=0.0 & flin=0.0 & erin = 0.0
  dumm4 = strarr(20)  & incase='ll' & cas=strarr(nof)
  dumm3 = strarr(3)
  dumm=''
  contin='' & again='y' & withps='n' & fall=''

  for iff=0, nof-1 do begin
    print, 'what filenumber'
    read, fnumber
;    print, 'what case'
;    read, incase
    if (incase ne '') then cas(iff)=incase
  
    openr, 2, 'datxl'+string(fnumber,'(i1)')

    readf, 2, dumm3
    print, dumm3

    i=-1
    while not eof(2) do begin
      readf, 2, ts, flin,erin

      i=i+1
      t(i,iff)=ts & fl(i,iff)=flin & er(i,iff)=erin 
      if (i eq 1000) then stop, ' to many records '
      itot(iff)=i-1
    endwhile
    close, 2

  endfor


  for iff=0, nof-1 do begin
    itott=itot(iff) &  ti(*)=t(*,iff) & dfl(*)=fl(*,iff) & efl(*)=er(*,iff)
    dt=ti(1:itott)-ti(0:(itott-1))
;   reconnected flux from integr of reconnection rate
    for it=1,itott do $
      dfll(it)=dfll(it-1)+0.5*dt(it-1)*( efl(it-1)+efl(it) )       
;   reconnection rate from the reconnected flux
      efll(1:itott)=(dfl(1:itott)-dfl(0:(itott-1)))/dt  
; smoothed reconnection rates:
    efls=smooth(efl,3)
    eflls=0.5*(smooth(efll,3)+smooth(efl,3))
    fll(*,iff)=dfll(*)
    err(*,iff)=efll(*)
    ers(*,iff)=efls(*)
    errs(*,iff)=eflls(*)
  endfor


     while again eq 'y' do begin

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='xl.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
        !P.THICK=3
       endif
; first page
  !P.REGION=[0.,0.,1.0,1.25]

  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

    !P.CHARSIZE=2.0  
    !P.MULTI=[0,0,4]

    !P.THICK=1.5


    !P.POSITION=[0.1,0.50,0.65,0.8]
    tmax=max(t)
    amax=max(fl)
    amin=0.0
    xo1=tmax-10
    yt1=1.1*amax
    yt2=1.0*amax
    yt3=0.9*amax
    yt4=0.8*amax


    plot, t(0:itot(0),0), fl(0:itot(0),0), line=0,$
        title='Reconnected Flux',$
        font=3, yrange=[amin,amax], xrange=[0,20]
    for in=1, nof-1 do oplot, t(0:itot(in),in), fl(0:itot(in),in), line=in
        xyouts, xo1, yt1,'_____ m1',charsize=1.3,charthick=2
        xyouts, xo1, yt2,'........ m2',charsize=1.3,charthick=2
        xyouts, xo1, yt3,'_ _ _ m3',charsize=1.3,charthick=2
;        xyouts, xo1, yt4,'_ . _ 0.4',charsize=1.3,charthick=2
;        xyouts, xo1, yt1,'_____ MHD-res=const',charsize=1.3,charthick=2
;        xyouts, xo1, yt2,'........ MHD-res(j)',charsize=1.3,charthick=2
;        xyouts, xo1, yt3,'_ _ _ Hall-res=const',charsize=1.3,charthick=2
;        xyouts, xo1, yt4,'_ . _ Hall-res(j)',charsize=1.3,charthick=2


    !P.POSITION=[0.1,0.1,0.65,0.4]
    amax=0.25
;    amax=max(er)
    amin=min(er)
;    amax=max(er(0:itot,nomax-1))
;    amin=min(er(0:itot,nomax-1))
    plot, t(0:itot(0),0), smooth(errs(0:itot(0),0),3), line=0,$
        title='E_xl-E_ol',$
        xtitle='time', font=3, yrange=[0,0.25], xrange=[0,20]
    for in=1, nof-1 do begin
        oplot, t(0:itot(in),in), errs(0:itot(in),in), line=in
    endfor


  endif



     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile


end




