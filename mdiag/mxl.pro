  t = fltarr(1001)
  xl=fltarr(1001,5) & yl=xl & cu=xl & ef=xl & sl=xl & fmax=xl & fmin=xl
  no=1 & nomax=1
  dumm4 = strarr(4)
  dumm3 = strarr(3)
  dumm=''
  contin='' & again='y' & withps='n' & fall=''

  openr, 2, 'magdxl'
  readf, 2, dumm3
  print, dumm3

  i=-1
  while not eof(2) do begin
    readf, 2, ts, xls, yls, cus, efs, sls, fmaxs, fmins, no

    if  (no eq 1) then i=i+1 
    if (no gt nomax) then nomax=no

    t(i)=ts & xl(i,no-1)=xls & yl(i,no-1)=yls & cu(i,no-1)=cus & ef(i,no-1)=efs 
    sl(i,no-1)=sls & fmax(i,no-1)=fmaxs & fmin(i,no-1)=fmins

    if (i eq 1000) then stop, ' to many records '
    itot=i
  endwhile
  close, 2

     print, 'Which case?'
     read, fall

     while again eq 'y' do begin

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='xl.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif
; first page
  !P.REGION=[0.,0.,1.0,1.25]

  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

    !P.CHARSIZE=2.0  
    !P.MULTI=[0,0,4]
    !P.POSITION=[0.15,0.75,0.7,0.95]
    amax=max(xl)
    amin=min(xl)
    plot, t(0:itot), xl(0:itot,0), line=0,$
        title='X coord. of X lines',$
        font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(0:itot), xl(0:itot,in), line=in

    !P.POSITION=[0.15,0.5,0.7,0.7]
    amax=max(yl)
    amin=min(yl)
    plot, t(0:itot), yl(0:itot,0), line=0,$
        title='Y coord. of X lines',$
        font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(0:itot), yl(0:itot,in), line=in

    !P.POSITION=[0.15,0.25,0.7,0.4]
    amax=max(fmax)
    amin=min(fmax)
    plot, t(0:itot), fmax(0:itot,0), line=0,$
        title='Flux for Y>Yxl',$
        font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(0:itot), fmax(0:itot,in), line=in

    !P.POSITION=[0.15,0.05,0.7,0.2]
    amax=max(fmin)
    amin=min(fmin)
    plot, t(0:itot), fmin(0:itot,0), line=0,$
        title='Flux for Y<Yxl',$
        xtitle='time', font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(0:itot), fmin(0:itot,in), line=in

  endif

  print, 'plot second page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

    !P.POSITION=[0.15,0.7,0.7,0.9]
    amax=max(ef)
    amin=min(ef)
    plot, t(0:itot), ef(0:itot,0), line=0,$
        title='Electric Field at X lines',$
        xtitle='time', font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(0:itot), ef(0:itot,in), line=in
    xpos=t(itot)+0.05*t(itot)
    ypos=0.5*amax
    xyouts,xpos,ypos,fall,font=3

    !P.POSITION=[0.15,0.4,0.7,0.6]
    amax=max(cu)
    amin=min(cu)
    plot, t(0:itot), cu(0:itot,0), line=0,$
        title='Current Density at X lines',$
        font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(0:itot), cu(0:itot,in), line=in

    !P.POSITION=[0.15,0.1,0.7,0.3]
    amax=max(sl)
    amin=min(sl)
    plot, t(0:itot), sl(0:itot,0), line=0,$
        title='Tild of Current Layer',$
        font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(0:itot), sl(0:itot,in), line=in


  endif


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile


end




