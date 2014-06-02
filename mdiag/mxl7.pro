 ; This version of the mxl program has been modified  
  
  t = fltarr(1001)
  xl=fltarr(1001,5)
  yl=xl & cu=xl & ef=xl & sl=xl & fmax=xl & fmin=xl
  vx=xl & vy=xl
  yave=t & vyave=t
  numxl=intarr(1001)
  istart=intarr(5)
  no=1 & nomax=1
  dumm4 = strarr(4)
  dumm3 = strarr(3)
  dumm=''
  contin='' & again='y' & withps='n' & fall=''
  numxl(*)=1
  istart(*)=0

  openr, 2, 'magdxl'
  readf, 2, dumm3

  i=-1
  while not eof(2) do begin
    readf, 2, ts, xls, yls, cus, efs, sls, fmaxs, fmins, no

    if  (no eq 1) then begin
      i=i+1
      if (i ne 0) then numxl(i-1)=lastno
    endif
    if (no gt nomax) then begin
      nomax=no
      istart(nomax-1)=i
    endif

    t(i)=ts & xl(i,no-1:4)=xls & yl(i,no-1:4)=yls & cu(i,no-1:4)=cus
    ef(i,no-1:4)=efs & sl(i,no-1:4)=sls
    fmax(i,no-1:4)=fmaxs & fmin(i,no-1:4)=fmins

    if (i ne 0) then begin
      vx(i-1,no-1:4)=(xl(i,no-1)-xl(i-1,no-1))/(t(i)-t(i-1))
      vy(i-1,no-1:4)=(yl(i,no-1)-yl(i-1,no-1))/(t(i)-t(i-1))
    endif

    lastno=no

    if (i eq 1000) then stop, ' too many records '
    itot=i
  endwhile
  close, 2
  numxl(itot)=lastno


  yave(0:itot)=yl(0:itot,0)
  vyave(0:itot-1)=vy(0:itot-1,0)

  for i=0, itot do begin
    if (numxl(i) gt 1) then begin
      nxl=numxl(i)
      for j=1, nxl-1 do begin
	yave(i)=yave(i) + yl(i,j)
	if (i lt itot) then vyave(i)=vyave(i) + vy(i,j)
      endfor
      yave(i)=yave(i)/nxl
      if (i lt itot) then vyave(i)=vyave(i)/nxl
    endif
  endfor

     print, 'Which case?'
     read, fall

     while again eq 'y' do begin

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
         device,filename='xl4.ps'
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
    print, amin, amax
    plot, t(0:itot), xl(0:itot,0), line=0,$
        title='X coord. of X lines',$
        font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(istart(in):itot), $
      xl(istart(in):itot,in), line=in

    !P.POSITION=[0.15,0.5,0.7,0.7]
    amax=max(yl)
    amin=min(yl)
    plot, t(0:itot), yl(0:itot,0), line=0,$
        title='Y coord. of X lines',$
        font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(istart(in):itot), $
      yl(istart(in):itot,in), line=in
    oplot, t(istart(in):itot), yave(istart(in):itot), line=in+1

    !P.POSITION=[0.15,0.25,0.7,0.4]
    amax=max(fmax)
    amin=min(fmax)
    plot, t(0:itot), fmax(0:itot,0), line=0,$
        title='Flux for Y>Yxl',$
        font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(istart(in):itot), $
      fmax(istart(in):itot,in), line=in

    !P.POSITION=[0.15,0.05,0.7,0.2]
    amax=max(fmin)
    amin=min(fmin)
    plot, t(0:itot), fmin(0:itot,0), line=0,$
        title='Flux for Y<Yxl',$
        xtitle='time', font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(istart(in):itot), $
      fmin(istart(in):itot,in), line=in

  endif


; second page
  !P.REGION=[0.,0.,1.0,1.25]

  print, 'plot second page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

    !P.CHARSIZE=2.0
    !P.MULTI=[0,0,3]
    !P.POSITION=[0.15,0.7,0.7,0.9]
    amax=max(ef)
    amin=min(ef)
    plot, t(0:itot), ef(0:itot,0), line=0,$
        title='Electric Field at X lines',$
        xtitle='time', font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(istart(in):itot), $
      ef(istart(in):itot,in), line=in
    xpos=t(itot)+0.05*t(itot)
    ypos=0.5*amax
    xyouts,xpos,ypos,fall,font=3

    !P.POSITION=[0.15,0.4,0.7,0.6]
    amax=max(cu)
    amin=min(cu)
    plot, t(0:itot), cu(0:itot,0), line=0,$
        title='Current Density at X lines',$
        font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(istart(in):itot), $
      cu(istart(in):itot,in), line=in

    !P.POSITION=[0.15,0.1,0.7,0.3]
    amax=max(sl)
    amin=min(sl)
    plot, t(0:itot), sl(0:itot,0), line=0,$
        title='Tild of Current Layer',$
        font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(istart(in):itot), $
      sl(istart(in):itot,in), line=in


  endif


; third page
  !P.REGION=[0.,0.,1.0,1.25]

  print, 'plot third page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

    !P.CHARSIZE=1.0
    !P.MULTI=[0,0,2]
    !P.POSITION=[0.15,0.7,0.7,0.9]
    amax=max(vx)
    amin=min(vx)
    plot, t(0:itot-1), vx(0:itot-1,0), line=0,$
        title='X Velocity of X lines',$
        xtitle='time', font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(istart(in)+1:itot-1), $
      vx(istart(in)+1:itot-1,in), line=in
    xpos=t(itot)+0.05*t(itot)
    ypos=0.5*amax
    xyouts,xpos,ypos,fall,font=3

    !P.POSITION=[0.15,0.2,0.7,0.4]
    amax=max(vy)
    amin=min(vy)
    plot, t(0:itot-1), vy(0:itot-1,0), line=0,$
        title='Y Velocity of X lines',$
        xtitle='time', font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(istart(in)+1:itot-1), $
      vy(istart(in)+1:itot-1,in), line=in
    oplot, t(istart(in)+1:itot-1), vyave(istart(in)+1:itot-1), $
        line=in+1

  endif


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile


end




