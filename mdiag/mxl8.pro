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

    yavsm=smooth(yave,3)
    vyavsm=smooth(vyave,3)

    !P.POSITION=[0.3,0.33,0.85,0.48]
    amax=max(10)
    amin=min(yl)
    plot, t(0:itot), yl(0:itot,0), line=0,$
        title='Y coord. of X lines',$
        font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(istart(in):itot), $
      yl(istart(in):itot,in), line=in
    oplot, t(istart(in):itot-1), yavsm(istart(in):itot-1), line=0,thick=2

    !P.POSITION=[0.3,0.1,0.85,0.25]
    amax=max(0.3)
    amin=min(0.)
;    plot, t(0:itot-1), vy(0:itot-1,0), line=0,$
    plot, t(istart(in)+1:itot-2), vyavsm(istart(in)+1:itot-2) , line=0,$
        title='Y Velocity of X lines',$
        xtitle='time', font=3, yrange=[amin,amax],thick=2
;    for in=0, nomax-1 do oplot, t(istart(in)+1:itot-1), $
;      vy(istart(in)+1:itot-1,in), line=in
    oplot, [0,100], [0.12,0.12], $
        line=1,thick=1

  endif






     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile


end




