; START OF MAIN PROGRAM

  ntmax=201 & nx=long(303) & ntf=7 & nxf=11

  name='' & contin='' & again='y' & withps='n' & run=''
  time=0.0 & xtit='x' & change ='n'
  
  t=fltarr(ntmax) & ioxf=fltarr(nxf)

  while again eq 'y' do begin

  openr, 8, 'sim1.bin',/F77_UNFORMATTED
  readu, 8, nx
  x=fltarr(nx,/NOZERO) & f=fltarr(nx,/NOZERO)
  farr=fltarr(nx,ntmax,/NOZERO)
  readu, 8, x
  itime = 0
  while not eof(8)  do begin
    readu, 8, time, f
    farr(*,itime) = f(*) &  t(itime) = time
    itot=itime
    if itime lt ntmax then itime = itime + 1 else stop, ' to many records '
  endwhile
  close, 8

;----PARAMETER-------
  tmin = t(0)    &  xmin = x(0)
  tmax = t(itot) &  xmax = x(nx-1)
  print, 'tmin=',tmin, '   tmax=',tmax 
  print, 'xmin=',xmin, '   xmax=',xmax 
; CHANGE PARAMETERS:
  tmin = t(0)    &  xmin = x(0)
  tmax = t(itot-1) &  xmax = x(nx-1)
  
; generation of new grid 
  xf=findgen(nxf) & dxf=(xmax-xmin)/float(nxf-1)
  xf=xf*dxf+xmin
 
  in=-1 & k=0
  repeat begin
    in=in+1
    while xf(in) gt x(k+1) do k=k+1
    ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k))        
  endrep until in eq nxf-1

  print, 'Which case?'
  read, run
  fall=run
;  fall='run:'+run


    print, 'With postscript?'
    read, withps
    if withps eq 'y' then begin 
        !P.THICK=2.
     	set_plot,'ps'
        device,filename='sat.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
      endif
    xp1=0.2 & xp2=0.7 
    mpl=float(indgen(5)) & yp1=0.83-0.19*mpl & yp2=0.95-0.19*mpl 
    !P.REGION=[0.,0.,1.0,1.0]
    !P.MULTI=[0,0,2*itot]

    print, 'plot page?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin
      !P.CHARSIZE=2.0  
      !P.FONT=3
      !X.THICK=2
      !Y.THICK=2
       !X.TICKS=4
      !Y.TICKS=4
;      !Y.TICKlen=0.04
      amax = max(farr(*,0:itot)) &  amin = min(farr(*,0:itot))
      print, 'amin=',amin,'  amax=',amax
      !X.RANGE=[xmin,xmax]
      !Y.RANGE=[amin,amax]
      
      print, 'plotmin=',amin,'  plotmax=',amax, $
             '   Change? Enter y and desired values!'
      read, change
      if change eq 'y' then read, amin, amax
  
       !P.THICK=1.      
       !P.POSITION=[0.2,0.55,0.8,0.9] 
       plot, x, farr(*,0),$
;          title='Temperature',/noerase,$
          title='Temperature',$
          xtitle='x', xstyle=1, ystyle=1, line=0
      for i=1,itot-1 do  oplot, x, farr(*,i), line=3
      !P.THICK=2.      
      oplot, x, farr(*,i), line=0
        xpos=1.02*x(nx-1)
        ypos=amin+0.6*(amax-amin)
	xyouts,xpos,ypos,fall
    endif

    print, 'view results again or make ps file?'
    read, again
;    if withps eq 'y' then begin
;       device,/close & !P.THICK=1. & set_plot,'x'
;    endif
;  endwhile

  openr, 8, 'sim2.bin',/F77_UNFORMATTED
  readu, 8, nx
  x=fltarr(nx,/NOZERO) & f=fltarr(nx,/NOZERO)
  farr=fltarr(nx,ntmax,/NOZERO)
  readu, 8, x
  itime = 0
  while not eof(8)  do begin
    readu, 8, time, f
    farr(*,itime) = f(*) &  t(itime) = time
    itot=itime
    if itime lt ntmax then itime = itime + 1 else stop, ' to many records '
  endwhile
  close, 8

;----PARAMETER-------
  tmin = t(0)    &  xmin = x(0)
  tmax = t(itot) &  xmax = x(nx-1)
  print, 'tmin=',tmin, '   tmax=',tmax 
  print, 'xmin=',xmin, '   xmax=',xmax 
; CHANGE PARAMETERS:
  tmin = t(0)    &  xmin = x(0)
  tmax = t(itot-1) &  xmax = x(nx-1)
  
; generation of new grid 
  xf=findgen(nxf) & dxf=(xmax-xmin)/float(nxf-1)
  xf=xf*dxf+xmin
 
  in=-1 & k=0
  repeat begin
    in=in+1
    while xf(in) gt x(k+1) do k=k+1
    ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k))        
  endrep until in eq nxf-1

  print, 'Which case?'
  read, run
  fall=run
;  fall='run:'+run



;  while again eq 'y' do begin

;    print, 'With postscript?'
;    read, withps
;    if withps eq 'y' then begin 
;        !P.THICK=2.
;     	set_plot,'ps'
;        device,filename='sat.ps'
;        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
;        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
;      endif
    xp1=0.2 & xp2=0.7 
    mpl=float(indgen(5)) & yp1=0.83-0.19*mpl & yp2=0.95-0.19*mpl 
    !P.REGION=[0.,0.,1.0,1.0]
    !P.MULTI=[0,0,2*itot]

    print, 'plot page?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin
      !P.CHARSIZE=2.0  
      !P.FONT=3
      !X.THICK=2
      !Y.THICK=2
       !X.TICKS=4
      !Y.TICKS=4
;      !Y.TICKlen=0.04
      amax = max(farr(*,0:itot)) &  amin = min(farr(*,0:itot))
      print, 'amin=',amin,'  amax=',amax
      !X.RANGE=[xmin,xmax]
      !Y.RANGE=[amin,amax]
      
      print, 'plotmin=',amin,'  plotmax=',amax, $
             '   Change? Enter y and desired values!'
      read, change
      if change eq 'y' then read, amin, amax
  
       !P.THICK=1.      
       !P.POSITION=[0.2,0.1,0.8,0.45] 
       plot, x, farr(*,0),$
          title='Temperature',/noerase,$
;          title='Temperature',$
          xtitle='x', xstyle=1, ystyle=1, line=0
        xpos=1.02*x(nx-1)
        ypos=amin+0.6*(amax-amin)
	xyouts,xpos,ypos,fall
      for i=1,itot-1 do  oplot, x, farr(*,i), line=3
      !P.THICK=2.      
      oplot, x, farr(*,i), line=0
    endif

    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then begin
       device,/close & !P.THICK=1. & set_plot,'x'
    endif
  endwhile
  
  
  
end

