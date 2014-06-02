; START OF MAIN PROGRAM

  ntmax=201 & nx=long(303) & ntf=21 & nxf=21

  name='' & contin='' & again='y' & withps='n' & run=''
  time=0.0 & xtit='x'
  
  t=fltarr(ntmax,/NOZERO)
  iotf=fltarr(ntf) & ioxf=fltarr(nxf)
  fa=fltarr(nxf,ntf) 

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
  tf=findgen(ntf) & dtf=(tmax-tmin)/float(ntf-1)
  tf=tf*dtf+tmin
  xf=findgen(nxf) & dxf=(xmax-xmin)/float(nxf-1)
  xf=xf*dxf+xmin
 
  in=-1 & k=0
  repeat begin
    in=in+1
    while tf(in) gt t(k+1) do k=k+1
    iotf(in) = float(k) + (tf(in)-t(k))/(t(k+1)-t(k)) 
  endrep until in eq ntf-1
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

  while (again eq 'y') do begin

    print, 'With postscript?'
    read, withps
    if withps eq 'y' then begin 
       !P.CHARTHICK=3
        set_plot,'ps'
        device,filename='cur.ps'
        device,/landscape
;        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
;        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
    endif

    dx=0.55 & dy=0.9 & dz=0.6
    if withps eq 'y' then begin dx=0.3 & dy=0.6 & dz=0.4 & endif
    x1=0.2 & x2=x1+dx
    y1=0.1 & y2=y1+dy
    z1=0.1 & z2=z1+dz
    ang1=55 & ang2=10
    
    print, 'plot temperature?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin

; plot  page
;       !P.REGION=[0.,0.,1.0,1.25]
;       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=-1
       !P.THICK=1
       !X.TICKS=5
       !Y.TICKS=3
       !X.THICK=2
       !Y.THICK=2
       !Z.THICK=2
       !Z.MINOR=-1
      erase
      fa=interpolate(farr,ioxf,iotf,/grid)
;      fa=smooth(fa,3)
      surface,fa,xf,tf,position=[x1, y1, x2, y2, z1, z2],$
        ax=ang1, az=ang2,$
        ztitle='x pos',xstyle=1,ystyle=1,zstyle=1,$
        xtitle=xtit,ytitle='time'

    endif

    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then device,/close
    set_plot,'x'
    !P.CHARTHICK=1
    
  endwhile
 
end 

