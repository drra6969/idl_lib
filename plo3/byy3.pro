; START OF MAIN PROGRAM
;
; program needs update for vect, and laser 600 resolution
  nxn=21 & nyn=21 & nzn=21     ;grid for arrow presentation
  nxf=121 & nyf=121 & nzf=121     ;grid for uniform interpol fields
  nx=long(131) & ny=long(126) & nz=long(126) 
  iox=fltarr(nxn) & ioy=fltarr(nyn) & ioz=fltarr(nzn)
  ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
  time=0.0 & fnumber=1
  fgroup='1' & group='1'
  name='' & wsmooth='' & again='y' & withps='n' & run=''  
  closeps='n' & jnew='y' & newplot='a' & coltab='o'
  plane='x' & whatcut='x' & choice='f' & igrid=21
  jnew= 'y' & enew= 'y' & plform='p' 
  names=strarr(15)
  names=replicate(' ',15)
   ctab=findgen(nxf)
   cbary=fltarr(2,nxf)

;-----------PARAMETER----------------
  xmin = -8. & ymin = -50  & zmin = 0
  xmax =  8. & ymax = 50   & zmax = 120
  
  print, 'Input filenumber'
  read, fnumber
  print, 'Which case?'
  read, run
  name='magtap0'+string(fnumber,'(i1)')
;  name='magbin0'+string(fnumber,'(i1)')
  openr, 8, name,/F77_UNFORMATTED
  readu, 8, nx, ny, nz, time
    print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz
    print, 'time=',time
    x=fltarr(nx,/NOZERO) & dx=x & dxh=x
    y=fltarr(ny,/NOZERO) & dy=y & dyh=y
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z 
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
    readu, 8,  x,dx,dxh,dxh,dxh,dxh,dxh,y,dy,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
    print, 'xmin=', x(1), '  xmax=', x(nx-2)
    print, 'ymin=', y(1), '  ymax=', y(ny-2)
    print, 'zmin=', z(1), '  zmax=', z(nz-2)
    if xmin lt x(1) then xmin = x(1)
    if xmax gt x(nx-2) then xmax = x(nx-2)
    if ymin lt y(1) then ymin = y(1)
    if ymax gt y(ny-2) then ymax = y(ny-2)
    if zmin lt z(1) then zmin = z(1)
    if zmax gt z(nz-2) then zmax = z(nz-2)
   for iz=1,nz-2 do print,'z(',iz,')=',z(iz)
    
  readu, 8,  bx,by,bz
  close, 8

ix1=11 & ix2=90  & nnx=ix2-ix1+1

     while again eq 'y' do begin
;     print, 'Input iy'
;     read, icoo
;     if icoo ne '' then iyy=fix(icoo)
;     print, 'iy:', iyy, '   y(iyy):', y(iyy)
     

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='flux.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif
; first page
  !P.REGION=[0.,0.,1.0,1.25]
    !P.CHARSIZE=1.0  
    !P.MULTI=[0,2,2]
    iy0=31
    mz=2 & mzz=5

    !P.POSITION=[0.08,0.6,0.45,0.9]
    plot, by(*,3,3), bz(*,3,3), line=0,$
        title='Bz vs By',$
        font=3, xrange=[-1.,1.],yrange=[-1.,1.],$
;        font=3, xrange=[bymin,bymax],yrange=[bzmin,bzmax],$
        xtitle='By',ytitle='Bz'

    for iy=3,iy0,6 do $
    for iz=3,mz,6 do $
      oplot, by(*,iy,iz),bz(*,iy,iz) , line=0


    !P.POSITION=[0.55,0.6,0.92,0.9]
    plot, by(*,3,3), bz(*,3,3), line=0,$
        title='Bz vs By',$
        font=3, xrange=[-1.,1.],yrange=[-1.,1.],$
;        font=3, xrange=[bymin,bymax],yrange=[bzmin,bzmax],$
        xtitle='By',ytitle='Bz'

    for iy=iy0,ny-2,6 do $
    for iz=3,mz,6 do $
      oplot, by(*,iy,iz),bz(*,iy,iz) , line=0


    !P.POSITION=[0.08,0.2,0.45,0.5]
    plot, by(*,3,mz), bz(*,3,mz), line=0,$
        title='Bz vs By',$
        font=3, xrange=[-1.,1.],yrange=[-1.,1.],$
;        font=3, xrange=[bymin,bymax],yrange=[bzmin,bzmax],$
        xtitle='By',ytitle='Bz'

    for iy=3,iy0,6 do $
    for iz=mz,mzz, 6 do $
      oplot, by(*,iy,iz),bz(*,iy,iz) , line=0

    !P.POSITION=[0.55,0.2,0.92,0.5]
    plot, by(*,3,mz), bz(*,3,mz), line=0,$
        title='Bz vs By',$
        font=3, xrange=[-1.,1.],yrange=[-1.,1.],$
;        font=3, xrange=[bymin,bymax],yrange=[bzmin,bzmax],$
        xtitle='By',ytitle='Bz'

    for iy=iy0,ny-2,6 do $
    for iz=mz,mzz, 6 do $
      oplot, by(*,iy,iz),bz(*,iy,iz) , line=0



     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'

   endwhile






end


