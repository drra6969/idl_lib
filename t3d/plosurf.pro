; START OF MAIN PROGRAM

  nx=long(131) & ny=long(126) & nz=long(126) 
  time=0.0 & fnumber=1
  nframes=9
  name='' & contin='' & again='y' & withps='n' & run=''  
  closeps='' & answer='n'
  nxf=41 & nyf=41 & nzf=41     ;grid for uniform interpol fields
  ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
    
;-----------PARAMETER----------------
  xmin = -10.0 & ymin = -10.0 & zmin = -10.0
  xmax =  10.0 & ymax =  10.0 & zmax =  10.0

  print, 'Input filenumber'
  read, fnumber
  print, 'Which case?'
  read, run
  name='magtap'+string(fnumber,'(i1)')
  openr, 8, name,/F77_UNFORMATTED
  readu, 8, nx, ny, nz, time
   print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz
   print, 'time=',time
  
  rho=fltarr(nx,ny,nz,/NOZERO) 
  x=fltarr(nx,/NOZERO) & dx=0.1 & dxh=0.1
  y=fltarr(ny,/NOZERO) & dy=0.1 & dyh=0.1
  z=fltarr(nz,/NOZERO) & dz=0.1 & dzh=0.1 
  
  readu, 8,  x,dx,dxh,dxh,y,dy,dyh,dyh,z,dz,dzh,dzh
; print, x,y,z
  readu, 8, rho
  print, 'xmin=', x(1), '  xmax=', x(nx-2)
  print, 'ymin=', y(1), '  ymax=', y(ny-2)
  print, 'zmin=', z(1), '  zmax=', z(nz-2)
  close, 8

  f1=interpolate(rho,ioxf,ioxf,iozf,/grid)
  amin=min(rho) & amax=max(rho) &  thresh=amin+0.5*(amax-amin)
  print,'rho:',amin,amax,thresh

; generation of grid for uniform interpol fields
  xf=findgen(nxf) & yf=findgen(nyf) & zf=findgen(nzf) 
  dxf=(xmax-xmin)/float(nxf-1) &  xf=xf*dxf+xmin
  dyf=(ymax-ymin)/float(nyf-1) &  yf=yf*dyf+ymin
  dzf=(zmax-zmin)/float(nzf-1) &  zf=zf*dzf+zmin 
  in=-1 &  k=0
  repeat begin &    in=in+1
    while xf(in) gt x(k+1) do k=k+1
    ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
  endrep until in eq nxf-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while yf(in) gt y(k+1) do k=k+1
    ioyf(in) = float(k) + (yf(in)-y(k))/(y(k+1)-y(k))        
  endrep until in eq nyf-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while zf(in) gt z(k+1) do k=k+1
    iozf(in) = float(k) + (zf(in)-z(k))/(z(k+1)-z(k))        
  endrep until in eq nzf-1


  for ijk=0,nframes-1 do begin
       xang=20
       yang=10+ijk*20
       zang=0
       persp=6
back:  boxy,xmin,xmax,ymin,ymax,zmin,zmax,xang,yang,zang,persp
       scale3,xrange=[0,nxf],yrange=[0,nyf],zrange=[0,nzf]
       shade_volume, f1, 0.4, vert, polys
       tv, polyshade(vert,polys,/t3d)

       if answer eq 'y' then device, /close
       print,'Do you want to create a PS plot?'
       read,answer
       if (answer eq 'y') then begin
         print,'Please give filename for PS file'
         read,filenm
         set_plot,'PS'
         device,filename=filenm
         device,/inches,xsize=8.,scale_factor=0.8,xoffset=0.5
         device,/inches,ysize=8.,scale_factor=0.8,yoffset=0.5
         goto,back 
       endif else begin
         set_plot,'x'
       endelse
;        xanimate, frame=ijk, window=!d.window
  endfor
  print,'Do you want to repeat plots?'
  read,answer
  if (answer eq 'y')  then goto,back 

end


