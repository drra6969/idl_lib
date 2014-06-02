; START OF MAIN PROGRAM

  COMMON VOLUME_DATA, ff
  nx=long(131) & ny=long(126) & nz=long(126) 
  time=0.0 & fnumber=1 & next='y'
  nxf=41 & nyf=41 & nzf=41     ;grid for uniform interpol fields
  ioxf=fltarr(nxf) & ioyf=fltarr(nxf) & iozf=fltarr(nxf)
  
  xmin = -10.0 & ymin = -10.0 & zmin = -10.0
  xmax =  10.0 & ymax =  10.0 & zmax =  10.0

  print, 'Input filenumber'
  read, fnumber
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
  xf=findgen(nxf) & yf=findgen(nxf) & zf=findgen(nxf) 
  dxf=(xmax-xmin)/float(nxf-1) &  xf=xf*dxf+xmin
  dyf=(ymax-ymin)/float(nxf-1) &  yf=yf*dyf+ymin
  dzf=(zmax-zmin)/float(nxf-1) &  zf=zf*dzf+zmin 
  in=-1 &  k=0
  repeat begin &    in=in+1
    while xf(in) gt x(k+1) do k=k+1
    ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
  endrep until in eq nxf-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while yf(in) gt y(k+1) do k=k+1
    ioyf(in) = float(k) + (yf(in)-y(k))/(y(k+1)-y(k))        
  endrep until in eq nxf-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while zf(in) gt z(k+1) do k=k+1
    iozf(in) = float(k) + (zf(in)-z(k))/(z(k+1)-z(k)) 
  endrep until in eq nxf-1
  
  f1=interpolate(rho,ioxf,ioxf,iozf,/grid)
;  SLOWN
  read, next
  if next eq 'n' then stop
 
set_plot,'x'
;xloadct
;device,filename='p3.ps'
;sphere=fltarr(20,20,20)
;for x=0,19 do  for y=0,19 do $
;for z=0,19 do sphere(x,y,z)= $
;sqrt((x-10)^2+(y-10)^2+(z-10)^2)
anft=0.35
endt=0.4

  box_1x=[0,nxf,0,nxf,0,nxf,0,nxf]
  box_1y=[0,0,nyf,nyf,0,0,nyf,nyf]
  box_1z=[0,0,0,0,nzf,nzf,nzf,nzf]
  box_2x=[0,0,0,0,nxf,nxf,nxf,nxf]
  box_2y=[0,nyf,0,nyf,0,nyf,0,nyf]
  box_2z=[0,0,nzf,nzf,0,0,nzf,nzf]
  box_3x=[0,0,nxf,nxf,0,0,nxf,nxf]
  box_3y=[0,0,0,0,nyf,nyf,nyf,nyf]
  box_3z=[0,nzf,0,nzf,0,nzf,0,nzf]
  print, size(f1)
  shade_volume,f1,0.35,v,p,LOW=1
  array=polyshade(v,p,/t3d)
set_plot,'PS'
  device,filename='p.ps'
  tvscl, array
  device,/close
set_plot,'x'
  t3d,/reset
  scale3,xrange=[0,nxf],yrange=[0,nxf],zrange=[0,nxf] 
;  t3d,rotate=[-90,180+yang,0]
;  t3d,rotate=[xang,0,0],perspective=persp

  h=[0.5,0.5,0.5]
  thresh=0.4

   nframes=5
   nf1=5
;   xanimate, set=[640,512,nframes]
for ip=0,nf1-1 do begin
;  ip=0
  t3d,/reset
  scale3,xrange=[0,nxf],yrange=[0,nxf],zrange=[0,nxf] 
  t3d,trans=-h,rotate=[0,0,15*ip]
  t3d,perspective=(2.45-0.06*ip)
  t3d,trans=h
  print, size(f1)
  print, size(v)
  print, size(p)
  set_plot,'z'
  erase
  shade_volume,f1,thresh,v,p,LOW=1
  a =polyshade(v,p,/t3d,/data)
  image = tvrd()
;  set_plot,'x'
  shade_volume,f1,thresh,v1,p1,LOW=0
;  set_plot,'z'
  b = polyshade(v1,p1,/T3D)
  image = tvrd()
  for i=0,3 do plots, box_1x((2*i):(2*i+1)), $
                      box_1y((2*i):(2*i+1)), $
                      box_1z((2*i):(2*i+1)),/t3d,/data   
  for i=0,3 do plots, box_2x((2*i):(2*i+1)), $
                      box_2y((2*i):(2*i+1)), $
                      box_2z((2*i):(2*i+1)),/t3d,/data   
  for i=0,3 do plots, box_3x((2*i):(2*i+1)), $
                      box_3y((2*i):(2*i+1)), $
                      box_3z((2*i):(2*i+1)),/t3d,/data   
  image = tvrd()
  set_plot,'x'
  tvscl, image
  xanimate, frame=ip, window=!d.window
endfor
  read, next
  if next eq 'n' then stop
for ip=nf1,nframes-1 do begin
  t3d,/reset
  scale3,xrange=[0,nxf],yrange=[0,nxf],zrange=[0,nxf] 
  t3d,trans=-h,rotate=[0,0,3*nf1]
  t3d,rotate=[0,3*(ip+1-nf1),0]
  t3d,perspective=(2.45-0.06*nf1+0.02*(ip+1-nf1))
  t3d,trans=h
  print, size(f1)
  print, size(v)
  print, size(p)
  set_plot,'z'
  erase
  shade_volume,f1,thresh,v,p,LOW=1
  a =polyshade(v,p,/t3d)
  image = tvrd()
  set_plot,'x'
;  tvscl, image
;  z_last = tvrd()			;Save old display
;  zb_last = tvrd(CHANNEL=1, /WORDS)
  shade_volume,f1,thresh,v1,p1,LOW=0
  set_plot,'z'
  b = polyshade(v1,p1,/T3D)
  image = tvrd()
  set_plot,'x'
  tvscl, image
   xanimate, frame=ip, window=!d.window
endfor
xanimate
set_plot,'PS'
  device,bits_per_pixel=8  
  device,filename='p.ps'
  image(where(image eq 0B)) = 255B
  tvscl, image
  device,/close
set_plot,'x'
  read, next
  if next eq 'n' then stop
;  v2=[v,v1] & p2=[p,p1]
;  tvscl, polyshade(v2,p2,/t3d)
;endfor
       ; Create some strings.
         strings = STRARR(4)
         strings(0) = '0000000000000'
         strings(1) = '`F1,C200`'
         strings(2) = 'ABC`X200,V-100`DEF`F0`GHIJKL`C155`abc'
         strings(3) = '`C255,F1`ABCDEF`F2`GHIJKL'

       ; Specify the fonts.
         fonts = STRARR(3)
         fonts(0) = '*helvetica-bold*--24*|20!5'
         fonts(1) = 'vector|15!6'
         fonts(2) = '8x13|11!3'

       ; Create a window and display the text.
         Window, 0
         DISP_TXT, strings, fonts, 28, Def_Font='12x24'


       ; Create some data.
          vol = RANDOMU(s, 40, 40, 40)
          FOR i=0, 10 DO vol = SMOOTH(vol, 3)
          vol = BYTSCL(vol(3:37, 3:37, 3:37))

       ; Extract and display a slice.
          slice = EXTRACT_SLICE(vol, 40, 40, 17, 17, 17, 30.0, 30.0, 0.0, $
                                OUT_VAL=0B)
          TVSCL, REBIN(slice, 400, 400)
          
;CREATE_VIEW
       ; Create some data.
          vol = FLTARR(40, 50, 30)
          vol(3:36, 3:46, 3:26) = RANDOMU(s, 34, 44, 24)
          FOR i=0, 10 DO vol = SMOOTH(vol, 3)

       ; Generate the iso-surface.
          SHADE_VOLUME, vol, 0.2, polygon_list, vertex_list, /LOW

       ; Set up the view.
       ; Note that the subscripts into the Vol array range from
       ; 0 to 39 in X, 0 to 49 in Y, and 0 to 29 in Z.   As such,
       ; the 3-D coordinates of the iso-surface (vertex_list) may
       ; range from 0.0 to 39.0 in X, 0.0 to 49.0 in Y,
       ; and 0.0 to 29.0 in Z.   Set XMIN, YMIN, and ZMIN to
       ; zero (the default), and set XMAX=39, YMAX=49, and ZMAX=29.
          WINDOW, XSIZE=600, YSIZE=400
          CREATE_VIEW, XMAX=39, YMAX=49, ZMAX=29, AX=(-60.0), AZ=(30.0), $
                       WINX=600, WINY=400, ZOOM=(0.7), PERSP=(1.0)

       ; Display the iso surface in the specified view.
          img = POLYSHADE(polygon_list, vertex_list, /DATA, /T3D)
          TVSCL, img
          
          
       R = PROFILE(A)
       PLOT, R
   
;	.run PDE_ADI  (compiles the procedure)
;	 PDE_ADI     (runs the procedure)
EXAMPLE:
;	This procedure solves the partial differential equation:
;       Ut-diff*(Uxx+Uyy)=NONHOMOGENEOUS(X,Y,T)
;       Subject to the initial and boundary conditions:
;       U(x,y,0)=INITIAL_COND(X,Y)
;       U(0,y,t)=BOUNDARY_LT(Y,T)   U(1,y,t)=BOUNDARY_RT(Y,T)
;       U(x,0,t)=BOUNDARY_BOT(X,T)  U(x,1,t)=BOUNDARY_TOP(X,T)
;       where:
;             Ut ....   first partial derivative of U with respect to t
;             Uxx ...  second partial derivative of U with respect to x
;	      Uyy ...  second partial derivative of U with respect to y

end


