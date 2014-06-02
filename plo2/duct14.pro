; PROGRAM duct
;   program to plot duct flow pattern for various discretization methods 
   name='' & contin='' & again='y' & withps='n' & run=''

; PARAMETER & ARRAYS
   nx = 21   &   ny = 21 &   nt = 21  & nx2=nx-2 & ny2=ny-2
   fex=fltarr(nx,ny) & fg1=fex & fg2=fex 
   fs1=fex & fs2=fex & fc1=fex & fc2=fex 
   xmin=-1.0 & xmax=1.0 & ymin=-1.0 & ymax=1.0
   dx = (xmax-xmin)/(nx-1) & dy = (ymax-ymin)/(ny-1)
   pi=3.141593  & cex=(8.0/pi^2)^2

; GRID 
   x=findgen(nx) &  y=findgen(ny)
   x=xmin+dx*x   &  y=ymin+dy*y
   ix0=(nx-1)/2  &  iy0=(ny-1)/2 
; Exact solution
   fex=0.0*fex
   for k=0, nx-1 do $
      for i=1, nt, 2 do $
      for j=1, nt, 2 do $
       fex(k,*) = fex(k,*) $
                 + (-1)^( (i+j)/2-1 )/float( i*i*i*j+j*j )$
                    *cos(0.5*pi*i*x(k))*cos(0.5*pi*j*y(*))
       fex=cex*fex
       print, 'max.  F_exact: ', fex(ix0,iy0)
   
   
; Galerkin
   ag1=0.5*5./8.   & ag21=0.5*37./52.   & ag22=-0.5*105./64./13.
   print, 'galerkin', ag1, ag21, ag22
   for i=0, nx-1 do  fg1(i,*) = ag1*( 1.0-x(i)*x(i) )*( 1-y(*)*y(*) )
   for i=0, nx-1 do  $
     fg2(i,*) = ag21*( 1.0-x(i)*x(i) )*( 1-y(*)*y(*) )$
                + ag22*( 1.0-x(i)*x(i) )^2*( 1-y(*)*y(*) )^2
   
    sumg1=0. & sumg2=0.
    for j=2,nx-1 do $
    for k=2,ny-1 do begin
      sumg1 = sumg1 + ( fex(j,k)-fg1(j,k) )^2
      sumg2 = sumg2 + ( fex(j,k)-fg2(j,k) )^2
    endfor
    rmsg1 = sqrt(sumg1/nx2/nx2)
    rmsg2 = sqrt(sumg2/nx2/nx2)
    print, 'rms: ', rmsg1,rmsg2
    print, 'max: ', fg1(ix0,iy0),fg2(ix0,iy0)
    
; Subdomain
   as1=0.5*3./4.   & as21= 0.5*3./4.  & as22=-0.5*15./86.
   print, 'subdomain', as1, as21, as22
   for i=0, nx-1 do  fs1(i,*) = as1*( 1.0-x(i)*x(i) )*( 1-y(*)*y(*) )
   for i=0, nx-1 do  $
     fs2(i,*) = as21*( 1.0-x(i)*x(i) )*( 1-y(*)*y(*) )$
                + as22*( 1.0-x(i)*x(i) )^2*( 1-y(*)*y(*) )^2

    sums1=0. & sums2=0.
    for j=2,nx-1 do $
    for k=2,ny-1 do begin
      sums1 = sums1 + ( fex(j,k)-fs1(j,k) )^2
      sums2 = sums2 + ( fex(j,k)-fs2(j,k) )^2
    endfor
    rmss1 = sqrt(sums1/nx2/nx2)
    rmss2 = sqrt(sums2/nx2/nx2)
    print, 'rms: ', rmss1,rmss2
    print, 'max: ', fs1(ix0,iy0),fs2(ix0,iy0)
   
; Collocation
   ac1=0.5*0.5   & ac21=0.5*0.9   & ac22=-0.5*0.2
   print, 'collocation', ac1, ac21, ac22
   for i=0, nx-1 do  fc1(i,*) = ac1*( 1.0-x(i)*x(i) )*( 1-y(*)*y(*) )
   for i=0, nx-1 do  $
     fc2(i,*) = ac21*( 1.0-x(i)*x(i) )*( 1-y(*)*y(*) )$
                + ac22*( 1.0-x(i)*x(i) )^2*( 1-y(*)*y(*) )^2

    sumc1=0. & sumc2=0.
    for j=2,nx-1 do $
    for k=2,ny-1 do begin
      sumc1 = sumc1 + ( fex(j,k)-fc1(j,k) )^2
      sumc2 = sumc2 + ( fex(j,k)-fc2(j,k) )^2
    endfor
    rmsc1 = sqrt(sumc1/nx2/nx2)
    rmsc2 = sqrt(sumc2/nx2/nx2)
    print, 'rms: ', rmsc1,rmsc2
    print, 'max: ', fc1(ix0,iy0),fc2(ix0,iy0)
   


    print, 'Which case?'
    read, run

     while (again eq 'y') do begin

      !P.THICK=1.
      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
;        device,/landscape
        !P.THICK=2.
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
       endif

      xpos=xmax+0.03*dx
      ypos=ymin+0.2*dy         
      dpx =0.18 & dpy=0.8
      xleft=0.06 & xinter=0.12 & hop=0.47*xinter
      xa1=xleft & xe1=xleft+dpx  
      xa2=xe1+xinter & xe2=xa2+dpx
      xa3=xe2+xinter & xe3=xa3+dpx
;      ylo=0.06 & yup=ylo+4.*dpy
      ylo=0.06 & yup=ylo+dpy

       
  print, 'plot first page? Magnetic field, flow vectors'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot first page
;       !P.REGION=[0.,0.,1.0,1.25]
       !P.REGION=[0.,0.,1.0,1.0]
       !P.CHARSIZE=2.0
       !X.TICKS=4
       !Y.TICKS=4
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
       !X.RANGE=[xmin,xmax]
       !Y.RANGE=[ymin,ymax]

        !P.MULTI=[0,1,0,0,0]
        !P.FONT=-1
        

    surface,fex,x,y,position=[0.2,0.55,0.8,0.95],ax=35,$
      ztitle='F_exact',xstyle=1,ystyle=1,zstyle=1,xtitle='x',ytitle='y'

  endif

  print, 'continue with next plot, galerkin'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

        !P.MULTI=[0,1,0,0,0]
    surface,fg1,x,y,position=[0.2,0.55,0.8,0.95],ax=35,$
      ztitle='F_gal1',xstyle=1,ystyle=1,zstyle=1,xtitle='x',ytitle='y'
    surface,fg2,x,y,position=[0.2,0.1,0.8,0.5],ax=35,$
      ztitle='F_gal2',xstyle=1,ystyle=1,zstyle=1,xtitle='x',ytitle='y',/noerase

  endif

  print, 'continue with next plot, subdomain'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

      surface,fs1,x,y,position=[0.2,0.55,0.8,0.95],ax=35,$
      ztitle='F_subd1',xstyle=1,ystyle=1,zstyle=1,xtitle='x',ytitle='y'
      surface,fs2,x,y,position=[0.2,0.1,0.8,0.5],ax=35,$
      ztitle='F_subd2',xstyle=1,ystyle=1,zstyle=1,xtitle='x',ytitle='y',/noerase

  endif

  print, 'continue with next plot, collocation'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

      surface,fc1,x,y,position=[0.2,0.55,0.8,0.95],ax=35,$
      ztitle='F_coll1',xstyle=1,ystyle=1,zstyle=1,xtitle='x',ytitle='y'
      surface,fc2,x,y,position=[0.2,0.1,0.8,0.5],ax=35,$
      ztitle='F_coll2',xstyle=1,ystyle=1,zstyle=1,xtitle='x',ytitle='y',/noerase

  endif
     !P.FONT=3


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end

