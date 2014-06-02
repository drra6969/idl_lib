; MAIN PROGRAM
;   program reads data from 2D simulations(x/y) 
;   and substitutes the simulation y direction with 
;   z for plotting data from MP simulations
;      PLOT        SIMULATION
;     -------       -------
;  z !       !    y!       !
;    !       !     !       !
;    !       !     !       !
;    !       !     !       !
;    !       !     !       !
;    <-------       ------->
;      x                  x

; GRID FOR VELOCITY VECTORS
   nxn = 15   &   nyn = 21
   iox=fltarr(nxn) & ioy=fltarr(nyn)
   fn1=fltarr(nxn,nyn) & fn2=fn1 & fn3=fn1 & fn4=fn1 
; GRID FOR CONTOUR AND SURFACE PLOTS
   nxf = 61   &   nyf = 61
   ioxf=fltarr(nxf) & ioyf=fltarr(nyf)
   fa=fltarr(nxf,nyf) & fb=fa
  names=strarr(15)
  names=replicate(' ',15)
  igrid=2 & newgrid='2'

;----PARAMETER-------
  xmin =  -10. & ymin = 0.0
  xmax =  10. & ymax = 12.0
;--------------------
   time=0.0 & fnumber=1
   name='' & contin='' & again='y' & withps='n' & run=''
   nx=long(303) & ny=long(303) 

; READ INPUT DATA OF DIMENSION NX, NY
   print, 'Input filenumber'
   read, fnumber
   name='magtap'+string(fnumber,'(i2.2)')
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,time
   print, 'dimension nx=',nx,'     ny=',ny

   x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
   g1=fltarr(nx,/NOZERO) & g2=g1 & g3=g1 & dx=g1
   h1=fltarr(ny,/NOZERO) & h2=h1 & h3=h1 & dy=h1
   bx=fltarr(nx,ny,/NOZERO) & by=bx & bz=bx & 
   vx=bx & vy=bx & vz=bx & vnx=bx & vny=bx & vnz=bx 
   rho=bx & u=bx & rhon=bx & un=bx & rhon1=bx & un1=bx 
   res=bx & nu12s=bx 
   p=bx & pn=bx & p1=bx & jx=bx & jy=bx & jz=bx 
   f1=bx & f2=bx & f3=bx & f4=bx 

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
;  print, x
;  print, y
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz
   readu, 8,  rho,u,res
   close, 8
   vx=vx/rho &  vz=vz/rho & vy=vy/rho  & p=2*u^(5.0/3.0) 
   
   name='magnap'+string(fnumber,'(i2.2)')
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
;  print, x
;  print, y
   readu, 8,  vnx,vny,vnz
   readu, 8,  rhon,un,rhon1,un1,nu12s
   close, 8
   vnx=vnx/rhon &  vnz=vnz/rhon & vny=vny/rhon
   pn=2*un^(5.0/3.0) & pn1=2*un1^(5.0/3.0)


      f1 = shift(bz,0,-1)-shift(bz,0,1)
      for j=1,ny-2 do f3(*,j)=dy(j)*f1(*,j)
      jx = f3
      f2 = shift(bz,-1,0)-shift(bz,1,0)
      for i=1,nx-2 do f4(i,*)=dx(i)*f2(i,*)
      jy = f3-f4
      jx([0,nx-1],*)=0.0 & jy([0,nx-1],*)=0.0 
      jx(*,[0,ny-1])=0.0 & jy(*,[0,ny-1])=0.0 
   
;   for iv=0, ny-1 do vy(*,iv)=vy(*,iv)/rho(*,iv)-1.5*tanh(x)
;          print, 'igrid: ',21,'   vy: ', vy(21,*)
  
  
   if (xmin lt x(1)) then begin
     print, 'warning! xmin:',xmin,' out of bounds:',x(1),' Reset!'
     xmin=x(1)
   endif  
   if (xmax gt x(nx-2)) then begin
     print, 'warning! xmax:',xmax,' out of bounds:',x(nx-2),' Reset!'
     xmax=x(nx-2)
   endif  
   if (ymin lt y(1)) then begin
     print, 'warning! ymin:',ymin,' out of bounds:',y(1),' Reset!'
     ymin=y(1)
   endif  
   if (ymax gt y(ny-2)) then begin
     print, 'warning! ymax:',ymax,' out of bounds:',y(ny-2),' Reset!'
     ymax=y(ny-2)
   endif  
   delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
  
    plane='z'  & nplane=ny & coord=y & xtit='x'   
    max=12. & min=0.
    if max gt x(nx-1) then max = x(nx-1)
    if min lt x(0)    then min = x(0)
    if max lt min then begin
      min = x(0)
      max = x(nx-1)
    endif
    
    print, 'Which case?'
    read, run

    
gridindex:
  print, plane, 'Coordinates:
  for i=0,nplane-1,2 do print, i,'  ',plane,'=',coord(i)
  print, 'Input - i = Grid Index of Chosen Plane(>0 and <',nplane-1,')'
  print, 'Input Options:   integer -> grid index'
  print, '                  return -> no changes applied'
  print, '                       m -> change min, max'
  print, '                       p -> postscript, cut***.ps'
  print, '                       q -> terminate'
  print, '                       x -> cut at x =const'
  print, '                       y -> cut at z =const'
  print, 'Present Choices: '
  print, '  Cut at constant ', plane,',  Grid index: ', igrid
  read, newgrid 
  if newgrid eq 'm' then begin
    print, '  Input min and max values of x axis of plot!'
    read, min
    read, max
  endif
  if newgrid eq 'q' then stop
  if newgrid eq 'p' then begin
        withps = 'y'
        set_plot,'ps'
        device,filename=$
              'cut'+plane+string(coord(igrid),'(i3.3)')+'t'$
              +string(time,'(i3.3)')+'.ps'
        !P.THICK=2.
        device,/portrait
        device,/inches,xsize=5.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=6.5,scale_factor=1.0,yoffset=0.5
  endif
  if newgrid eq 'x' then begin 
    plane='x' & nplane=nx & coord=x & xtit='z'
    if max gt y(ny-1) then max = y(ny-1)
    if min lt y(0)    then min = y(0)
    if max lt min then begin
      min = y(0)
      max = y(ny-1)
    endif
    goto, gridindex
  endif
  if newgrid eq 'y' then begin 
    plane='z' & nplane=ny & coord=y & xtit='x'
    if max gt x(nx-1) then max = x(nx-1)
    if min lt x(0)    then min = x(0)
    if max lt min then begin
      min = x(0)
      max = x(nx-1)
    endif
    goto, gridindex
  endif
  if (newgrid ne '') and (newgrid ne 'q') and (newgrid ne 'p') $
     and (newgrid ne 'x') and (newgrid ne 'x') and (newgrid ne 'm') $
     then igrid=fix(newgrid)
  if (newgrid eq '') or (newgrid eq 'p') or (newgrid eq 'm') then $
       print,'choice=',igrid,' not altered'
  if (igrid lt 0) or (igrid gt nplane-1) then igrid=(nplane+1)/2  

    del = max-min

    dpx=0.68  & dpy=0.2
    xa=0.07 & xe=xa+dpx 
    hopp=0.03     ;to seperate plots if desired
    ylo1=0.75 & yup1=ylo1+dpy
    ylo2=ylo1-dpy & yup2=ylo1
    ylo3=ylo2-dpy & yup3=ylo2
    
    
       
; plot first page
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=1.6
       !P.FONT=-1
       !X.TICKS=0
       !Y.TICKS=0
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
        !X.RANGE=[min,max]
;       !X.RANGE=[xmin,xmax]
;       !Y.RANGE=[ymin,ymax]

        if (plane eq 'x') then begin
          plcoo=y & f1=y & f2=y & f1a=y & f2a=y 
          f3=y & f4=y & f5=y & f6=y
          f7=y & f8=y & f9=y & f10=y & f11=y & f12=y & f13=y 
          f14=y & f15=y & f16=y & f17=y & f18=y & f19=y 
          f1=rhon(igrid,*) & f2=pn(igrid,*) & f3=pn(igrid,*)/rhon(igrid,*) 
          f1a=rhon(igrid,*)-rhon1(igrid,*) & f2a=pn(igrid,*)-pn1(igrid,*) 
          f4=vnx(igrid,*) & f5=vny(igrid,*) & f6=vnz(igrid,*)
          f7=res(igrid,*) & f8=nu12s(igrid,*) 
          f11=rho(igrid,*) & f12=p(igrid,*) & f13=p(igrid,*)/rho(igrid,*) 
          f14=vx(igrid,*) & f15=vy(igrid,*) & f16=vz(igrid,*)
          f17=bx(igrid,*) & f18=by(igrid,*) & f19=bz(igrid,*)
          coorval=plane+'='+string(x(igrid),'(f6.1)')
        endif
        if (plane eq 'z') then begin
          plcoo=x & f1=x & f2=x & f1a=x & f2a=x 
          f3=x & f4=x & f5=x & f6=x
          f7=x & f8=x & f9=x & f10=x & f11=x & f12=x & f13=x 
          f14=x & f15=x & f16=x & f17=x & f18=x & f19=x 
          f1=rhon(*,igrid) & f2=pn(*,igrid) & f3=pn(*,igrid)/rhon(*,igrid) 
          f1a=rhon(*,igrid)-rhon1(*,igrid) & f2a=pn(*,igrid)-pn1(*,igrid) 
          f4=vnx(*,igrid) & f5=vny(*,igrid) & f6=vnz(*,igrid)
          f7=res(*,igrid) & f8=nu12s(*,igrid) 
          f11=rho(*,igrid) & f12=p(*,igrid) & f13=p(*,igrid)/rho(*,igrid) 
          f14=vx(*,igrid) & f15=vy(*,igrid) & f16=vz(*,igrid)
          f17=bx(*,igrid) & f18=by(*,igrid) & f19=bz(*,igrid)
          coorval=plane+'='+string(y(igrid),'(f6.1)')
        endif
        
  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

        timeval = string(time,'(i4)')
        maintitle = 'Cut at '+coorval+', time ='+timeval

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f1,f2]) & print, 'rho,p max: ',bmax
        bmin=min([f1,f2]) & print, 'rho,p min: ',bmin
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0 & delb=bmax-bmin
        bmax=bmax+0.05*delb & delb=1.1*delb
        delb=bmax-bmin
	plot_io, plcoo, f1, title=maintitle, $
	yrange=[bmin,bmax],xstyle=1,ystyle=1,xtickname=names
	oplot, plcoo, f2, line=1
        xt1=max+0.02*del  &   xt2=max+0.04*del     
        yt1=bmin+0.89*delb    
        yt2=bmin+delb/24.  &   yt2a=bmin+delb/72.    
        yt3=bmin+delb/216. &   yt3a=bmin+delb/648.  
        xyouts, xt1, yt2,'Density',charsize=1.0
        xyouts, xt2, yt2a,'!7q!X!DN!N ___',charsize=1.0
        xyouts, xt1, yt3, 'Pressure',charsize=1.0
        xyouts, xt2, yt3a, 'p!DN!N _ _',charsize=1.0

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([f1a,f2a]) & print, 'del rho,p max: ',bmax
        bmin=min([f1a,f2a]) & print, 'del rho,p min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
	plot, plcoo, f1a, $
	yrange=[bmin,bmax],xstyle=1,ystyle=1,xtickname=names
	oplot, plcoo, f2a, line=1
        yt1=bmin+0.89*delb    
        yt2=bmin+0.63*delb  &   yt2a=bmin+0.51*delb    
        yt3=bmin+0.25*delb  &   yt3a=bmin+0.13*delb    
        xyouts, xt1, yt2,'Density Pert',charsize=1.0
        xyouts, xt2, yt2a,'!7q!X ___',charsize=1.0
        xyouts, xt1, yt3, 'Pressure Pert',charsize=1.0
        xyouts, xt2, yt3a, 'p _ _',charsize=1.0

	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([f3,f13]) & print, 'T max: ',bmax
        bmin=min([f3,f13]) & print, 'T min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
	plot, plcoo, f3,xtitle=xtit, $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f13, line=1
        yt1=bmin+0.89*delb    
        yt2=bmin+0.63*delb  &   yt2a=bmin+0.51*delb    
        yt3=bmin+0.25*delb  &   yt3a=bmin+0.13*delb    
        xyouts, xt1, yt2,'Neutr Temp',charsize=1.0
        xyouts, xt2, yt2a,'T!DN!N ___',charsize=1.0
        xyouts, xt1, yt3, 'Plasma Temp',charsize=1.0
        xyouts, xt2, yt3a, 'T _ _',charsize=1.0


  endif

  print, 'plot 2. page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin


	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f4,f5,f6]) & print, 'Vn max: ',bmax
        bmin=min([f4,f5,f6]) & print, 'Vn min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
	plot, plcoo, f4,title=' Vn Components', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f5, line=1
	oplot, plcoo, f6, line=2



  endif




        bmax=max([f7,f8,f9]) & print, 'B max: ',bmax
        bmin=min([f7,f8,f9]) & print, 'B min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0 & delb=bmax-bmin
        bmax=bmax+0.05*delb & bmin=bmin-0.05*delb & delb=1.1*delb
	plot, plcoo, f7, title=maintitle, $
	yrange=[bmin,bmax],xstyle=1,ystyle=1,xtickname=names
	oplot, plcoo, f8, line=1
	oplot, plcoo, f9, line=2
        xt1=max+0.02*del   &  yt1=bmin+0.73*delb    
        xt1a=max+0.04*del  &  yt1a=bmin+0.60*delb    
        xt2=max+0.04*del   &  yt2=bmin+0.45*delb    
        yt1=bmin+0.73*delb   
        yt1a=bmin+0.60*delb   
        yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Magn. Field',charsize=1.0
        xyouts, xt2, yt2,'B!Dx!N ___',charsize=1.0
        xyouts, xt2, yt3, 'B!Dy!N ......',charsize=1.0
        xyouts, xt2, yt4, 'B!Dz!N _ _',charsize=1.0

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([f4,f5,f6]) & print, 'V max: ',bmax
        bmin=min([f4,f5,f6]) & print, 'V min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0 &  delb=bmax-bmin
        bmax=bmax+0.05*delb & bmin=bmin-0.05*delb & delb=1.1*delb
	plot, plcoo, f4,$
	yrange=[bmin,bmax],xstyle=1,ystyle=1,xtickname=names
	oplot, plcoo, f5, line=1
	oplot, plcoo, f6, line=2
        yt1=bmin+0.73*delb    
        yt1a=bmin+0.60*delb    
        yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Velocity',charsize=1.0
        xyouts, xt2, yt2,'V!Dx!N ___',charsize=1.0
        xyouts, xt2, yt3, 'V!Dy!N ......',charsize=1.0
        xyouts, xt2, yt4, 'V!Dz!N _ _',charsize=1.0
	
	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([f1,f2]) & print, 'Rho,P max: ',bmax
        bmin=min([f1,f2]) & print, 'Rho,P min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0 &  delb=bmax-bmin
        bmax=bmax+0.05*delb & bmin=bmin-0.05*delb & delb=1.1*delb
	plot, plcoo, f1,xtitle=xtit, $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f2, line=1
        yt1=bmin+0.89*delb    
        yt2=bmin+0.63*delb  &   yt2a=bmin+0.51*delb    
        yt3=bmin+0.25*delb  &   yt3a=bmin+0.13*delb    
        xyouts, xt1, yt2,'Density',charsize=1.0
        xyouts, xt2, yt2a,'!7q!X ___',charsize=1.0
        xyouts, xt1, yt3, 'Pressure',charsize=1.0
        xyouts, xt2, yt3a, 'p _ _',charsize=1.0



     if withps eq 'y' then begin 
       withps = 'n'
       device,/close
       set_plot,'x'
       !P.THICK=1.
     endif
     
    goto, gridindex

end

