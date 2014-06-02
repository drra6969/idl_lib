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
  plane='x' & whatcut='x' & igrid=2 & newgrid='2'

;----PARAMETER-------
  xmin = -12. & ymin = -20.0
  xmax =  12. & ymax =  20.0
;--------------------
   time=0.0 & fnumber='1'
   name='' & contin='' & again='y' & withps='n' & run=''
   nx=long(303) & ny=long(303) 

; READ INPUT DATA OF DIMENSION NX, NY
   print, 'Input filenumber'
   read, fnumber
   name='magtap'+fnumber
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,time
   print, 'dimension nx=',nx,'     ny=',ny

   x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
   g1=fltarr(nx,/NOZERO) & g2=g1 & g3=g1 & dx=g1
   h1=fltarr(ny,/NOZERO) & h2=h1 & h3=h1 & dy=h1
   bx=fltarr(nx,ny,/NOZERO) & by=bx & bz=bx & 
   vx=bx & vy=bx & vz=bx 
   rho=bx & u=bx 
   res=bx 
   p=bx & jx=bx & jy=bx & jz=bx 
   f1=bx & f2=bx & f3=bx & f4=bx 

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
;  print, x
;  print, y
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz
   readu, 8,  rho,u,res
   close, 8
   vx=vx/rho &  vz=vz/rho & vy=vy/rho  & p=2*u^(5.0/3.0) 

   f1=shift(by,-1,0)-shift(by,1,0)
   f2=shift(bx,0,-1)-shift(bx,0,1)
   for j=0,ny-1 do f1(*,j) = dx*f1(*,j)
   for i=0,nx-1 do f2(i,*) = dy*f2(i,*)
   jz=f1-f2 & jz(0,*)=0. & jz(nx-1,*)=0. & 
   jz(*,0)=jz(*,ny-3) & jz(*,ny-1)=jz(*,2)
   ez=-(vx*by-vy*bx) +res*jz
  
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
  
    xa=0.05 & xe=0.85 & dpy=0.24 & hopp=0.03
    ylo1=0.65        & yup1=ylo1+dpy
    ylo2=ylo1-dpy-hopp & yup2=ylo2+dpy
    ylo3=ylo2-dpy-hopp & yup3=ylo3+dpy
   
   ytit='z'
   
  pi = 3.14159265536 & phi=20.0
  phir = phi*pi/180.0
  f2 = by*cos(phir) - bz*sin(phir) & f3 = by*sin(phir) + bz*cos(phir)
  by=f2 & bz=f3
  f2 = vy*cos(phir) - vz*sin(phir) & f3 = vy*sin(phir) + vz*cos(phir)
  vy=f2 & vz=f3

    ns=-120 & nr=ny-2
    rho=shift(rho(*,1:nr),0,ns) & p=shift(p(*,1:nr),0,ns)
    vx=shift(vx(*,1:nr),0,ns) & vy=shift(vy(*,1:nr),0,ns) 
    vz=shift(vz(*,1:nr),0,ns)
    bx=shift(bx(*,1:nr),0,ns) & by=shift(by(*,1:nr),0,ns) 
    bz=shift(bz(*,1:nr),0,ns)
    jz=shift(jz(*,1:nr),0,ns) & ez=shift(ez(*,1:nr),0,ns) 

     
    print, 'Which case?'
    read, run

     while (again eq 'y') do begin

cut:
  print, 'Input - What Cut Through The 2-D System:'
  print, 'Options: x: cut at x =const'
  print, '         y: cut at y =const'
  print, '    return -> no changes applied'
  print, '         q -> terminate'
  print, 'Present Choice: ', plane
  read, whatcut
  if whatcut eq 'q' then stop
  if whatcut eq 'x' then plane='x'
  if whatcut eq 'y' then plane='y'
  if whatcut eq '' then print,'choice=',plane,' not altered'
  if plane eq 'x' then begin 
   nplane=nx & coord=x & xtit='y' & max=y(ny-1) & min=y(0) & del=max-min & endif
  if plane eq 'y' then begin 
   nplane=ny & coord=y & xtit='x' & max=x(nx-1) & min=x(0) & del=max-min & endif
    
gridindex:
  print, plane, 'Coordinates:
  for i=0,nplane-1,2 do print, i,'  ',plane,'=',coord(i)
  print, 'Input - i =Grid Index of Chosen Plane(>0 and <',nplane-1,')'
  print, 'Options:   integer -> grid index'
  print, '            return -> no changes applied'
  print, '                 c -> back to cut'
  print, '                 q -> terminate'
  print, 'Present Choice: ', igrid
  read, newgrid 
  if newgrid eq 'q' then stop
  if newgrid eq 'c' then goto, cut
  if newgrid ne '' then igrid=fix(newgrid)
  if newgrid eq '' then print,'choice=',igrid,' not altered'
  if (igrid lt 0) or (igrid gt nplane-1) then igrid=(nplane+1)/2  


      !P.THICK=1.
      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
;        device,/portrait
        !P.THICK=2.
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
       endif

       
  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot first page
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=3
       !X.TICKS=4
;       !Y.TICKS=6
       !Y.TICKlen=0.02
       !X.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
;       !X.RANGE=[xmin,xmax]
;       !Y.RANGE=[ymin,ymax]

        if (plane eq 'x') then begin
          plcoo=y(0:ny-1) & f1=y & f2=y 
          f3=y & f4=y & f5=y & f6=y
          f7=y & f8=y & f9=y & f10=y & f11=y & f12=y & f13=y 
          f1=rho(igrid,*) & f2=p(igrid,*) & f3=p(igrid,*)/rho(igrid,*) 
          f4=vx(igrid,*) & f5=vy(igrid,*) & f6=vz(igrid,*)
          f7=bx(igrid,*) & f8=by(igrid,*) & f9=bz(igrid,*)
          f10=bx(igrid,*)^2 + by(igrid,*)^2 + bz(igrid,*)^2
          f11=f2+f10 & f12=smooth(jz(igrid,*),3) & f13=ez(igrid,*)
        endif
        if (plane eq 'y') then begin
          plcoo=x & f1=x & f2=x 
          f3=x & f4=x & f5=x & f6=x
          f7=x & f8=x & f9=x & f10=x & f11=x & f12=x & f13=x 
          f1=rho(*,igrid) & f2=p(*,igrid) & f3=p(*,igrid)/rho(*,igrid) 
          f4=vx(*,igrid) & f5=vy(*,igrid) & f6=vz(*,igrid)
          f7=bx(*,igrid) & f8=by(*,igrid) & f9=bz(*,igrid)
          f10=bx(*,igrid)^2 + by(*,igrid)^2 + bz(*,igrid)^2
          f11=f2+f10 & f12=jz(*,igrid) & f13=ez(*,igrid)
        endif
        !X.RANGE=[min,max]
;        !X.RANGE=[2,4]
        

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f1,f3]) & print, 'rho,T max: ',bmax
        bmin=min([f1,f3]) & print, 'rho,T min: ',bmin
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=max+0.015*del
;        bmax=10000.0 & bmin=1850.0
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.8*delb 
        ypos2=bmin+0.6*delb 
        ypos3=bmin+0.45*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f1, title=' Rho, T', $
	yrange=[bmin,bmax],xstyle=1,xtickname=names
	oplot, plcoo, f3, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time,'(f6.2)')
	xyouts,xpos,ypos2,plane+' ='+string(coord(igrid),'(f6.2)')
;	xyouts,xpos,ypos2,run
;	xyouts,xpos,ypos3,'Max='
;	xyouts,xpos,ypos4,'  '+string(fmax,'(f7.4)')

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([f2,f10,f11]) & print, 'p max: ',bmax
        bmin=min([f2,f10,f11]) & print, 'p min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=max+0.015*del
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.8*delb 
        ypos2=bmin+0.6*delb 
        ypos3=bmin+0.45*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f2, title='P,P_b, and P_tot', $
	yrange=[bmin,bmax],xstyle=1,xtickname=names
	oplot, plcoo, f10, line=1
	oplot, plcoo, f11, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time,'(f6.2)')
	xyouts,xpos,ypos2,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([f7,f8,f9]) & print, 'B max: ',bmax
        bmin=min([f7,f8,f9]) & print, 'B min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=max+0.015*del
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.7*delb 
        ypos2=bmin+0.5*delb 
        ypos3=bmin+0.3*delb 
        ypos4=bmin+0.1*delb 
	plot, plcoo, f7,title='B Components',xtitle=xtit, $
	yrange=[bmin,bmax],xstyle=1
	oplot, plcoo, f8, line=1
	oplot, plcoo, f9, line=2

  endif
  
  print, 'plot 2. page? '
  read, contin
  if (contin eq '' or contin eq 'y') then begin

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f4,f5,f6]) & print, 'V max: ',bmax
        bmin=min([f4,f5,f6]) & print, 'V min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=max+0.015*del
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.7*delb 
        ypos2=bmin+0.5*delb 
        ypos3=bmin+0.3*delb 
        ypos4=bmin+0.1*delb 
	plot, plcoo, f4,title=' V Components', $
	yrange=[bmin,bmax],xstyle=1,xtickname=names
	;,ystyle=1
	oplot, plcoo, f5, line=1
	oplot, plcoo, f6, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time,'(f6.2)')
	xyouts,xpos,ypos2,plane+' ='+string(coord(igrid),'(f6.2)')
;	xyouts,xpos,ypos2,run
;	xyouts,xpos,ypos3,'Max='
;	xyouts,xpos,ypos4,'  '+string(fmax,'(f7.4)')

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([f12]) & print, 'Jz max: ',bmax
        bmin=min([f12]) & print, 'Jz min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=max+0.015*del
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.7*delb 
        ypos2=bmin+0.5*delb 
        ypos3=bmin+0.3*delb 
        ypos4=bmin+0.1*delb 
	plot, plcoo, f12,title='Jz', $
	yrange=[bmin,bmax],xstyle=1,xtickname=names
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time,'(f6.2)')
	xyouts,xpos,ypos2,plane+' ='+string(coord(igrid),'(f6.2)')
	
	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([f13]) & print, 'Ez max: ',bmax
        bmin=min([f13]) & print, 'Ez min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=max+0.015*del
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.7*delb 
        ypos2=bmin+0.5*delb 
        ypos3=bmin+0.3*delb 
        ypos4=bmin+0.1*delb 
	plot, plcoo, f13,title=' Ez',xtitle=xtit, $
	yrange=[bmin,bmax],xstyle=1

  endif
  


     !P.FONT=3


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end

