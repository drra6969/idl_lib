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
  xmin =   0. & ymin = 0.0
  xmax =  12. & ymax = 30.0
;--------------------
   time=0.0 & fnumber='1'
   name='' & contin='' & again='y' & withps='n' & run=''
   nx=long(303) & ny=long(303) & nyy=long(303)

; READ INPUT DATA OF DIMENSION NX, NY
   print, 'Input filenumber'
   read, fnumber
   name='magtap'+fnumber
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

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

   name='magnap'+fnumber
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
  
    xa=0.1 & xe=0.8 & dpy=0.24
    ylo1=0.65 & yup1=ylo1+dpy
    ylo2=0.35 & yup2=ylo2+dpy
    ylo3=0.05 & yup3=ylo3+dpy
   
   ytit='z'
     
    print, 'Which case?'
    read, run

     while (again eq 'y') or (again eq '') do begin

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
    nplane=nx & coord=x & xtit='y' & pmax=ymax & pmin=ymin & endif
  if plane eq 'y' then begin 
    nplane=ny & coord=y & xtit='x' & pmax=xmax & pmin=xmin & endif
    
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
;        device,/landscape
        !P.THICK=2.
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
       endif
       
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=3
       !X.TICKS=6
       !Y.TICKS=6
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
       
        if (plane eq 'x') then begin
          plcoo=y(0:ny-1) & f1=y & f2=y & f1a=y & f2a=y 
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
        endif
        if (plane eq 'y') then begin
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
        endif
        !X.RANGE=[pmin,pmax]
;        !X.RANGE=[2,4]

       
  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot first page

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f1,f2]) & print, 'rho,p max: ',bmax
        bmin=min([f1,f2]) & print, 'rho,p min: ',bmin
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
;        bmax=10000.0 & bmin=1850.0
        ypos0=bmin+delb/8.  
        ypos1=bmin+delb/24. 
        ypos2=bmin+delb/72. 
        ypos3=bmin+delb/216. 
        ypos4=bmin+delb/648. 
	plot_io, plcoo, f1, title=' Rhon, Pn', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f2, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time,'(f6.2)')
	xyouts,xpos,ypos4,plane+' ='+string(coord(igrid),'(f6.2)')
;	xyouts,xpos,ypos2,run
;	xyouts,xpos,ypos3,'Max='
;	xyouts,xpos,ypos4,'  '+string(fmax,'(f7.4)')

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([f1a,f2a]) & print, 'del rho,p max: ',bmax
        bmin=min([f1a,f2a]) & print, 'del rho,p min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f1a, title=' Del Rhon,pn', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f2a, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time,'(f6.2)')
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([f3,f13]) & print, 'T max: ',bmax
        bmin=min([f3,f13]) & print, 'T min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f3,title=' Tn, T',xtitle=xtit, $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f13, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time,'(f6.2)')
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

  endif
  
  print, 'plot 2. page? '
  read, contin
  if (contin eq '' or contin eq 'y') then begin

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f4,f5,f6]) & print, 'Vn max: ',bmax
        bmin=min([f4,f5,f6]) & print, 'Vn min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f4,title=' Vn Components', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f5, line=1
	oplot, plcoo, f6, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time,'(f6.2)')
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([f14,f15,f16]) & print, 'V max: ',bmax
        bmin=min([f14,f15,f16]) & print, 'V min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f14,title=' V Components', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f15, line=1
	oplot, plcoo, f16, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time,'(f6.2)')
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')
	
	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([f11,f12]) & print, 'Rho,P max: ',bmax
        bmin=min([f11,f12]) & print, 'Rho,P min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f11,title=' Rho, P',xtitle=xtit, $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f12, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time,'(f6.2)')
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

  endif
  
  print, 'plot 3. page? '
  read, contin
  if (contin eq '' or contin eq 'y') then begin

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f17,f18,f19]) & print, 'B max: ',bmax
        bmin=min([f17,f18,f19]) & print, 'B min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f17,title='B Componenets', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f18, line=1
	oplot, plcoo, f19, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time,'(f6.2)')
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([f7]) & print, 'Res max: ',bmax
        bmin=min([f7]) & print, 'Res min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
;        bmax=10000.0 & bmin=1850.0
        ypos0=bmin+delb/8.  
        ypos1=bmin+delb/16. 
        ypos2=bmin+delb/32. 
        ypos3=bmin+delb/64. 
        ypos4=bmin+delb/128. 
	plot_io, plcoo, f7,title='Res', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time,'(f6.2)')
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')
	
	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([f8]) & print, 'Nu12s max: ',bmax
        bmin=min([f8]) & print, 'Nu12s min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
;        bmax=10000.0 & bmin=1850.0
        ypos0=bmin+delb/8.  
        ypos1=bmin+delb/24. 
        ypos2=bmin+delb/72. 
        ypos3=bmin+delb/216. 
        ypos4=bmin+delb/648. 
	plot_io, plcoo, f8,title='Nu12s',xtitle=xtit, $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time,'(f6.2)')
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

  endif


     !P.FONT=3

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end

