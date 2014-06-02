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
  unit='s' & whatunit='s'

;----PARAMETER-------
  xmin =  -12. & ymin = 0.
  xmax =   0. & ymax = 1100.
;--------------------
   time=0.0 & fnumber='1'
   name='' & contin='' & again='y' & withps='n' & run=''
   nx=long(303) & ny=long(303) & nyy=long(303)

   ntmax=40
   ix0=13
   iy0=28
   x0=0.0
   h0=100.0

; READ INPUT DATA OF DIMENSION NX, NY
   openr, 8, 'magtap01',/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

          f1=fltarr(ny,ntmax+1)
          f2=f1 & f1a=f1 & f2a=f1 
          f3=f1 & f4=f1 & f5=f1 & f6=f1 & f7=f1 & f8=f1 & f9=f1 & f10=f1
          f11=f1 & f11a=f1 & f12=f1 & f12a=f1 & f13=f1 & f13a=f1
          f14=f1 & f15=f1 & f16=f1 & f17=f1 & f18=f1 & f19=f1 & f20=f1 & f21=f1
	  f01=f1 & f02=f1 
   close, 8

   for it=0,ntmax do begin

; READ INPUT DATA OF DIMENSION NX, NY
   print, 'Input filenumber'
;   read, fnumber	
	fnumber=string(it,form='(i2.2)')
    print,fnumber
;   name='magtap'+fnumber
	name='magtap'+fnumber
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

   x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
   g1=fltarr(nx,/NOZERO) & g2=g1 & g3=g1 & dx=g1
   h1=fltarr(ny,/NOZERO) & h2=h1 & h3=h1 & dy=h1
   bx=fltarr(nx,ny,/NOZERO) & by=bx & bz=bx & 
   vx=bx & vy=bx & vz=bx & vex=bx & vey=bx & vez=bx 
   vnx=bx & vny=bx & vnz=bx 
   rho=bx & rho1=bx & u=bx & u1=bx & te=bx & ti=bx & tp=bx & t0=bx
   rhono=bx & rhono2=bx & rhonn2=bx  & tea=bx & tna=bx & tia=bx
   rhon=bx & un=bx & rhon1=bx & un1=bx 
   res=bx & nu12s=bx & nu12a=bx & ioniz=bx
   p=bx & pp=bx & pe=bx & pn=bx & p1=bx & pn1=bx & jx=bx & jy=bx & jz=bx
   ex=bx & ey=bx & ez=bx 
   w1=bx & w2=bx & w3=bx & w4=bx 

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz,vex,vey,vez
   readu, 8,  rho,rho1,u,u1,te,res
   readu, 8,  rescal,b0,length0,lengthy0,dens0,h0
   readu, 8,  nuei,nuen,nuin ;,ioniz
   readu, 8,  jx,jy,jz
   close, 8
   x0=x(ix0)
   y0=y(iy0)
   vx=vx/rho &  vz=vz/rho & vy=vy/rho
   vex=vex/rho &  vez=vez/rho & vey=vey/rho
   p=2*u^(5.0/3.0) & p1=2*u1^(5.0/3.0)
   pe=rho*te & pp=p-pe & t0=p1/rho1/2.
   ti=pp/rho
;   te=smooth(te,3)
;   ti=smooth(ti,3)
;   rho=smooth(rho,3)

      y = h0 + y/rescal
      dy = dy*rescal


   name='magnap'+fnumber
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
;  print, x
;  print, y
   readu, 8,  vnx,vny,vnz
   readu, 8,  rhono,rhono2,rhonn2,rhon,un,rhon1,un1,nu12s,nu12a,ioniz
   close, 8
;   for iy=0, ny-1 do print, 'iy,rhon,rhon1:',iy, rhon(11,iy),rhon1(11,iy)
   vnx=vnx/rhon &  vnz=vnz/rhon & vny=vny/rhon
   pn=2*un^(5.0/3.0) & pn1=2*un1^(5.0/3.0)

   rhont=rhono+rhono2/2+rhonn2*4/7
   tna = pn1/rhont & tia=tna & tea=p1/rho1-tia

     y = h0 + y/rescal
     dy = dy*rescal
     vy = vy/rescal
     vey = vey/rescal
     vny = vny/rescal
     by = by/rescal

   
      w1 = shift(bz,0,-1)-shift(bz,0,1)
      for j=1,ny-2 do w3(*,j)=dy(j)*w1(*,j)
      jx = f3
      w2 = shift(bz,-1,0)-shift(bz,1,0)
      for i=1,nx-2 do w4(i,*)=dx(i)*w2(i,*)
      jy = -w4
      ey = res*jy
      jx([0,nx-1],*)=0.0 & jy([0,nx-1],*)=0.0 
      jx(*,[0,ny-1])=0.0 & jy(*,[0,ny-1])=0.0 
      gradp=(shift(p,0,-1)-shift(p,0,1))
      for j=1,ny-2 do gradp(*,j)=dy(j)*gradp(*,j)
   
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
  
;  print, 'What unit?'
;  print, 'Options: s: plot in simulation unit'
;  print, '         p: plot in physical unit'
;  print, '    return -> no changes applied'
;  print, 'Present Choice: ', unit
;  read, whatunit
;  if (whatunit eq 's') or (whatunit eq 'p') then unit=whatunit
;  print, 'current unit= ', unit
     
;    print, 'Which case?'
;    read, run


plane='x'
igrid=ix0
        
          plcoo=y(0:ny-1) 

	  f01(*,it)=jy(igrid,*) & f02(*,it)=ey(igrid,*)
          f1(*,it)=rhon(igrid,*) & f2(*,it)=pn(igrid,*) 
          f3(*,it)=pn(igrid,*)/rhont(igrid,*)
          f1a(*,it)=rhon(igrid,*)-rhon1(igrid,*) 
          f2a(*,it)=pn(igrid,*)-pn1(igrid,*) 
          f4(*,it)=vnx(igrid,*) & f5(*,it)=vny(igrid,*)
          f6(*,it)=vnz(igrid,*)
          f7(*,it)=res(igrid,*) & f8(*,it)=nu12s(igrid,*) 
          f11(*,it)=rho(igrid,*) & f11a(*,it)=rho(igrid,*)-rho1(igrid,*)
          f12(*,it)=p(igrid,*) & f12a(*,it)=p(igrid,*)-p1(igrid,*)
          f13(*,it)=pp(igrid,*)/rho(igrid,*) 
          f13a(*,it)=te(igrid,*)
          f14(*,it)=vx(igrid,*) & f15(*,it)=vy(igrid,*) & f16(*,it)=vz(igrid,*)
          f17(*,it)=bx(igrid,*) & f18(*,it)=by(igrid,*) & f19(*,it)=bz(igrid,*)
          f20(*,it)=gradp(igrid,*)
          f21(*,it)=pn(igrid,*)/rhont(igrid,*)-p(igrid,*)/rho(igrid,*)
;plane='y'
;igrid=iy0
        
;          plcoo=x

;	  f01(*,it)=jy(*,igrid) & f02(*,it)=ey(*,igrid)
;          f1(*,it)=rhon(*,igrid) & f2(*,it)=pn(*,igrid) 
;          f3(*,it)=pn(*,igrid)/rhont(*,igrid)
;          f1a(*,it)=rhon(*,igrid)-rhon1(*,igrid) 
;          f2a(*,it)=pn(*,igrid)-pn1(*,igrid) 
;          f4(*,it)=vnx(*,igrid) & f5(*,it)=vny(*,igrid)
;          f6(*,it)=vnz(*,igrid)
;          f7(*,it)=res(*,igrid) & f8(*,it)=nu12s(*,igrid) 
;          f11(*,it)=rho(*,igrid) & f11a(*,it)=rho(*,igrid)-rho1(*,igrid)
;          f12(*,it)=p(*,igrid) & f12a(*,it)=p(*,igrid)-p1(*,igrid)
;          f13(*,it)=ti(*,igrid) 
;          f13a(*,it)=te(*,igrid)
;          f14(*,it)=vx(*,igrid) & f15(*,it)=vy(*,igrid) & f16(*,it)=vz(*,igrid)
;          f17(*,it)=bx(*,igrid) & f18(*,it)=by(*,igrid) & f19(*,it)=bz(*,igrid)
;          f20(*,it)=gradp(*,igrid)
;          f21(*,it)=pn(*,igrid)/rhont(*,igrid)-p(*,igrid)/rho(*,igrid)

     endfor

     while (again eq 'y') or (again eq '') do begin


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
       !X.TICKS=4
       !Y.TICKS=4
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
       pmin=h0 & pmax=ymax
;       pmin=xmin & pmax=xmax
        !X.RANGE=[pmin,pmax]
;        !X.RANGE=[2,4]

    
   b_units=b0*rescal*100000.                     ; in nT
   n_units=dens0                                 ; in cm^(-3)
   v_units=2.18*1e6/4.*rescal*b0/sqrt(dens0)     ; for oxygen in km/s
   vv_units=v_units*1000.                        ; vel in m/s 
   l_units=length0/1e5                           ; in km
   ta_time=l_units/v_units                       ; in s
   curr_units = b0*rescal/l_units/4./3.1415*1e6  ; in micro A/m^2
   e_units = v_units*b_units                     ; in micro V/m
   res_units = e_units/curr_units                ; Ohm m
   t_units = b0^2*rescal^2/8./3.1415/1.38/dens0*1e13 ; in 1000K

print, 'b_units',b_units
print, 'n_units',n_units
print, 'v_units',v_units
print, 'vv_units',vv_units
print, 'l_units',l_units
print, 'ta_time',ta_time
print, 'curr_units',curr_units
print, 'e_units',e_units
print, 'res_units',res_units
print, 't_units',t_units


  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot first page

print, 'plot page 5, jy,ey,res'
; plot  page 5

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=n_units*max([f11]) & print, 'Jy max: ',bmax
        bmin=n_units*min([f11]) & print, 'Jy min: ',bmin
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
;        bmax=10000.0 & bmin=1850.0
;        bmin = 180
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 

	plot, plcoo, n_units*f11(*,1), title='Number Density  (10^5 /cm^3)', $
	  yrange=[bmin,bmax],xstyle=1,ystyle=1, line=0
        for it=2,ntmax-1,2 do begin
           oplot, plcoo, n_units*f11(*,it), line=it/10
        endfor
;	oplot, plcoo, f2, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string( ta_time*time,'(f6.2)')+'s'
	xyouts,xpos,ypos4,plane+' ='+string(x(igrid),'(f6.2)')
;	xyouts,xpos,ypos2,run
;	xyouts,xpos,ypos3,'Max='
;	xyouts,xpos,ypos4,'  '+string(fmax,'(f7.4)')

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=t_units*max([f13]) & print, 'Ey max: ',bmax
        bmin=t_units*min([f13]) & print, 'Ey min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, t_units*f13(*,1),title='Ion Temperature  (1000 K)', $
	  yrange=[bmin,bmax],xstyle=1,ystyle=1
        for it=2,ntmax-1,2 do begin
           oplot, plcoo, t_units*f13(*,it), line=it/10
        endfor
;	oplot, plcoo, f12, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.2)')+'s'
	xyouts,xpos,ypos3,plane+' ='+string(x(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo3,xe,yup3]
;        bmax=res_units*max([f7]) & print, 'Res max: ',bmax
;        bmin=res_units*min([f7]) & print, 'Res min: ',bmin
        bmax=t_units*max([f13a]) & print, 'Te max: ',bmax
        bmin=t_units*min([f13a]) & print, 'Te min: ',bmin
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
	plot, plcoo, t_units*f13a(*,1),title='Electron Temperature  (1000 K)', $
	  yrange=[bmin,bmax],xstyle=1,ystyle=1
        for it=5,ntmax,5 do begin
           oplot, plcoo, t_units*f13a(*,it), line=it/10
        endfor
;	plot_io, plcoo, res_units*f7(*,0),title='Resistivity (ohm m)', $
;	  yrange=[.002,bmax],xstyle=1,ystyle=1
;        for it=1,ntmax-1 do  oplot, plcoo, res_units*f7(*,it), line=it
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.2)')+'s'
	xyouts,xpos,ypos3,plane+' ='+string(x(igrid),'(f6.2)')


  endif

  print, 'plot second page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot first page

print, 'plot page 2'
; plot  page 2

	!P.POSITION=[xa,ylo1,xe,yup1]
;        bmax=max([vv_units*f15]) & print, 'Jy max: ',bmax
;        bmin=min([vv_units*f15]) & print, 'Jy min: ',bmin
;        bmax=max([curr_units*f01]) & print, 'Jy max: ',bmax
;        bmin=min([curr_units*f01]) & print, 'Jy min: ',bmin
        bmax=50.0
        bmin=0.0
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
;        bmax=10000.0 & bmin=1850.0
;        bmin = 180
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 

	plot, plcoo, curr_units*f01(*,0), title='Jz (uA/m^2)', $
	  yrange=[bmin,bmax],xstyle=1,ystyle=1, line=0
        for it=2,ntmax,2 do  oplot, plcoo, curr_units*f01(*,it), line=it
;	oplot, plcoo, f2, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.2)')+'s'
	xyouts,xpos,ypos4,plane+' ='+string(x(igrid),'(f6.2)')
;	xyouts,xpos,ypos2,run
;	xyouts,xpos,ypos3,'Max='
;	xyouts,xpos,ypos4,'  '+string(fmax,'(f7.4)')

	!P.POSITION=[xa,ylo2,xe,yup2]
;        bmax=max([-v_units*f16]) & print, 'Ey max: ',bmax
;        bmin=min([-v_units*f16]) & print, 'Ey min: ',bmin
        bmax=0.0
        bmin=-0.5
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, -v_units*f16(*,0),title='Horizontal velocity Vy (km/s)', $
	  yrange=[bmin,bmax],xstyle=1,ystyle=1
        for it=2,ntmax,2 do  oplot, plcoo, -v_units*f16(*,it), line=it
;	oplot, plcoo, f12, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.2)')+'s'
	xyouts,xpos,ypos3,plane+' ='+string(x(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo3,xe,yup3]
;        bmax=max([-b_units*f19]) & print, 'Res max: ',bmax
;        bmin=min([-b_units*f19]) & print, 'Res min: ',bmin
        bmax=40.0
        bmin=0.0
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
	plot, plcoo, -b_units*f19(*,0),title='Magnetic Field By (nT)', $
	  yrange=[bmin,bmax],xstyle=1,ystyle=1
        for it=2,ntmax,2 do  oplot, plcoo, -b_units*f19(*,it), line=it
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.2)')+'s'
	xyouts,xpos,ypos3,plane+' ='+string(x(igrid),'(f6.2)')


  endif

     !P.FONT=3

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

;save,file='/why1/zhu/io2d/ray/data/newcurr/up.dat',$
save,file='up.dat',$
;save,file='/why1/zhu/io2d/gradpe/agu/data/curr/curr/curr_up.dat',$
             /xdr,h0,b_units,n_units,v_units,x0,$
             vv_units,l_units,ta_time,curr_units,e_units,res_units,$
             t_units,f01,f02,f7,f11,f13,f13a,plcoo
end

