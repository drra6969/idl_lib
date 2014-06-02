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
   nxn = 21   &   nyn = 21
   iox=fltarr(nxn) & ioy=fltarr(nyn)
   fn1=fltarr(nxn,nyn) & fn2=fn1 & fn3=fn1 & fn4=fn1 
; GRID FOR CONTOUR AND SURFACE PLOTS
   nxf = 81   &   nyf = 81
   ioxf=fltarr(nxf) & ioyf=fltarr(nyf)
   fa=fltarr(nxf,nyf) & fb=fa
  names=strarr(15)
  names=replicate(' ',15)

;----PARAMETER-------
  xmin =   0. & ymin = 0.0
  xmax =  20. & ymax = 4.0
;--------------------
   time=0.0 & fnumber=1
   name='' & contin='' & again='y' & withps='n' & run=''
   nx=long(303) & ny=long(303)

; READ INPUT DATA OF DIMENSION NX, NY
   print, 'Input filenumber'
   read, fnumber
   name='magtap0'+string(fnumber,'(i1.1)')
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  ny,nx,time
   print, 'dimension nx=',nx,'     ny=',ny

   x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
   g1=fltarr(nx,/NOZERO) & g2=g1 & g3=g1 & dx=g1
   h1=fltarr(ny,/NOZERO) & h2=h1 & h3=h1 & dy=h1
   bx=fltarr(ny,nx,/NOZERO) & by=bx & bz=bx & vx=bx & vy=bx & vy=bx
   rho=bx & u=bx & res=bx

   readu, 8,  y,dy,h2,h3,h3,h3,h3, x,dx,g2,g3,g3,g3,g3 
;  print, x
;  print, y
   readu, 8,  by,bx,bz
   readu, 8,  vy,vx,vz
   readu, 8,  rho,u,res
   close, 8
   vx=vx/rho & vy=vy/rho  &  vz=vz/rho
   bx=rotate(bx,4) & by=rotate(by,4) & bz=rotate(bz,4)
   vx=rotate(vx,4) & vy=rotate(vy,4) & vz=rotate(vz,4)
   rho=rotate(rho,4) & u=rotate(u,4) & res=rotate(res,4)
   
   bsq=bx & p=bx & jz=bx & ez=bx & jx=bx & jy=bx
   f1=bx & f2=bx & f3=bx & f4=bx 
   a=fltarr(nx,ny) 

      f1 = shift(bz,0,-1)-shift(bz,0,1)
      for j=1,ny-2 do f3(*,j)=dy(j)*f1(*,j)
      jx = f3
      f2 = shift(bz,-1,0)-shift(bz,1,0)
      for i=1,nx-2 do f4(i,*)=dx(i)*f2(i,*)
      jy = -f4
      jx([0,nx-1],*)=0.0 & jy([0,nx-1],*)=0.0 
      jx(*,[0,ny-1])=0.0 & jy(*,[0,ny-1])=0.0 
   
;   for iv=0, ny-1 do vy(*,iv)=vy(*,iv)/rho(*,iv)-1.5*tanh(x)
  
  
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
   delx=xmax-xmin & dely=ymax-ymin & sizeratio=(xmax-xmin)/(ymax-ymin)
   print, 'Plot xmin, xmax:', xmin, xmax
   print, '     zmin, zmax:', ymin, ymax
   print, 'Program xmin, xmax:', x(1), x(nx-2)
   print, '        zmin, zmax:', y(1), y(ny-2)

; GRID FOR VELOCITY VECTORS 
   xn=findgen(nxn) & yn=findgen(nyn)
   dxn=(xmax-xmin)/float(nxn-1) & xn=xn*dxn+xmin
   dyn=(ymax-ymin)/float(nyn-1) & yn=yn*dyn+ymin
   in=-1 & k=0
   repeat begin
     in=in+1
     while xn(in) gt x(k+1) do k=k+1
     iox(in) = float(k) + (xn(in)-x(k))/(x(k+1)-x(k)) 
   endrep until in eq nxn-1
   in=-1 & k=0
   repeat begin
     in=in+1
     while yn(in) gt y(k+1) do k=k+1
     ioy(in) = float(k) + (yn(in)-y(k))/(y(k+1)-y(k))        
   endrep until in eq nyn-1

; GRID FOR CONTOUR/SURFACE PLOTS
   xf=findgen(nxf) & yf=findgen(nyf)
   dxf=(xmax-xmin)/float(nxf-1) & xf=xf*dxf+xmin
   dyf=(ymax-ymin)/float(nyf-1) & yf=yf*dyf+ymin
   in=-1 & k=0
   repeat begin
     in=in+1
     while xf(in) gt x(k+1) do k=k+1
     ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
   endrep until in eq nxf-1
   in=-1 & k=0
   repeat begin
     in=in+1
     while yf(in) gt y(k+1) do k=k+1
     ioyf(in) = float(k) + (yf(in)-y(k))/(y(k+1)-y(k))        
   endrep until in eq nyf-1

; VECTORPOTENTIAL:
    a=0.0*bx
    for k=3, ny-2, 2 do $
        a(1,k)=a(1,k-2)+bx(1,k-1)*(y(k)-y(k-2))
    for k=2, ny-3, 2 do $
        a(1,k)=a(1,k-1)+0.5*(bx(1,k-1)+bx(1,k))*(y(k)-y(k-1))
    for k=1, ny-1 do begin
     for l=3, nx-2,2 do begin
        a(l,k)=a(l-2,k)-by(l-1,k)*(x(l)-x(l-2))
     endfor  
    endfor
    for k=1, ny-1 do $
     for l=2, nx-3,2 do $
        a(l,k)=a(l-1,k)-0.5*(by(l-1,k)+by(l,k))*(x(l)-x(l-1))
    fmax=max(a((nx-1)/2,2:ny-1))
    fmin=min(a((nx-1)/2,2:ny-1))
    print, fmax, fmin
;    print, a(*,0),a(*,1)
    i=1 & while (by(i,1) lt 0.0) and (i lt nx) do i=i+1
    aa = max([a(i,1),a(i-1,1)])

; CURRENT DENSITY J_Z AND ELECTRIC FIELD E_Z
   f1=shift(by,-1,0)-shift(by,1,0)
   f2=shift(bx,0,-1)-shift(bx,0,1)
   for j=0,ny-1 do f1(*,j) = dx*f1(*,j)
   for i=0,nx-1 do f2(i,*) = dy*f2(i,*)
   jz=f1-f2 & jz(0,*)=0. & jz(nx-1,*)=0. & jz(*,0)=0. & jz(*,ny-1)=0.
   ez=-(vx*by-vy*bx) +res*jz
  
; COORDINATES FOR PLOTS
   srat=1.0
   if (sizeratio lt 2.6) then begin
     dpx=0.27*sizeratio & dpy=0.23 
   endif
   if (sizeratio ge 2.6 and sizeratio le 4.0) then begin
     dpx=0.702 & dpy=0.23*2.6/sizeratio
   endif
   if (sizeratio gt 4.0) then begin
     dpx=0.702 & dpy=0.23*2.6/4.
     srat=sizeratio/4.5
   endif
   print, sizeratio, dpx, dpy

;    dpx=0.8 & dpy =0.18 
    yup=0.06 & yinter=0.05 & hop=0.47*yinter
    ya1=yup & ye1=yup+dpy  
    ya2=ye1+yinter & ye2=ya2+dpy
    ya3=ye2+yinter & ye3=ya3+dpy

    xlo=0.1 & xup=xlo+dpx
 	
    pos1=[xlo,ya1,xup,ye1]
    pos2=[xlo,ya2,xup,ye2]
    pos3=[xlo,ya3,xup,ye3]
    hopp=[0,hop,0,hop]

   xpmin=xmin
   xpmax=xmax
   ytit='z'
     

    print, 'Which case?'
    read, run

     while (again eq 'y') do begin

      !P.THICK=1.
      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con'+string(time,'(i3.3)')+'.ps'
;        device,/landscape
        !P.THICK=2.
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
       endif

        xpos=xpmax+0.01*delx/dpx
        ypos0=ymin+0.18*dely         ; location for 'time'
        ypos1=ypos0-0.021*dely/dpy   ; next line
        ypos2=ypos0+0.075*dely/dpy   ; location 'CASE'
        ypos3=ymin+.8*dely          ; location 'Max='
        ypos4=ypos3-0.02*dely/dpy   ; next line
        ypos5=ypos4-0.03*dely/dpy         ; location 'Min='
        ypos6=ypos5-0.02*dely/dpy   ; next line

       
  print, 'plot first page? Magnetic field, flow vectors'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot first page
;       !P.REGION=[0.,0.,1.0,1.25]
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=3
       !X.TICKS=4
       !Y.TICKS=4
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
       !X.RANGE=[xpmin,xpmax]
       !Y.RANGE=[ymin,ymax]


	!P.POSITION=pos3
        fa=interpolate(a,ioxf,ioyf,/grid)
	bmax=max(fa,indmax) & bmin=min(fa,indmin) & del=(bmax-bmin)/16.
        ixmin=indmin mod nxf
	fmax=max(fa(ixmin,1:nyf-1))
        delf=fmax-fmin
        print, 'magnetic flux:', del
        fb=fmax+5.0
        lvec=findgen(17)*del+bmin & lvec(16)=fb
        lvec=lvec(sort(lvec))
	contour,fa,xf,yf,levels=lvec,$
        c_linestyle=lvec lt aa,$ 
        title=' Magnetic Field and Plasma Velocity',xstyle=1,ystyle=1,$
        ytitle=ytit,xtickname=names

;        ytitle=ytit,ytickname=names

        fn1=interpolate(vx,iox,ioy,/grid)
        fn2=interpolate(vy,iox,ioy,/grid)
        fmax=sqrt(max(fn1^2+fn2^2))
        vect, fn1, fn2, xn, yn, length=2.0*fmax,$
        title=' '
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(time,'(i3)')
	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max='
	xyouts,xpos,ypos4,'  '+string(fmax,'(f7.4)')



	!P.POSITION=pos2
        smo,bz,f1,1
        f1=smooth(f1,3)
        fa=interpolate(f1,ioxf,ioyf,/grid) ;& fa=smooth(fa,3)
	bmax=max(fa) & bmin=min(fa)
        if bmax eq bmin then bmax=bmin+1
        del=(bmax-bmin)/15.
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt 0.0,$
        title='By Component',xstyle=1,ystyle=1,$
        ytitle=ytit,xtickname=names,/noerase
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(time,'(i3)')
	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max'
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)')
	xyouts,xpos,ypos5,'Min'
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)')



	!P.POSITION=pos1
        smo,jz,f1,1   ;&f1=smooth(jz,5) 
        fa=interpolate(f1,ioxf,ioyf,/grid) ;& fa=smooth(fa,5)
	bmax=max(fa) & bmin=min(fa)
        if bmax eq bmin then bmax=bmin+1
        del=(bmax-bmin)/8.
	contour,fa,xf,yf,levels=findgen(9)*del+bmin,$
        c_linestyle=findgen(9)*del+bmin lt 0.0,$
        title=' Current Density',xstyle=1,ystyle=1,$
        xtitle=' x',ytitle=ytit ,/noerase
;      	contour,fa,xf,yf,levels=lvec,$
;        c_linestyle=lvec gt 0.0,$ 
;        title='Current Density',$
;        xstyle=1,ystyle=1,$
;        xtitle='x',ytitle=ytit,thick=1.0
	xyouts,xpos,ypos3,'Max' 
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min' 
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)') 



        smo,jx,f1,1  & smo,jy,f2,1         
        fn1=interpolate(f1,iox,ioy,/grid)
        fn2=interpolate(f2,iox,ioy,/grid)
        fmax=sqrt(max(fn1^2+fn2^2))
        fa=sqrt(fn1^2+fn2^2)
        clim=0.15
        ll=where(fa gt clim,count)
        print,'count=',count
        if count gt 0 then begin
          fn1(where(fa gt clim))=0.0  &  fn2(where(fa gt clim))=0.0
        endif
        fmax=sqrt(max(fn1^2+fn2^2))
        vect, fn1, fn2, xn, yn, length=15.*fmax,$
        title=' '
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(time,'(i3)')



  endif

  print, 'continue with next plot, Bz,',$
         '  and overlay current and B lines'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot 2. page
        fa=interpolate(a,ioxf,ioyf,/grid)
	bmax=max(fa,indmax) & bmin=min(fa,indmin) & del=(bmax-bmin)/20.
	fmax=min(fa(1,1:(nyf-2)))
        fb=fmax
        lvec=findgen(21)*del+bmin & lvec(20)=fb-0.001
        bav=0.5*(bmax+bmin)
        wwide=2.0 & mmid=bav & ooben=mmid+wwide & uunten=mmid-wwide
        print, bav, mmid
        lvec=lvec(sort(lvec))
        print, lvec
      
	!P.POSITION=pos1
	contour,fa,xf,yf,levels=lvec,$
        c_linestyle=lvec lt aa,$ 
        xstyle=1,ystyle=1,$
        xtitle='x',ytitle=ytit,thick=1.0

        smo,jx,f1,1  & smo,jy,f2,1 
        f1=smooth(f1,3)  & f2=smooth(f2,3) 
        fn1=interpolate(f1,iox,ioy,/grid)
        fn2=interpolate(f2,iox,ioy,/grid)
        fmax=sqrt(max(fn1^2+fn2^2))
	!P.POSITION=pos1
        vect, fn1, fn2, xn, yn, length=0.8,$
        title=' B Field and Current Density'
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(time,'(i3)')
;	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max'
	xyouts,xpos,ypos4,' '+string(fmax,'(f7.4)')


	!P.POSITION=pos2
        fa=interpolate(rho,ioxf,ioyf,/grid) 
	bmax=max(fa) & bmin=min(fa) & bav=0.5*(bmax+bmin)
        if bmax eq bmin then bmax=bmin+1 & del=(bmax-bmin)/11.
	contour,fa,xf,yf,levels=findgen(12)*del+bmin+0.01,$
        c_linestyle=findgen(12)*del+bmin lt bav,$
        title=' Density !9 r !X',xstyle=1,ystyle=1,$
        xtitle=' ',xtickname=names,ytitle=ytit ,/noerase
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(time,'(i3)')
	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max='
	xyouts,xpos,ypos4,'  '+string(bmax,'(f5.2)')
	xyouts,xpos,ypos5,'Min='
	xyouts,xpos,ypos6,'  '+string(bmin,'(f5.2)')



	!P.POSITION=pos3
        bsq=bx*bx+by*by+bz*bz
        p=2*u^(5.0/3.0)
        bsq=bsq+p
        f1=(jx*bx+jy*by)/sqrt(bx*bx+by*by)
        smo,f1,f1,1   &        f1=smooth(f1,3)
        fa=interpolate(f1,ioxf,ioyf,/grid) 
;        fa=smooth(fa,3) 
	bmax=max(fa)
	bmin=min(fa)
        bav=0.
        del=(bmax-bmin)/15.
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
        title=' J_parall (pol)',xstyle=1,ystyle=1,$
        ytitle=ytit,xtickname=names
        ;,ytitle=ytit 
        
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max' 
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min' 
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)') 
  endif


  print, 'continue with next plot, density,',$
         ' pressure, and total pressure'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot 3. page
	!P.POSITION=pos1
        p=2*u^(5.0/3.0)
        fa=interpolate(p,ioxf,ioyf,/grid) & fa=smooth(fa,3)
	bmax=max(fa) & bmin=min(fa) & bav=0.5*(bmax+bmin) & del=(bmax-bmin)/15.
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
        title=' Pressure',xstyle=1,ystyle=1,$
        xtitle=' ',ytitle=ytit,xtickname=names
        
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos3,'Max' 
	xyouts,xpos,ypos4,''+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min' 
	xyouts,xpos,ypos6,''+string(bmin,'(f5.2)') 





	!P.POSITION=pos2
        f1=smooth(bx,5)
        fa=interpolate(f1,ioxf,ioyf,/grid) 
	bmax=max(fa) & bmin=min(fa)
        if bmax eq bmin then bmax=bmin+1
        del=(bmax-bmin)/15.
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt 0.0,$
        title='BN Component',xstyle=1,ystyle=1,$
        ytitle=ytit,xtickname=names,/noerase
        ;,ytitle=ytit
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(time,'(i3)')
	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max'
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)')
	xyouts,xpos,ypos5,'Min'
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)')



	!P.POSITION=pos3
        bsq=bx*bx+by*by+bz*bz
        p=2*u^(5.0/3.0)
        bsq=bsq+p
        fa=interpolate(bsq,ioxf,ioyf,/grid) & fa=smooth(fa,3) 
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
        title=' Tot. Pressure',xstyle=1,ystyle=1,$
        ytitle=ytit,xtickname=names
        ;,ytitle=ytit 
        
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max' 
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min' 
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)') 
  endif



     !P.FONT=3


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end

