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
   nxf = 101   &   nyf = 101
   ioxf=fltarr(nxf) & ioyf=fltarr(nyf)
   fa=fltarr(nxf,nyf) & fb=fa
  names=strarr(15)
  names=replicate(' ',15)

;----PARAMETER-------
  xmin = -16. & ymin = -20.
  xmax = 16. & ymax =  20.
;--------------------
   time=0.0 & fnumber=1
   name='' & contin='' & again='y' & withps='n' 
   run='' & closeps='n' & plot='0' 
   nx=long(503) & ny=long(503)

; READ INPUT DATA OF DIMENSION NX, NY
   print, 'Input filenumber'
   read, fnumber
   name='magtap'+string(fnumber,'(i2.2)')
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,time
   print, 'dimension nx=',nx,'     ny=',ny

   x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
   g1=fltarr(nx,/NOZERO) & g2=g1 & g3=g1 
   h1=fltarr(ny,/NOZERO) & h2=h1 & h3=h1 
   bx=fltarr(nx,ny,/NOZERO) & by=bx & bz=bx & vx=bx & vy=bx & vz=bx
   rho=bx & u=bx & res=bx
   bsq=bx & p=bx & jz=bx & ez=bx
   f1=bx & f2=bx & bzpro=bx & vzpro=bx & vypro=bx
   a=fltarr(nx,ny) 

   readu, 8,  x,g1,g2,g3,g3,g3,g3, y,h1,h2,h3,h3,h3,h3
  print, 'after read x'
;  print, y
   readu, 8,  bx,by,bz
  print, 'after read b'
   readu, 8,  vx,vy,vz
  print, 'after read v'
   readu, 8,  rho,u,res
  print, 'after read rho'
   close, 8
   vx=vx/rho &  vz=vz/rho
   vy=vy/rho 
    
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
  
; CURRENT DENSITY J_Z AND ELECTRIC FIELD E_Z
   f1=shift(by,-1,0)-shift(by,1,0)
   f2=shift(bx,0,-1)-shift(bx,0,1)
   for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
   for i=0,nx-1 do f2(i,*) = h1*f2(i,*)
   jz=f1-f2 & jz(0,*)=0. & jz(nx-1,*)=0. & jz(*,0)=0. & jz(*,ny-1)=0.
   ez=-(vx*by-vy*bx) +res*jz
  
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
    a(*,0)=a(*,ny-3) & a(*,ny-1)=a(*,2) 
    fmax=max(a((nx-1)/2,2:ny-1))
    fmin=min(a((nx-1)/2,2:ny-1))
    print, 'vectorpotential:',fmax, fmin
    
  pi = 3.14159265536 & phi=25.0
  phir = phi*pi/180.0
  bzpro = by*sin(phir) + bz*cos(phir)
  vzpro = vy*sin(phir) + vz*cos(phir)
  vypro = vy*cos(phir) - vz*sin(phir)
    vy1=0.0   & vy2=0.0
    for iv=1, ny-2 do vy1=vy1+vypro(1,iv)
    for iv=1, ny-2 do vy2=vy2+vypro(nx-2,iv)
    vymsp=vy1/(ny-2) & vymsh=vy2/(ny-2) & vyav=(vy1+vy2)/2.
    print, 'vy_msp = ', vymsp,'   vy_msh = ',vymsh,'   vyav = ',vyav
    vypros=vymsp
    vys=vypros*cos(phir) & vzs=-vypros*sin(phir)
    
    
    nsh=-100
    a=shift(a(*,1:ny-2),0,nsh) & rho=shift(rho,0,nsh) & u=shift(u,0,nsh)
    vx=shift(vx,0,nsh) & vy=shift(vy,0,nsh) & vz=shift(vz,0,nsh)
    bx=shift(bx,0,nsh) & by=shift(by,0,nsh) & bz=shift(bz,0,nsh)
    jz=shift(jz,0,nsh) & ez=shift(ez,0,nsh) & bzpro=shift(bzpro,0,nsh) 
    vzpro=shift(vzpro,0,nsh) & vypro=shift(vypro,0,nsh) 
    
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

; COORDINATES FOR PLOTS
   srat=1.0 & dpx=0.21 & & dpy=0.87
   if (sizeratio lt 3.0) then  dpy=0.29*sizeratio
   if (sizeratio lt 2.0) then  dpy=0.58
   print, sizeratio, dpx, dpy
    xleft=0.06 & xinter=0.075 & hop=0.47*xinter
    xa1=xleft & xe1=xleft+dpx  
    xa2=xe1+xinter & xe2=xa2+dpx
    xa3=xe2+xinter & xe3=xa3+dpx
    ylo=0.06 & yup=ylo+dpy
   
   ytit='y'
   xpmin=xmax
   xpmax=xmin
   if (ytit eq 'y') then begin     
     xpmin=xmin
     xpmax=xmax
     
   endif

    print, 'Which case?'
    read, run

     while (again eq 'y') do begin

      !P.THICK=1.
      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
        device,/landscape
        !P.THICK=2.
        device,/inches,xsize=10.,scale_factor=1.0,xoffset= 0.8
        device,/inches,ysize=8.0,scale_factor=1.0,yoffset=10.5
;        device,/times,/bold,font_index=3
       endif

        xpos=xpmax-0.01*delx/dpx
        if (ytit eq 'y') then  xpos=xpmax+0.01*delx/dpx
        ypos0=ymin+0.2*dely         ; location for 'time'
        ypos1=ypos0-0.025*dely/dpy   ; next line
        ypos2=ypos0+0.075*dely/dpy   ; location 'CASE'
        ypos3=ymin+.8*dely          ; location 'Max='
        ypos4=ypos3-0.025*dely/dpy   ; next line
        ypos5=ypos4-0.05*dely/dpy         ; location 'Min='
        ypos6=ypos5-0.025*dely/dpy   ; next line

       
  print, 'plot first page? Magnetic field, flow vectors'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot first page
;       !P.REGION=[0.,0.,1.0,1.25]
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=3
       !X.TICKS=2
       !Y.TICKS=8
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
       !X.RANGE=[xpmin,xpmax]
       !Y.RANGE=[ymin,ymax]


        fa=interpolate(a,ioxf,ioyf,/grid)
	bmax=max(fa) & bmin=min(fa) & del=(bmax-bmin)/16.
        bav=0.5*(bmax+bmin)
        print, 'vecpot:', bmax, bmin, bav
        lvec=findgen(17)*del+bmin & lvec(16)=bav-1.2*del
        lvec=lvec(sort(lvec))
        print, 'lvecs:',lvec
	!P.POSITION=[xa1,ylo,xe1,yup]
	contour,fa,xf,yf,levels=lvec,$
        c_linestyle=lvec lt bav-1.5*del,$ 
        ;title=' B Field and Flow',
        xstyle=1,ystyle=1,$
        xtitle='x',ytitle=ytit,thick=1.0

        fn1=interpolate(vx,iox,ioy,/grid)
        fn2=interpolate(vy,iox,ioy,/grid)
        fmax=sqrt(max(fn1^2+fn2^2))
	!P.POSITION=[xa1,ylo,xe1,yup]
        vect, fn1, fn2, xn, yn, length=1.0,$
        title=' B Field and Flow'
	xyouts,xpos,ypos3,'Max'
	xyouts,xpos,ypos4,' '+string(fmax,'(f5.2)')

        fa=interpolate(vz,ioxf,ioyf,/grid) 
	bmax=max(fa) & bmin=min(fa)
        bav=0.5*(bmax+bmin)
        if bmax eq bmin then bmax=bmin+1
        del=(bmax-bmin)/11.
	!P.POSITION=[xa2,ylo,xe2,yup]
	contour,fa,xf,yf,levels=findgen(12)*del+bmin+0.01,$
        c_linestyle=findgen(12)*del+bmin lt 0.0,$
;        title=' Density !9 r !X',xstyle=1,ystyle=1,$
        title=' Vz', xstyle=1,ystyle=1,$
        xtitle='x',ytickname=names,/noerase
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(time,'(i3)')
	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max='
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)')
	xyouts,xpos,ypos5,'Min='
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)')

        fa=interpolate(vzpro,ioxf,ioyf,/grid) & fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa3,ylo,xe3,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt 0.0,$
        title='Vz_orig',xstyle=1,ystyle=1,$
        xtitle=' x', ytickname=names
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max' 
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min' 
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)') 

  endif

  print, 'continue with next plot, BN,',$
         '  and overlay V and B lines'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot 2. page
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

        fa=interpolate(a,ioxf,ioyf,/grid)
	bmax=max(fa) & bmin=min(fa) & del=(bmax-bmin)/16.
        bav=0.5*(bmax+bmin)
        print, 'vecpot:', bmax, bmin, bav
        lvec=findgen(17)*del+bmin & lvec(16)=bav-1.2*del
        lvec=lvec(sort(lvec))
        print, 'lvecs:',lvec
	!P.POSITION=[xa1,ylo,xe1,yup]
	contour,fa,xf,yf,levels=lvec,$
        c_linestyle=lvec lt bav-1.5*del,$ 
        title=' Magnetic Field',xstyle=1,ystyle=1,$
        xtitle='x',ytitle=ytit,thick=1.0

        fn1=interpolate(bx,iox,ioy,/grid)
        fn2=interpolate(by,iox,ioy,/grid)
        fmax=sqrt(max(fn1^2+fn2^2))
	!P.POSITION=[xa1,ylo,xe1,yup]
        vect, fn1, fn2, xn, yn, length=1.3,$
        title=' Magnetic Field'
	xyouts,xpos,ypos3,'Max'
	xyouts,xpos,ypos4,' '+string(fmax,'(f5.2)')

        f1=smooth(bz,5) & fa=interpolate(f1,ioxf,ioyf,/grid) & fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
	bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/10.
	!P.POSITION=[xa2,ylo,xe2,yup]
	contour,fa,xf,yf,levels=findgen(11)*del+bmin,$
        c_linestyle=findgen(11)*del+bmin lt bav,$
        title=' Bz ',xstyle=1,ystyle=1,$
        xtitle=' x',ytickname=names,/noerase
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max=' 
	xyouts,xpos,ypos4,' '+string(bmax,'(f6.2)') 
	xyouts,xpos,ypos5,'Min=' 
	xyouts,xpos,ypos6,' '+string(bmin,'(f6.2)') 

        f1=smooth(bzpro,3) & fa=interpolate(f1,ioxf,ioyf,/grid)
	bmax=max(fa)
	bmin=min(fa)
	bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/10.
	!P.POSITION=[xa3,ylo,xe3,yup]
	contour,fa,xf,yf,levels=findgen(11)*del+bmin,$
        c_linestyle=findgen(11)*del+bmin lt .5,$
        title=' Bz_orig',xstyle=1,ystyle=1,$
        xtitle=' x',ytickname=names
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max=' 
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min=' 
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)') 

  endif

  print, 'continue with next plot, pressure,',$
         ' abs B, and total pressure'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot 3. page
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

        p=2*u^(5.0/3.0)
        fa=interpolate(p,ioxf,ioyf,/grid) & fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa1,ylo,xe1,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
        title=' Pressure',xstyle=1,ystyle=1,$
        xtitle=' x',ytitle=ytit 
	xyouts,xpos,ypos3,'Max' 
	xyouts,xpos,ypos4,''+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min' 
	xyouts,xpos,ypos6,''+string(bmin,'(f5.2)') 

        bsq=sqrt(by*by+bx*bx+bz*bz)
        fa=interpolate(bsq,ioxf,ioyf,/grid)
        fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa2,ylo,xe2,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
        title='Abs(B)',xstyle=1,ystyle=1,$
        xtitle=' x',ytickname=names
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max=' 
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min=' 
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)') 

        p=2*u^(5.0/3.0)
        bsq=bsq*bsq+p
        fa=interpolate(bsq,ioxf,ioyf,/grid) & fa=smooth(fa,3) 
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa3,ylo,xe3,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
        title=' Tot. Pressure',xstyle=1,ystyle=1,$
        xtitle=' x',ytickname=names
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max' 
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min' 
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)') 


  endif

  print, 'continue with next plot, pressure,',$
         ' abs B, and total pressure'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot 4. page
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

        fa=interpolate(a,ioxf,ioyf,/grid)
	bmax=max(fa) & bmin=min(fa) & del=(bmax-bmin)/16.
        bav=0.5*(bmax+bmin)
        print, 'vecpot:', bmax, bmin, bav
        lvec=findgen(17)*del+bmin & lvec(16)=bav-1.2*del
        lvec=lvec(sort(lvec))
        print, 'lvecs:',lvec
	!P.POSITION=[xa1,ylo,xe1,yup]
	contour,fa,xf,yf,levels=lvec,$
        c_linestyle=lvec lt bav-1.5*del,$ 
        ;title=' B Field and Flow',
        xstyle=1,ystyle=1,$
        xtitle='x',ytitle=ytit,thick=1.0

        fn1=interpolate(vx,iox,ioy,/grid)
        fn2=interpolate(vy,iox,ioy,/grid)
        fmax=sqrt(max(fn1^2+fn2^2))
	!P.POSITION=[xa1,ylo,xe1,yup]
        vect, fn1, fn2, xn, yn, length=1.0,$
        title=' B Field and Flow'
	xyouts,xpos,ypos3,'Max'
	xyouts,xpos,ypos4,' '+string(fmax,'(f5.2)')

        fa=interpolate(rho,ioxf,ioyf,/grid) 
	bmax=max(fa) & bmin=min(fa)
        bav=0.5*(bmax+bmin)
        if bmax eq bmin then bmax=bmin+1
        del=(bmax-bmin)/11.
	!P.POSITION=[xa2,ylo,xe2,yup]
	contour,fa,xf,yf,levels=findgen(12)*del+bmin+0.01,$
        c_linestyle=findgen(12)*del+bmin lt 1.,$
        title=' Density !9 r !X',xstyle=1,ystyle=1,$
;        title=' Vz', xstyle=1,ystyle=1,$
        xtitle='x',ytickname=names,/noerase
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(time,'(i3)')
	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max='
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)')
	xyouts,xpos,ypos5,'Min='
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)')

        fa=interpolate(vypro,ioxf,ioyf,/grid) & fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa3,ylo,xe3,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt 0.0,$
        title='Vy_orig',xstyle=1,ystyle=1,$
        xtitle=' x', ytickname=names
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max' 
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min' 
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)') 



  endif

  print, 'continue with next plot, surface rho and ptot'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

        !P.CHARSIZE=2.0
        !P.MULTI=[0,2,0,0,0]
        !P.FONT=-1
        
        p=2*u^(5.0/3.0)
        bsq=bx*bx+by*by+bz*bz
;        bsq=bsq+p
        bsq=bsq
        f1=smooth(bsq,5)
        fa=interpolate(f1,ioxf,ioyf,/grid)
        fb=interpolate(rho,ioxf,ioyf,/grid)
;        fb=smooth(fb,3)

    surface,fb,xf,yf,position=[0.2,0.1,0.45,1.1,0.1,0.5],ax=35,$
      ztitle='rho',xstyle=1,ystyle=1,xtitle='x',ytitle=ytit
    surface,fa,xf,yf,position=[0.6,0.2,0.85,1.2,0.4,0.9],ax=35,$
      ztitle='p_B',xstyle=1,ystyle=1,xtitle='x',ytitle=ytit,/noerase

  endif

  print, 'continue with next plot, bn, and vn'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot 6. page
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]
        !P.FONT=3

        f1=smooth(bx,3) & fa=interpolate(f1,ioxf,ioyf,/grid)
	bmax=max(fa)
	bmin=min(fa)
        if bmax eq bmin then bmax=bmin+1
        del=(bmax-bmin)/15.
	!P.POSITION=[xa1,ylo,xe1,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt 0.0,$
        title=' BN Component',xstyle=1,ystyle=1,$
        xtitle=' x',ytitle=ytit 
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max=' 
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min=' 
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)') 

        f1=smooth(vx,3) & fa=interpolate(f1,ioxf,ioyf,/grid)
	bmax=max(fa)
	bmin=min(fa)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa2,ylo,xe2,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin gt 0.0,$
        title=' VN Component',xstyle=1,ystyle=1,$
        xtitle=' x', ytickname=names
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max=' 
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min=' 
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)') 

        f1=smooth(jz,3) & fa=interpolate(f1,ioxf,ioyf,/grid)
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa3,ylo,xe3,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
        title=' J_z(sim) ',xstyle=1,ystyle=1,$
        xtitle=' x', ytickname=names
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max=' 
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min=' 
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)') 
   endif

  print, 'continue with next plot, surface bn and vn'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

        !P.CHARSIZE=2.0
        !P.MULTI=[0,2,0,0,0]
        !P.FONT=-1
        f1=smooth(bx,5)
        fb=interpolate(f1,ioxf,ioyf,/grid)
        f2=smooth(vx,5)
        fa=interpolate(f2,ioxf,ioyf,/grid)
    surface,fb,xf,yf,position=[0.2,0.1,0.45,1.1,0.1,0.5],ax=35,$
      ztitle='BN',xstyle=1,ystyle=1,xtitle='x',ytitle=ytit
    surface,fa,xf,yf,position=[0.6,0.2,0.85,1.2,0.4,0.9],ax=35,$
      ztitle='VN',xstyle=1,ystyle=1,xtitle='x',ytitle=ytit

  endif

  print, 'continue with next plot, surface jz and ez'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

        !P.CHARSIZE=2.0
        !P.MULTI=[0,2,0,0,0]
        !P.FONT=-1
        f1=smooth(jz,5)
        fb=interpolate(f1,ioxf,ioyf,/grid) & fb=smooth(fb,3)
        f2=smooth(ez,5)
        fa=interpolate(f2,ioxf,ioyf,/grid)
      surface,-fb,xf,yf,position=[0.2,0.1,0.45,1.1,0.1,0.5],ax=35,$
      ztitle='-Current Density',xstyle=1,ystyle=1,$
                                xtitle='x',ytitle=ytit
      surface,fa,xf,yf,position=[0.6,0.2,0.85,1.2,0.4,0.9],ax=35,$
      ztitle='Electric Field',xstyle=1,ystyle=1,$
                              xtitle='x',ytitle=ytit

  endif
     !P.FONT=3


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end

