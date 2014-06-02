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

;----PARAMETER-------
  xmin = -10. & ymin = -0.0
  xmax =  20. & ymax =  160.0
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
   bx=fltarr(nx,ny,/NOZERO) & by=bx & bz=bx & vx=bx & vy=bx & vy=bx
   rho=bx & u=bx & res=bx
   bsq=bx & p=bx & jz=bx & ez=bx & jx=bx & jy=bx
   f1=bx & f2=bx & f3=bx & f4=bx 
   a=fltarr(nx,ny) 

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
;  print, x
;  print, y
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz
   readu, 8,  rho,u,res
   close, 8
   vx=vx/rho &  vz=vz/rho
   vy=vy/rho  
      f1 = shift(bz,0,-1)-shift(bz,0,1)
      for j=1,ny-2 do f3(*,j)=dy(j)*f1(*,j)
      jx = f3
      f2 = shift(bz,-1,0)-shift(bz,1,0)
      for i=1,nx-2 do f4(i,*)=dx(i)*f2(i,*)
      jy = f4
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
   delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
  
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

; CURRENT DENSITY J_Z AND ELECTRIC FIELD E_Z
   f1=shift(by,-1,0)-shift(by,1,0)
   f2=shift(bx,0,-1)-shift(bx,0,1)
   for j=0,ny-1 do f1(*,j) = dx*f1(*,j)
   for i=0,nx-1 do f2(i,*) = dy*f2(i,*)
   jz=f1-f2 & jz(0,*)=0. & jz(nx-1,*)=0. & jz(*,0)=0. & jz(*,ny-1)=0.
   ez=-(vx*by-vy*bx) +res*jz
  
; COORDINATES FOR PLOTS
   srat=1.0
   if (sizeratio lt 3.1) then begin
     dpx=0.21 & dpy=0.282*sizeratio
   endif
   if (sizeratio ge 3.1 and sizeratio le 4.5) then begin
     dpy=0.88 & dpx=0.66/sizeratio
   endif
   if (sizeratio gt 4.5) then begin
     dpy=0.88 & dpx=0.66/4.5
     srat=sizeratio/4.5
   endif
   print, sizeratio, dpx, dpy
;    dpx =0.95*dpx & dpy=0.85*dpy
    dpx =0.18 & dpy=0.8
    xleft=0.06 & xinter=0.12 & 
;    hop=0.47*xinter
    hop=0.*xinter
    xa1=xleft & xe1=xleft+dpx  
    xa2=xe1+xinter & xe2=xa2+dpx
    xa3=xe2+xinter & xe3=xa3+dpx
;    ylo=0.06 & yup=ylo+4.*dpy
    ylo=0.06 & yup=ylo+dpy
   
   xpmin=xmax
   xpmax=xmin
   ytit='z'
     

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
;        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
;        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
       endif

        xpos=xpmax-0.01*delx/dpx
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
       !X.TICKS=3
       !Y.TICKS=8
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
       !X.RANGE=[xpmin,xpmax]
       !Y.RANGE=[ymin,ymax]

        fa=interpolate(a,ioxf,ioyf,/grid)
;	bmax=max(fa,indmax) & bmin=min(fa,indmin) & del=(bmax-bmin)/12.
	bmax=max(fa,indmax) & bmin=min(fa,indmin) & del=(bmax-bmin)/16.
        ixmin=indmin mod nxf
	fmax=max(fa(ixmin,1:nyf-1))
        delf=fmax-fmin
        print, 'magnetic flux:', del
        fb=fmax+5.0
;        fmax=0.5*(fmax+fmin)
;        lvec=findgen(13)*del+bmin & lvec(12)=fb
        lvec=findgen(17)*del+bmin & lvec(16)=fb
        lvec=lvec(sort(lvec))

	!P.POSITION=[xa1,ylo,xe1,yup]
	contour,fa,xf,yf,levels=lvec,$
        c_linestyle=lvec gt fb,$ 
        title=' Magnetic Field',xstyle=1,ystyle=1,$
        xtitle='x',ytitle=ytit
;	xyouts,xpos,ypos0,'time'
;	xyouts,xpos,ypos1,' '+string(time,'(i3)')
;	xyouts,xpos,ypos2,run 
;	xyouts,xpos,ypos3,'DEL A='
;	xyouts,xpos,ypos4,'  '+string(delf,'(f6.2)')

;        fn1=interpolate(vx,iox,ioy,/grid)
;        fn2=interpolate(vy,iox,ioy,/grid)
;        fmax=sqrt(max(fn1^2+fn2^2))
;	!P.POSITION=[xa3,ylo,xe3,yup]
;;        vect, fn1, fn2, xn, yn, length=5.*fmax,$
;;        vect, fn1, fn2, xn, yn, length=0.8*fmax,$
;        vect, fn1, fn2, xn, yn, length=1.2,$
;        title=' Velocity'
;        ;,sizerat=srat
;	xyouts,xpos,ypos0,'time'
;	xyouts,xpos,ypos1,' '+string(time,'(i3)')
;	xyouts,xpos,ypos2,run
;	xyouts,xpos,ypos3,'Max='
;	xyouts,xpos,ypos4,'  '+string(fmax,'(f7.4)')

;        fa=interpolate(rho,ioxf,ioyf,/grid) 
;	bmax=max(fa) & bmin=min(fa)
;        bav=0.5*(bmax+bmin)
;        if bmax eq bmin then bmax=bmin+1
;;        del=(bmax-bmin)/15.
;        del=(bmax-bmin)/11.
;	!P.POSITION=[xa2-hop,ylo,xe2-hop,yup]
;	contour,fa,xf,yf,levels=findgen(12)*del+bmin+0.01,$
;        c_linestyle=findgen(12)*del+bmin lt bav,$
;        title=' Density !9 r !X',xstyle=1,ystyle=1,$
;        xtitle='x',ytickname=names,/noerase
;        ;,ytitle=ytit
;	xyouts,xpos,ypos0,'time'
;	xyouts,xpos,ypos1,' '+string(time,'(i3)')
;	xyouts,xpos,ypos2,run
;	xyouts,xpos,ypos3,'Max='
;	xyouts,xpos,ypos4,'  '+string(bmax,'(f5.2)')
;	xyouts,xpos,ypos5,'Min='
;	xyouts,xpos,ypos6,'  '+string(bmin,'(f5.2)')
  endif

  print, 'continue with next plot, Bz,',$
         '  and overlay current and B lines'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot 2. page
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

        fa=interpolate(a,ioxf,ioyf,/grid)
	bmax=max(fa,indmax) & bmin=min(fa,indmin) & del=(bmax-bmin)/16.
	fb=min(fa(*,0))
        lvec=findgen(17)*del+bmin & lvec(16)=fb+0.01
        lvec=lvec(sort(lvec))
        print, lvec
      
	!P.POSITION=[xa1,ylo,xe1,yup]
	contour,fa,xf,yf,levels=lvec,$
        c_linestyle=lvec lt fb,$ 
        ;title=' B Field and Flow',
        xstyle=1,ystyle=1,$
        xtitle='x',ytitle=ytit,thick=1.0
;	xyouts,xpos,ypos0,'time'
;	xyouts,xpos,ypos1,' '+string(time,'(i3)')
;	xyouts,xpos,ypos2,run 
;	xyouts,xpos,ypos3,'DEL A='
;	xyouts,xpos,ypos4,'  '+string(delf,'(f6.2)')

        fn1=interpolate(jx,iox,ioy,/grid)
        fn2=interpolate(jy,iox,ioy,/grid)
        fmax=sqrt(max(fn1^2+fn2^2))
	!P.POSITION=[xa1,ylo,xe1,yup]
;        vect, fn1, fn2, xn, yn, length=5.*fmax,$
        vect, fn1, fn2, xn, yn, length=1.0,$
;        vect, fn1, fn2, xn, yn, length=1.3*fmax,$
        title=' B Field and current density'
        ;,sizerat=srat
;	xyouts,xpos,ypos0,'time'
;	xyouts,xpos,ypos1,' '+string(time,'(i3)')
;	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max'
	xyouts,xpos,ypos4,' '+string(fmax,'(f7.4)')

        f1=smooth(bz,5)
        fa=interpolate(f1,ioxf,ioyf,/grid) 
	bmax=max(fa) & bmin=min(fa)
        if bmax eq bmin then bmax=bmin+1
        del=(bmax-bmin)/15.
	!P.POSITION=[xa2-hop,ylo,xe2-hop,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt 0.0,$
        title='By Component',xstyle=1,ystyle=1,$
        xtitle='x',ytickname=names,/noerase
        ;,ytitle=ytit
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(time,'(i3)')
	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max'
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)')
	xyouts,xpos,ypos5,'Min'
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)')

        bsq=bx*bx+by*by+bz*bz
        p=2*u^(5.0/3.0)
        bsq=bsq+p
        fa=interpolate(bsq,ioxf,ioyf,/grid) & fa=smooth(fa,3) 
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa3-2*hop,ylo,xe3-2*hop,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
        title=' Tot. Pressure',xstyle=1,ystyle=1,$
        xtitle=' x',ytickname=names
        ;,ytitle=ytit 
        
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max' 
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min' 
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)') 
  endif

  print, 'continue with next plot, B,',$
         '  and overlay V and B lines'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot 2. page
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

        fa=interpolate(a,ioxf,ioyf,/grid)
;	bmax=max(fa,indmax) & bmin=min(fa,indmin) & del=(bmax-bmin)/12.
	bmax=max(fa,indmax) & bmin=min(fa,indmin) & del=(bmax-bmin)/16.
        ixmin=indmin mod nxf
	fb=min(fa(*,0))
;        fmax=0.5*(fmax+fmin)
;        lvec=findgen(13)*del+bmin & lvec(12)=fmax
        lvec=findgen(17)*del+bmin & lvec(16)=fb+0.01
        bav=0.5*(bmax+bmin)
        wwide=2.0 & mmid=bav & ooben=mmid+wwide & uunten=mmid-wwide
        print, bav, mmid
;        lvec(16)=mmid+.05
        lvec=lvec(sort(lvec))
        print, lvec
      
	!P.POSITION=[xa1,ylo,xe1,yup]
	contour,fa,xf,yf,levels=lvec,$
;        c_linestyle=(lvec gt ooben) or (lvec lt uunten),$ 
        c_linestyle=lvec lt fb,$ 
        ;title=' B Field and Flow',
        xstyle=1,ystyle=1,$
        xtitle='x',ytitle=ytit,thick=1.0
;	xyouts,xpos,ypos0,'time'
;	xyouts,xpos,ypos1,' '+string(time,'(i3)')
;	xyouts,xpos,ypos2,run 
;	xyouts,xpos,ypos3,'DEL A='
;	xyouts,xpos,ypos4,'  '+string(delf,'(f6.2)')

        fn1=interpolate(vx,iox,ioy,/grid)
        fn2=interpolate(vy,iox,ioy,/grid)
        fmax=sqrt(max(fn1^2+fn2^2))
	!P.POSITION=[xa1,ylo,xe1,yup]
;        vect, fn1, fn2, xn, yn, length=5.*fmax,$
        vect, fn1, fn2, xn, yn, length=1.0,$
;        vect, fn1, fn2, xn, yn, length=1.3*fmax,$
        title=' B Field and Flow'
        ;,sizerat=srat
;	xyouts,xpos,ypos0,'time'
;	xyouts,xpos,ypos1,' '+string(time,'(i3)')
;	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max'
	xyouts,xpos,ypos4,' '+string(fmax,'(f7.4)')

        f1=smooth(bx,5)
        fa=interpolate(f1,ioxf,ioyf,/grid) 
	bmax=max(fa) & bmin=min(fa)
        if bmax eq bmin then bmax=bmin+1
        del=(bmax-bmin)/15.
	!P.POSITION=[xa2-hop,ylo,xe2-hop,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt 0.0,$
        title='BN Component',xstyle=1,ystyle=1,$
        xtitle='x',ytickname=names,/noerase
        ;,ytitle=ytit
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(time,'(i3)')
	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max'
	xyouts,xpos,ypos4,' '+string(bmax,'(f5.2)')
	xyouts,xpos,ypos5,'Min'
	xyouts,xpos,ypos6,' '+string(bmin,'(f5.2)')

        bsq=bx*bx+by*by+bz*bz
        p=2*u^(5.0/3.0)
        bsq=bsq+p
        fa=interpolate(bsq,ioxf,ioyf,/grid) & fa=smooth(fa,3) 
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa3-2*hop,ylo,xe3-2*hop,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
        title=' Tot. Pressure',xstyle=1,ystyle=1,$
        xtitle=' x',ytickname=names
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
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

        bsq=by*by+2*u^(5.0/3.0)
;        fa=interpolate(bsq,ioxf,ioyf,/grid)
        fa=interpolate(rho,ioxf,ioyf,/grid)
        fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa3,ylo,xe3,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
;        title=' BY**2 + p',xstyle=1,ystyle=1,$
;        title=' !9 r !X',xstyle=1,ystyle=1,$
        title=' Density !9 r !X',xstyle=1,ystyle=1,$
        xtitle=' x',ytitle=ytit 
        
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max=' 
	xyouts,xpos,ypos4,'  '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min=' 
	xyouts,xpos,ypos6,'  '+string(bmin,'(f5.2)') 

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
        
;	xyouts,xpos,ypos0,'time' 
;	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
;	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max' 
	xyouts,xpos,ypos4,''+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min' 
	xyouts,xpos,ypos6,''+string(bmin,'(f5.2)') 

;        f1=smooth(bz,3) & fa=interpolate(f1,ioxf,ioyf,/grid)
        bsq=bx^2+by^2 +bz^2 
;        bsq=by^2 
        fa=interpolate(bsq,ioxf,ioyf,/grid) & fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa2-hop,ylo,xe2-hop,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
        title='B**2',xstyle=1,ystyle=1,$
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

; plot 5. page
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
	xyouts,xpos,ypos4,'  '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min=' 
	xyouts,xpos,ypos6,'  '+string(bmin,'(f5.2)') 

        f1=smooth(vx,3) & fa=interpolate(f1,ioxf,ioyf,/grid)
	bmax=max(fa)
	bmin=min(fa)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa2,ylo,xe2,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin gt 0.0,$
        title=' VN Component',xstyle=1,ystyle=1,$
        xtitle=' x',ytitle=ytit 
        
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max=' 
	xyouts,xpos,ypos4,'  '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min=' 
	xyouts,xpos,ypos6,'  '+string(bmin,'(f5.2)') 

        f1=smooth(by^2,3) & fa=interpolate(f1,ioxf,ioyf,/grid)
	bmax=max(fa)
	bmin=min(fa)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa3,ylo,xe3,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt 0.0,$
        title=' BY**2 ',xstyle=1,ystyle=1,$
        xtitle=' x',ytitle=ytit 
        
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max=' 
	xyouts,xpos,ypos4,'  '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min=' 
	xyouts,xpos,ypos6,'  '+string(bmin,'(f5.2)') 
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
      ztitle='VN',xstyle=1,ystyle=1,xtitle='x',ytitle=ytit,/noerase

  endif

  print, 'continue with next plot, jz and ez'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot 7. page
        !P.CHARSIZE=1.0
        !P.MULTI=[0,2,0,0,0]
        !P.FONT=3

        f1=smooth(jz,5) & fa=interpolate(f1,ioxf,ioyf,/grid) & fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        del=(bmax-bmin)/10.
	!P.POSITION=[xa1,ylo,xe1,yup]
	contour,fa,xf,yf,levels=findgen(11)*del+bmin,$
        c_linestyle=findgen(11)*del+bmin gt 0.0,$
        title=' Current Density, JZ',xstyle=1,ystyle=1,$
        xtitle=' x',ytitle=ytit 
        
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max=' 
	xyouts,xpos,ypos4,'  '+string(bmax,'(f6.2)') 
	xyouts,xpos,ypos5,'Min=' 
	xyouts,xpos,ypos6,'  '+string(bmin,'(f6.2)') 

        f1=smooth(ez,3) & fa=interpolate(f1,ioxf,ioyf,/grid)
	bmax=max(fa)
	bmin=min(fa)
        del=(bmax-bmin)/15.
	!P.POSITION=[xa2,ylo,xe2,yup]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin gt 0.0,$
        title=' Electric Field, EZ',xstyle=1,ystyle=1,$
        xtitle=' x',ytitle=ytit 
        
	xyouts,xpos,ypos0,'time' 
	xyouts,xpos,ypos1,' '+string(time,'(i3)') 
	xyouts,xpos,ypos2,run 
	xyouts,xpos,ypos3,'Max=' 
	xyouts,xpos,ypos4,'  '+string(bmax,'(f5.2)') 
	xyouts,xpos,ypos5,'Min=' 
	xyouts,xpos,ypos6,'  '+string(bmin,'(f5.2)') 
  endif

  print, 'continue with next plot, surface jz and ez'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

        !P.CHARSIZE=2.0
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
                              xtitle='x',ytitle=ytit,/noerase

  endif
     !P.FONT=3


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end

