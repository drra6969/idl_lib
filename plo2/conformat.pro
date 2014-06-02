; START OF MAIN PROGRAM

  nx=99 & ny=303 
  nxn=15 & nyn=21
  nxf=81 & nyf=31
  mx=1 & my=1 
  b2=bytarr(2)
  x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
  g1=fltarr(nx,/NOZERO)
  h1=fltarr(ny,/NOZERO)
  iox=fltarr(nxn) & ioy=fltarr(nyn)
  fn1=fltarr(nxn,nyn) & fn2=fn1 & fn3=fn1 & fn4=fn1 
  ioxf=fltarr(nxf) & ioyf=fltarr(nyf)
  fa=fltarr(nxf,nyf) & fb=fa

  bx=fltarr(nx,ny,/NOZERO)
  by=bx & bz=bx & sx=bx & sy=bx & sz=bx
  rho=bx & u=bx & res=bx & prof=bx
  bsq=bx & p=bx & vz=bx & jz=bx & ez=bx
  f1=bx & f2=bx & f3=bx 
  a=fltarr(nx,ny) 
  time=0.0 & fnumber=1
  name='' & contin='' & again='y' & withps='n' & run=''

  dumm='' & rest= nx mod 9 & nrows=(nx-rest) / 9 
  print, 'nrows= ', nrows, '         rest= ', rest
  xdx=fltarr(2,nx) & ydy=fltarr(2,ny) 
  hf=fltarr(9,ny) & 
  if rest gt 0 then hfrest=fltarr(rest,ny)

  print, 'Input filenumber'
  read, fnumber
  name='magout'+string(fnumber,'(i1)')
  openr, 8, name
  readf, 8, dumm
  readf, 8, mx,my,time
  if (mx ne nx) or (my ne ny) then print, 'nx <> mx =', mx, $
                                '   or     ny <> my =', my
  readf, 8, dumm
  readf, 8, dumm
  readf, 8, xdx
  readf, 8, dumm
  readf, 8, dumm
  readf, 8, ydy
  x=xdx(0,0:nx-1) & g1=xdx(1,0:nx-1)
  y=ydy(0,0:ny-1) & h1=ydy(1,0:ny-1)
;rho
  readf, 8, dumm
  readf, 8, dumm
  for k=0, nrows-1  do begin
    readf, 8, dumm
    readf, 8, hf
    for ix=0,8 do $
     for iy=0,ny-1 do rho(ix+k*9,iy)=hf(ix,iy)
  endfor
  if rest gt 0 then  readf, 8, dumm
;  print, dumm
  if rest gt 0 then  readf, 8, hfrest
  if rest gt 0 then  for ix=0,rest-1 do $
   for iy=0,ny-1 do rho(ix+nrows*9,iy)=hfrest(ix,iy)
  print, 'rho'
;pressure p
  readf, 8, dumm
  readf, 8, dumm
  for k=0, nrows-1  do begin
    readf, 8, dumm
    readf, 8, hf
    for ix=0,8 do $
     for iy=0,ny-1 do u(ix+k*9,iy)=hf(ix,iy)
  endfor
  if rest gt 0 then  readf, 8, dumm
  if rest gt 0 then  readf, 8, hfrest
  if rest gt 0 then  for ix=0,rest-1 do $
   for iy=0,ny-1 do u(ix+nrows*9,iy)=hfrest(ix,iy)
  print, 'p'
;sx
  readf, 8, dumm
  readf, 8, dumm
  for k=0, nrows-1  do begin
    readf, 8, dumm
    readf, 8, hf
    for ix=0,8 do $
     for iy=0,ny-1 do sx(ix+k*9,iy)=hf(ix,iy)
  endfor
  if rest gt 0 then  readf, 8, dumm
  if rest gt 0 then  readf, 8, hfrest
  if rest gt 0 then  for ix=0,rest-1 do $
   for iy=0,ny-1 do sx(ix+nrows*9,iy)=hfrest(ix,iy)
  print, 'sx'
;sy
  readf, 8, dumm
  readf, 8, dumm
  for k=0, nrows-1  do begin
    readf, 8, dumm
    readf, 8, hf
    for ix=0,8 do $
     for iy=0,ny-1 do sy(ix+k*9,iy)=hf(ix,iy)
  endfor
  if rest gt 0 then  readf, 8, dumm
  if rest gt 0 then  readf, 8, hfrest
  if rest gt 0 then  for ix=0,rest-1 do $
   for iy=0,ny-1 do sy(ix+nrows*9,iy)=hfrest(ix,iy)
  print, 'sy'
;sz
  readf, 8, dumm
  readf, 8, dumm
  for k=0, nrows-1  do begin
    readf, 8, dumm
    readf, 8, hf
    for ix=0,8 do $
     for iy=0,ny-1 do sz(ix+k*9,iy)=hf(ix,iy)
  endfor
  if rest gt 0 then  readf, 8, dumm
  if rest gt 0 then  readf, 8, hfrest
  if rest gt 0 then  for ix=0,rest-1 do $
   for iy=0,ny-1 do sz(ix+nrows*9,iy)=hfrest(ix,iy)
  print, 'sz'
;bx
  readf, 8, dumm
  readf, 8, dumm
  for k=0, nrows-1  do begin
    readf, 8, dumm
    readf, 8, hf
    for ix=0,8 do $
     for iy=0,ny-1 do bx(ix+k*9,iy)=hf(ix,iy)
  endfor
  if rest gt 0 then  readf, 8, dumm
  if rest gt 0 then  readf, 8, hfrest
  if rest gt 0 then  for ix=0,rest-1 do $
   for iy=0,ny-1 do bx(ix+nrows*9,iy)=hfrest(ix,iy)
  print, 'bx'
;by
  readf, 8, dumm
  readf, 8, dumm
  for k=0, nrows-1  do begin
    readf, 8, dumm
    readf, 8, hf
    for ix=0,8 do $
     for iy=0,ny-1 do by(ix+k*9,iy)=hf(ix,iy)
  endfor
  if rest gt 0 then  readf, 8, dumm
  if rest gt 0 then  readf, 8, hfrest
  if rest gt 0 then  for ix=0,rest-1 do $
   for iy=0,ny-1 do by(ix+nrows*9,iy)=hfrest(ix,iy)
  print, 'by'
;bz
  readf, 8, dumm
  readf, 8, dumm
  for k=0, nrows-1  do begin
    readf, 8, dumm
    readf, 8, hf
    for ix=0,8 do $
     for iy=0,ny-1 do bz(ix+k*9,iy)=hf(ix,iy)
  endfor
  if rest gt 0 then  readf, 8, dumm
  if rest gt 0 then  readf, 8, hfrest
  if rest gt 0 then  for ix=0,rest-1 do $
   for iy=0,ny-1 do bz(ix+nrows*9,iy)=hfrest(ix,iy)
  print, 'bz'
;res
  readf, 8, dumm
  readf, 8, dumm
  for k=0, nrows-1  do begin
    readf, 8, dumm
    readf, 8, hf
    for ix=0,8 do $
     for iy=0,ny-1 do res(ix+k*9,iy)=hf(ix,iy)
  endfor
  if rest gt 0 then  readf, 8, dumm
;  print, dumm
  if rest gt 0 then  readf, 8, hfrest
  if rest gt 0 then  for ix=0,rest-1 do $
   for iy=0,ny-1 do res(ix+nrows*9,iy)=hfrest(ix,iy)
  print, 'res'

  close, 8

;   print, mx,my,mpos,mrow
;  print, x
;  print, y

;----PARAMETER----
  xmin = -10  &   ymin = 0
  xmax =  10 &   ymax = 150
;  xmin = -14  &   ymin = y(2)
;  xmax =  14  &   ymax = 150  

;  xminn = xmin &  yminn = ymin  ;for interpolated fields(vector)
;  xmaxn = xmax &  ymaxn = ymax   
  xminn = xmin+1 &  yminn = ymin+1  ;for interpolated fields(vector)
  xmaxn = xmax-1 &  ymaxn = ymax-1   
  fall = 'PP'


; generation of new grid for velocity vectors
  xn=findgen(nxn) & yn=findgen(nyn)
  dxn=(xmaxn-xminn)/float(nxn-1)
  xn=xn*dxn+xminn
;  print, dxn,xn
  dyn=(ymaxn-yminn)/float(nyn-1)
  yn=yn*dyn+yminn
;  print, dyn,yn
  in=-1
  k=0
  repeat begin
    in=in+1
    while xn(in) gt x(k+1) do k=k+1
    iox(in) = float(k) + (xn(in)-x(k))/(x(k+1)-x(k)) 
;  print, in, xn(in), k, x(k), iox(in)      
  endrep until in eq nxn-1
  in=-1
  k=0
  repeat begin
    in=in+1
    while yn(in) gt y(k+1) do k=k+1
    ioy(in) = float(k) + (yn(in)-y(k))/(y(k+1)-y(k))        
;    print, in, yn(in), k, y(k), ioy(in)      
  endrep until in eq nyn-1

; generation of new grid for vectorpotential
  xf=findgen(nxf) & yf=findgen(nyf)
  dxf=(xmax-xmin)/float(nxf-1)
  xf=xf*dxf+xmin
;  print, dxf,xf
  dyf=(ymax-ymin)/float(nyf-1)
  yf=yf*dyf+ymin
;  print, dyf,yf
 
  in=-1
  k=0
  repeat begin
    in=in+1
    while xf(in) gt x(k+1) do k=k+1
    ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
;  print, in, xf(in), k, x(k), ioxf(in)      
  endrep until in eq nxf-1
  in=-1
  k=0
  repeat begin
    in=in+1
    while yf(in) gt y(k+1) do k=k+1
    ioyf(in) = float(k) + (yf(in)-y(k))/(y(k+1)-y(k))        
;    print, in, ynf(in), k, y(k), ioyf(in)      
  endrep until in eq nyf-1

; Computation of Vectorpotential a:
    a=0*bx
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
    print, fmax
    print, fmin

; Computation of current density jz and e field ez

  f1=shift(by,-1,0)-shift(by,1,0)
  f2=shift(bx,0,-1)-shift(bx,0,1)
  for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
  for i=0,nx-1 do f2(i,*) = h1*f2(i,*)
  jz=f1-f2 
  jz(0,*)=0. & jz(nx-1,*)=0. & jz(*,0)=0. &jz(*,ny-1)=0.
  ez=-(sx*by-sy*bx)/rho +res*jz

    print, 'Which case?'
    read, run
    fall=run
;    fall='run:'+run

     while (again eq 'y') do begin

      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif

        xpos=xmax+0.05*(xmax-xmin)
        ypos1=ymin+.4*(ymax-ymin)
        ypos2=ymin+.6*(ymax-ymin)
        ypos3=ymin+.75*(ymax-ymin)
        ypos4=ymin+.72*(ymax-ymin)
        ypos5=ymin+.9*(ymax-ymin)
        ypos6=ymin+.87*(ymax-ymin)
        xpost=xmin+.2*(xmax-xmin)
        ypost=ymax+.01*(ymax-ymin)
       
  print, 'plot first page? Magnetic field, flow vectors'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot first page
       !P.REGION=[0.,0.,1.0,1.25]
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0

;        a=smooth(a,4)
        fa=interpolate(a,ioxf,ioyf,/grid)
	bmax=max(fa)
	bmin=min(fa)
        del=(bmax-bmin)/15.
	fmax=max(fa((nxf-1)/2,1:nyf-1))
	fmin=min(fa((nxf-1)/2,1:nyf-1))
        delf=fmax-fmin
        lvec=findgen(17)*del+bmin
        lvec(16)=fmax
        lvec=lvec(sort(lvec))

	!P.POSITION=[0.1,0.1,0.28,0.65]
	contour,fa,xf,yf,levels=lvec,$
        c_linestyle=lvec lt fmax,$ 
        xrange=[xmin,xmax],yrange=[ymin,ymax],$
        title='!7 Magnetic Field',xstyle=1,ystyle=1,$
        xtitle='x',ytitle='y',font=2
	xyouts,xpos,ypos1,'t='+string(time,'(i3)'),font=2
	xyouts,xpos,ypos2,fall,font=2
	xyouts,xpos,ypos3,'DEL A=',font=2
	xyouts,xpos,ypos4,'  '+string(delf,'(f6.2)'),font=2

        fn1=interpolate(sx,iox,ioy,/grid)
        fn2=interpolate(sy,iox,ioy,/grid)
        fn3=interpolate(rho,iox,ioy,/grid)
        fn1=fn1/fn3
        fn2=fn2/fn3+0.
        fmax=sqrt(max(fn1^2+fn2^2))
	!P.POSITION=[0.4,0.1,0.58,0.65]
        velovect, fn1, fn2, xn, yn, length=1.5,$
        title='!5       Velocity'
	xyouts,xpos,ypos3,'Max=',font=2
	xyouts,xpos,ypos4,'  '+string(fmax,'(f6.3)'),font=2

        fa=interpolate(bx,ioxf,ioyf,/grid)
        fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        if bmax eq bmin then bmax=bmin+1
        del=(bmax-bmin)/15.
	!P.POSITION=[0.7,0.1,0.88,0.65]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt 0.0,$
        xrange=[xmin,xmax],yrange=[ymin,ymax],$
        title='!7 BN Component',xstyle=1,ystyle=1,$
        xtitle='!7 x',ytitle='!7 y',font=2
        
	xyouts,xpos,ypos1,'t='+string(time,'(i3)'),font=2
	xyouts,xpos,ypos2,fall,font=2
	xyouts,xpos,ypos3,'Max=',font=2
	xyouts,xpos,ypos4,'  '+string(bmax,'(f6.3)'),font=2
	xyouts,xpos,ypos5,'Min=',font=2
	xyouts,xpos,ypos6,'  '+string(bmin,'(f6.3)'),font=2
  endif

  print, 'continue with next plot, density,',$
         ' pressure, and total pressure'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot second page
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

        fa=interpolate(rho,ioxf,ioyf,/grid)
        fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	!P.POSITION=[0.05,0.1,0.25,0.75]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
        xrange=[xmin,xmax],yrange=[ymin,ymax],$
        title='!7 Density RHO',xstyle=1,ystyle=1,$
        xtitle='!7 x',ytitle='!7 y',font=2
        
	xyouts,xpos,ypos1,'time='+string(time,'(i3)'),font=2
	xyouts,xpos,ypos2,fall,font=2
	xyouts,xpos,ypos3,'Max=',font=2
	xyouts,xpos,ypos4,'  '+string(bmax,'(f6.3)'),font=2
	xyouts,xpos,ypos5,'Min=',font=2
	xyouts,xpos,ypos6,'  '+string(bmin,'(f6.3)'),font=2

        p=2*u^(5.0/3.0)
        fa=interpolate(p,ioxf,ioyf,/grid)
;        fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	!P.POSITION=[0.35,0.1,0.55,0.75]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
        xrange=[xmin,xmax],yrange=[ymin,ymax],$
        title='!7 Pressure',xstyle=1,ystyle=1,$
        xtitle='!7 x',ytitle='!7 y',font=2
        
	xyouts,xpos,ypos1,'time='+string(time,'(i3)'),font=2
	xyouts,xpos,ypos2,fall,font=2
	xyouts,xpos,ypos3,'Max=',font=2
	xyouts,xpos,ypos4,'  '+string(bmax,'(f6.3)'),font=2
	xyouts,xpos,ypos5,'Min=',font=2
	xyouts,xpos,ypos6,'  '+string(bmin,'(f6.3)'),font=2

        bsq=bx*bx+by*by+bz*bz
        bsq=bsq+p
        fa=interpolate(bsq,ioxf,ioyf,/grid)
;        fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/15.
	!P.POSITION=[0.65,0.1,0.85,0.75]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt bav,$
        xrange=[xmin,xmax],yrange=[ymin,ymax],$
        title='!7 Tot. Pressure',xstyle=1,ystyle=1,$
        xtitle='!7 x',ytitle='!7 y',font=2
        
	xyouts,xpos,ypos1,'time='+string(time,'(i3)'),font=2
	xyouts,xpos,ypos2,fall,font=2
	xyouts,xpos,ypos3,'Max=',font=2
	xyouts,xpos,ypos4,'  '+string(bmax,'(f6.3)'),font=2
	xyouts,xpos,ypos5,'Min=',font=2
	xyouts,xpos,ypos6,'  '+string(bmin,'(f6.3)'),font=2
  endif

  print, 'continue with next plot, surface rho and ptot'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

        bsq=bx*bx+by*by+bz*bz
        bsq=bsq+p
        fa=interpolate(bsq,ioxf,ioyf,/grid)
        fb=interpolate(rho,ioxf,ioyf,/grid)
;        fb=smooth(fb,3)

    surface,fb,xf,yf,position=[0.2,0.1,0.45,1.1,0.1,0.5],ax=35,$
      ztitle='rho',xstyle=1,ystyle=1,xtitle='x',ytitle='y'
    surface,fa,xf,yf,position=[0.6,0.2,0.85,1.2,0.4,0.9],ax=35,$
      ztitle='ptot',xstyle=1,ystyle=1,xtitle='x',ytitle='y',/noerase

  endif

  print, 'continue with next plot, jz, bn, and vn'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot third page
        !P.CHARSIZE=1.0
        !P.MULTI=[0,2,0,0,0]

        fa=interpolate(bx,ioxf,ioyf,/grid)
        fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        if bmax eq bmin then bmax=bmin+1
        del=(bmax-bmin)/15.
	!P.POSITION=[0.15,0.1,0.35,0.75]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt 0.0,$
        xrange=[xmin,xmax],yrange=[ymin,ymax],$
        title='!7 BN Component',xstyle=1,ystyle=1,$
        xtitle='!7 x',ytitle='!7 y',font=2
        
	xyouts,xpos,ypos1,'time='+string(time,'(i3)'),font=2
	xyouts,xpos,ypos2,fall,font=2
	xyouts,xpos,ypos3,'Max=',font=2
	xyouts,xpos,ypos4,'  '+string(bmax,'(f6.3)'),font=2
	xyouts,xpos,ypos5,'Min=',font=2
	xyouts,xpos,ypos6,'  '+string(bmin,'(f6.3)'),font=2

        f1=sx/rho
        fa=interpolate(f1,ioxf,ioyf,/grid)
        fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        del=(bmax-bmin)/15.
	!P.POSITION=[0.45,0.1,0.65,0.75]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt 0.0,$
        xrange=[xmin,xmax],yrange=[ymin,ymax],$
        title='!7 VN Component',xstyle=1,ystyle=1,$
        xtitle='!7 x',ytitle='!7 y',font=2
        
	xyouts,xpos,ypos1,'time='+string(time,'(i3)'),font=2
	xyouts,xpos,ypos2,fall,font=2
	xyouts,xpos,ypos3,'Max=',font=2
	xyouts,xpos,ypos4,'  '+string(bmax,'(f6.3)'),font=2
	xyouts,xpos,ypos5,'Min=',font=2
	xyouts,xpos,ypos6,'  '+string(bmin,'(f6.3)'),font=2
   endif

  print, 'continue with next plot, surface bn and vn'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

        fb=interpolate(bx,ioxf,ioyf,/grid)
        fb=smooth(fb,3)
        f1=sx/rho
        fa=interpolate(f1,ioxf,ioyf,/grid)
        fa=smooth(fa,3)
    surface,-fb,xf,yf,position=[0.2,0.1,0.45,1.1,0.1,0.5],ax=35,$
      ztitle='-BN',xstyle=1,ystyle=1,xtitle='x',ytitle='y'
    surface,-fa,xf,yf,position=[0.6,0.2,0.85,1.2,0.4,0.9],ax=35,$
      ztitle='-VN',xstyle=1,ystyle=1,xtitle='x',ytitle='y',/noerase

  endif

  print, 'continue with next plot, jz and ez'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot fourth page
        !P.CHARSIZE=1.0
        !P.MULTI=[0,2,0,0,0]

        fa=interpolate(jz,ioxf,ioyf,/grid)
;        fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        del=(bmax-bmin)/15.
	!P.POSITION=[0.15,0.1,0.35,0.75]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin gt -0.5,$
        xrange=[xmin,xmax],yrange=[ymin,ymax],$
        title='!7 Current Density, JZ',xstyle=1,ystyle=1,$
        xtitle='!7 x',ytitle='!7 y',font=2
        
	xyouts,xpos,ypos1,'time='+string(time,'(i3)'),font=2
	xyouts,xpos,ypos2,fall,font=2
	xyouts,xpos,ypos3,'Max=',font=2
	xyouts,xpos,ypos4,'  '+string(bmax,'(f6.3)'),font=2
	xyouts,xpos,ypos5,'Min=',font=2
	xyouts,xpos,ypos6,'  '+string(bmin,'(f6.3)'),font=2

        fa=interpolate(ez,ioxf,ioyf,/grid)
        fa=smooth(fa,3)
	bmax=max(fa)
	bmin=min(fa)
        del=(bmax-bmin)/15.
	!P.POSITION=[0.45,0.1,0.65,0.75]
	contour,fa,xf,yf,levels=findgen(16)*del+bmin,$
        c_linestyle=findgen(16)*del+bmin lt 0.0,$
        xrange=[xmin,xmax],yrange=[ymin,ymax],$
        title='!7 Electric Field, EZ',xstyle=1,ystyle=1,$
        xtitle='!7 x',ytitle='!7 y',font=2
        
	xyouts,xpos,ypos1,'time='+string(time,'(i3)'),font=2
	xyouts,xpos,ypos2,fall,font=2
	xyouts,xpos,ypos3,'Max=',font=2
	xyouts,xpos,ypos4,'  '+string(bmax,'(f6.3)'),font=2
	xyouts,xpos,ypos5,'Min=',font=2
	xyouts,xpos,ypos6,'  '+string(bmin,'(f6.3)'),font=2
  endif

  print, 'continue with next plot, surface jz and ez'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

        fb=interpolate(jz,ioxf,ioyf,/grid)
        fb=smooth(fb,3)

    surface,-fb,xf,yf,position=[0.2,0.1,0.45,1.1,0.1,0.5],ax=35,$
      ztitle='-Current Density',xstyle=1,ystyle=1,$
                                xtitle='x',ytitle='y'
    surface,fa,xf,yf,position=[0.6,0.2,0.85,1.2,0.4,0.9],ax=35,$
      ztitle='Electric Field',xstyle=1,ystyle=1,$
                              xtitle='x',ytitle='y',/noerase

  endif


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile

end

