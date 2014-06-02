; START OF MAIN PROGRAM

  nx=23 & ny=23  & nz=23 
  nxn=15 & nyn=21 & nzn=21
  nxf=81 & nyf=51 & nzf=51
  mx=23 & my=23  & mz=23 & mpos=1 & mrow=1 
  b2=bytarr(2)
  b1=bytarr(1)
  x=fltarr(nx,/NOZERO) & dx=x & dxh=x
  y=fltarr(ny,/NOZERO) & dy=y & dyh=y
  z=fltarr(nz,/NOZERO) & dz=z & dzh=z 
  bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 

  iox=fltarr(nxn) & ioy=fltarr(nyn) & ioz=fltarr(nzn)
  fn1=fltarr(nxn,nyn,nzn) & fn2=fn1 & fn3=fn1 & fn4=fn1 
  ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
  fa=fltarr(nxf,nyf,nzf) & fb=fa

  time=0.0 & fnumber=1
  name='' & contin='' & again='y' & withps='n' & run=''

  print, 'Input filenumber'
  read, fnumber
  name='magveb'+string(fnumber,'(i1)')
  openr, 8, name,/F77_UNFORMATTED

  readu, 8,  mx,b2,my,b2,mz,b2,time
  print, mx,my,mz,time
  readu, 8,  x,dx,dxh,dxh,dxh,dxh,dxh,y,dy,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
;  print, x,y,z
  readu, 8,  bx,by,bz
  print, mx,my,mz,time
  print, 'xmin=', x(1), '  xmax=', x(nx-2)
  print, 'ymin=', y(1), '  ymax=', y(ny-2)
  print, 'zmin=', z(1), '  zmax=', z(nz-2)
  close, 8

  amin=min(bx) & amax=max(bx)
  thresh=amin+0.5*(amax-amin)
  print,'bx:',amin,amax,thresh
  amin=min(by) & amax=max(by)
  thresh=amin+0.5*(amax-amin)
  print,'by:',amin,amax,thresh
  amin=min(bz) & amax=max(bz)
  thresh=amin+0.5*(amax-amin)
  print,'bz:',amin,amax,thresh
;  print, x
;  print, y
;  print, z

;----PARAMETER----
  xmin =  0 & ymin =  0  & zmin =  0
  xmax = 10 & ymax = 10  & zmax = 10
;  xmin = -14  &  ymin = y(1)
;  xmax =  14  &  ymax = 150  

;for interpolated fields(vector)
  xminn = xmin & yminn = ymin+1  & zminn = zmin+1  
  xmaxn = xmax & ymaxn = ymax-1  & zmaxn = zmax-1  
;  xminn = xmin+1 &  yminn = ymin+1  
;  xmaxn = xmax-1 &  ymaxn = ymax-1   
  fall = 'PP'


; generation of new grid for velocity vectors
  xn=findgen(nxn) & yn=findgen(nyn) & zn=findgen(nzn)
  dxn=(xmaxn-xminn)/float(nxn-1)
  xn=xn*dxn+xminn
;  print, dxn,xn
  dyn=(ymaxn-yminn)/float(nyn-1)
  yn=yn*dyn+yminn
;  print, dyn,yn
  dzn=(zmaxn-zminn)/float(nzn-1)
  zn=zn*dzn+zminn
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
  in=-1
  k=0
  repeat begin
    in=in+1
    while zn(in) gt z(k+1) do k=k+1
    ioz(in) = float(k) + (zn(in)-z(k))/(z(k+1)-z(k))        
;    print, in, zn(in), k, z(k), ioz(in)      
  endrep until in eq nzn-1

; generation of new grid for vectorpotential
  xf=findgen(nxf) & yf=findgen(nyf) & zf=findgen(nzf)
  dxf=(xmax-xmin)/float(nxf-1)
  xf=xf*dxf+xmin
;  print, dxf,xf
  dyf=(ymax-ymin)/float(nyf-1)
  yf=yf*dyf+ymin
;  print, dyf,yf
  dzf=(zmax-zmin)/float(nzf-1)
  zf=zf*dzf+zmin
;  print, dzf,zf
 
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
  in=-1
  k=0
  repeat begin
    in=in+1
    while zf(in) gt z(k+1) do k=k+1
    iozf(in) = float(k) + (zf(in)-z(k))/(z(k+1)-z(k))        
;    print, in, znf(in), k, z(k), iozf(in)      
  endrep until in eq nzf-1

  s=size(bx)
  if s(0) ne 3 then write, 'fehler'
  amin=min(bx*bx) & amax=max(bx*bx)
  thresh=amin+0.1*(amax-amin)
  print,amin,amax,thresh
  surface, fltarr(2,2), /nodata, /save, $
    xrange=[0,s(1)-1],yrange=[0,s(2)-1],zrange=[0,s(3)-1]
;  if n_elements(low) eq 0 then low=0
  shade_volume, bx*bx, thresh, v, p;, low=low
  tv, polyshade(v,p,/t3d)
  surface, fltarr(2,2), /nodata, /save, /noerase,$
    ztitle='z',xtitle='x',ytitle='y',$
    xrange=[0,s(1)-1],yrange=[0,s(2)-1],zrange=[0,s(3)-1]

;  read, contin
;  amin=min(by) & amax=max(by)
;  thresh=amin+0.2*(amax-amin)
;  print,amin,amax,thresh
;;  surface, fltarr(2,2), /nodata, /save, $
;;    xrange=[0,s(1)-1],yrange=[0,s(2)-1],zrange=[0,s(3)-1]
;;  if n_elements(low) eq 0 then low=0
;  shade_volume, by, thresh, v, p;, low=low
;  tv, polyshade(v,p,/t3d)
;  surface, fltarr(2,2), /nodata, /save, /noerase,$
;    ztitle='z',xtitle='x',ytitle='y',$
;    xrange=[0,s(1)-1],yrange=[0,s(2)-1],zrange=[0,s(3)-1]

;  read, contin
;  amin=min(bz) & amax=max(bz)
;  thresh=0.0
;  print,amin,amax,thresh
;;  surface, fltarr(2,2), /nodata, /save, $
;;    xrange=[0,s(1)-1],yrange=[0,s(2)-1],zrange=[0,s(3)-1]
;;  if n_elements(low) eq 0 then low=0
;  shade_volume, bz, thresh, v, p;, low=low
;  tv, polyshade(v,p,/t3d)
;  surface, fltarr(2,2), /nodata, /save, /noerase,$
;    ztitle='z',xtitle='x',ytitle='y',$
;    xrange=[0,s(1)-1],yrange=[0,s(2)-1],zrange=[0,s(3)-1]

  nl=10  & maxnstep=501 & nltot=2*nl
  xl=findgen(nl) & yl=xl & zl=xl
  xin=0.5 & yin=0.5 & zin=0.5 
  dxin=0.5 & dyin=0.5 & dzin=0.5
  xl=xl*dxin+xin & yl=yl*dyin+yin & zl=zl*dzin+zin 
  dels=0.2 & del=fltarr(nltot)
  xline=fltarr(nltot,maxnstep) & yline=xline & zline=xline
  ixl=intarr(nltot) & iyl=intarr(nltot) & izl=intarr(nltot) 
  dxl=fltarr(nltot) & dyl=dxl & dzl=dxl 
  hxo=dxl & hyo=dxl & hzo=dxl & hxu=dxl & hyu=dxl & hzu=dxl 
  bxl=dxl & byl=dxl & bzl=dxl
  nstep=intarr(nltot) & inarea=intarr(nltot)
  for i=0,nl-1 do begin 
    xline(i,0)=xl(i) & xline(nl+i,0)=xl(i) & endfor
  for i=0,nl-1 do begin 
    yline(i,0)=yl(i) & yline(nl+i,0)=yl(i) & endfor
  for i=0,nl-1 do begin 
    zline(i,0)=zl(i) & zline(nl+i,0)=zl(i) & endfor
  for i=0,nl-1 do del(i)=dels
  for i=nl,nltot-1 do del(i)=-dels
  for i=0,nltot-1 do begin & nstep(i)=0 & inarea(i)=1 & endfor

  xl=xline(*,0) & yl=yline(*,0) & zl=zline(*,0) 

  ks=0 
  for i=0,nltot-1 do $
    if ( (xl(i) lt xmin) or (xl(i) gt xmax) $
      or (yl(i) lt ymin) or (yl(i) gt ymax) $
      or (zl(i) lt zmin) or (zl(i) gt zmax) ) then inarea(i)=0
  inatot=total(inarea)
  ks=ks+1
  nstep=nstep+inarea
  print,'nu gehts los'

  while (ks lt maxnstep and inatot gt 0) do begin

   for i=0,nltot-1 do $
    if (inarea(i) eq 1) then begin
      l=nx-1 & r=2
      while (r le l) do begin
        k=(l+r)/2
        if (xl(i) le x(k)) then l=k-1
        if (xl(i) ge x(k)) then r=k+1
      endwhile
      if (r-l eq 2) then ixl(i)=l+1 else ixl(i)=l 
;      print, 'i/ks=',i,ks, '  ixl',ixl(i), xl(i),x(ixl(i)),x(ixl(i)+1)
      l=ny-1 & r=2
      while (r le l) do begin
        k=(l+r)/2
        if (yl(i) le y(k)) then l=k-1
        if (yl(i) ge y(k)) then r=k+1
      endwhile
      if (r-l eq 2) then iyl(i)=l+1 else iyl(i)=l 
      l=nz-1 & r=2
      while (r le l) do begin
        k=(l+r)/2
        if (zl(i) le z(k)) then l=k-1
        if (zl(i) ge z(k)) then r=k+1
      endwhile
      if (r-l eq 2) then izl(i)=l+1 else izl(i)=l 
      dxl(i) = x(ixl(i)+1)-x(ixl(i))
      dyl(i) = y(iyl(i)+1)-y(iyl(i))
      dzl(i) = z(izl(i)+1)-z(izl(i))
      hxo(i) = (xl(i)-x(ixl(i))) / dxl(i)
      hyo(i) = (yl(i)-y(iyl(i))) / dyl(i)
      hzo(i) = (zl(i)-z(izl(i))) / dzl(i)
;      hxu(i) = 1.0 - hxo(i)
;      hyu(i) = 1.0 - hyo(i)
;      hzu(i) = 1.0 - hzo(i)
;      bxl(i)=hxu(i) * hyu(i) * hzu(i) * bx(ixl(i),iyl(i),izl(i)) $
;         + hxu(i) * hyu(i) * hzo(i) * bx(ixl(i),iyl(i),izl(i)+1) $
;         + hxu(i) * hyo(i) * hzu(i) * bx(ixl(i),iyl(i)+1,izl(i)) $
;         + hxu(i) * hyo(i) * hzo(i) * bx(ixl(i),iyl(i)+1,izl(i)+1) $
;         + hxo(i) * hyu(i) * hzu(i) * bx(ixl(i)+1,iyl(i),izl(i)) $
;         + hxo(i) * hyu(i) * hzo(i) * bx(ixl(i)+1,iyl(i),izl(i)+1) $
;         + hxo(i) * hyo(i) * hzu(i) * bx(ixl(i)+1,iyl(i)+1,izl(i)) $
;         + hxo(i) * hyo(i) * hzo(i) * bx(ixl(i)+1,iyl(i)+1,izl(i)+1) 
;      byl(i)=hxu(i) * hyu(i) * hzu(i) * by(ixl(i),iyl(i),izl(i)) $
;         + hxu(i) * hyu(i) * hzo(i) * by(ixl(i),iyl(i),izl(i)+1) $
;         + hxu(i) * hyo(i) * hzu(i) * by(ixl(i),iyl(i)+1,izl(i)) $
;         + hxu(i) * hyo(i) * hzo(i) * by(ixl(i),iyl(i)+1,izl(i)+1) $
;         + hxo(i) * hyu(i) * hzu(i) * by(ixl(i)+1,iyl(i),izl(i)) $
;         + hxo(i) * hyu(i) * hzo(i) * by(ixl(i)+1,iyl(i),izl(i)+1) $
;         + hxo(i) * hyo(i) * hzu(i) * by(ixl(i)+1,iyl(i)+1,izl(i)) $
;         + hxo(i) * hyo(i) * hzo(i) * by(ixl(i)+1,iyl(i)+1,izl(i)+1) 
;      bzl(i)=hxu(i) * hyu(i) * hzu(i) * bz(ixl(i),iyl(i),izl(i)) $
;         + hxu(i) * hyu(i) * hzo(i) * bz(ixl(i),iyl(i),izl(i)+1) $
;         + hxu(i) * hyo(i) * hzu(i) * bz(ixl(i),iyl(i)+1,izl(i)) $
;         + hxu(i) * hyo(i) * hzo(i) * bz(ixl(i),iyl(i)+1,izl(i)+1) $
;         + hxo(i) * hyu(i) * hzu(i) * bz(ixl(i)+1,iyl(i),izl(i)) $
;         + hxo(i) * hyu(i) * hzo(i) * bz(ixl(i)+1,iyl(i),izl(i)+1) $
;         + hxo(i) * hyo(i) * hzu(i) * bz(ixl(i)+1,iyl(i)+1,izl(i)) $
;         + hxo(i) * hyo(i) * hzo(i) * bz(ixl(i)+1,iyl(i)+1,izl(i)+1) 
;      print, i, bxl(i),byl(i),bzl(i)
    endif
      rixl=float(ixl)+hxo
      riyl=float(iyl)+hyo
      rizl=float(izl)+hzo
      bxl=interpolate(bx,rixl,riyl,rizl)
      byl=interpolate(by,rixl,riyl,rizl)
      bzl=interpolate(bz,rixl,riyl,rizl)
;   for i=0,nltot-1 do print, i, bxl(i),byl(i),bzl(i)


   babs=sqrt(bxl*bxl+byl*byl+bzl*bzl)
   xl=xl+0.5*float(inarea)*del*bxl/babs
   yl=yl+0.5*float(inarea)*del*byl/babs
   zl=zl+0.5*float(inarea)*del*bzl/babs

   for i=0,nltot-1 do $
      if ( xl(i) lt xmin or xl(i) gt xmax $
        or yl(i) lt ymin or yl(i) gt ymax $
        or zl(i) lt zmin or zl(i) gt zmax) then inarea(i)=0
   inatot=total(inarea)

   for i=0,nltot-1 do $
    if (inarea(i) eq 1) then begin
      l=nx-1 & r=2
      while (r le l) do begin
        k=(l+r)/2
        if (xl(i) le x(k)) then l=k-1
        if (xl(i) ge x(k)) then r=k+1
      endwhile
      if (r-l eq 2) then ixl(i) = l + 1 else ixl(i) = l 
      l=ny-1 & r=2
      while (r le l) do begin
        k=(l+r)/2
        if (yl(i) le y(k)) then l=k-1
        if (yl(i) ge y(k)) then r=k+1
      endwhile
      if (r-l eq 2) then iyl(i) = l + 1 else iyl(i) = l 
      l=nz-1 & r=2
      while (r le l) do begin
        k=(l+r)/2
        if (zl(i) le z(k)) then l=k-1
        if (zl(i) ge z(k)) then r=k+1
      endwhile
      if (r-l eq 2) then izl(i) = l + 1 else izl(i) = l 
      dxl(i) = x(ixl(i)+1)-x(ixl(i))
      dyl(i) = y(iyl(i)+1)-y(iyl(i))
      dzl(i) = z(izl(i)+1)-z(izl(i))
      hxo(i) = (xl(i)-x(ixl(i))) / dxl(i)
      hyo(i) = (yl(i)-y(iyl(i))) / dyl(i)
      hzo(i) = (zl(i)-z(izl(i))) / dzl(i)
;      hxu(i) = 1.0 - hxo(i)
;      hyu(i) = 1.0 - hyo(i)
;      hzu(i) = 1.0 - hzo(i)
;      bxl(i)=hxu(i) * hyu(i) * hzu(i) * bx(ixl(i),iyl(i),izl(i)) $
;         + hxu(i) * hyu(i) * hzo(i) * bx(ixl(i),iyl(i),izl(i)+1) $
;         + hxu(i) * hyo(i) * hzu(i) * bx(ixl(i),iyl(i)+1,izl(i)) $
;         + hxu(i) * hyo(i) * hzo(i) * bx(ixl(i),iyl(i)+1,izl(i)+1) $
;         + hxo(i) * hyu(i) * hzu(i) * bx(ixl(i)+1,iyl(i),izl(i)) $
;         + hxo(i) * hyu(i) * hzo(i) * bx(ixl(i)+1,iyl(i),izl(i)+1) $
;         + hxo(i) * hyo(i) * hzu(i) * bx(ixl(i)+1,iyl(i)+1,izl(i)) $
;         + hxo(i) * hyo(i) * hzo(i) * bx(ixl(i)+1,iyl(i)+1,izl(i)+1) 
;      byl(i)=hxu(i) * hyu(i) * hzu(i) * by(ixl(i),iyl(i),izl(i)) $
;         + hxu(i) * hyu(i) * hzo(i) * by(ixl(i),iyl(i),izl(i)+1) $
;         + hxu(i) * hyo(i) * hzu(i) * by(ixl(i),iyl(i)+1,izl(i)) $
;         + hxu(i) * hyo(i) * hzo(i) * by(ixl(i),iyl(i)+1,izl(i)+1) $
;         + hxo(i) * hyu(i) * hzu(i) * by(ixl(i)+1,iyl(i),izl(i)) $
;         + hxo(i) * hyu(i) * hzo(i) * by(ixl(i)+1,iyl(i),izl(i)+1) $
;         + hxo(i) * hyo(i) * hzu(i) * by(ixl(i)+1,iyl(i)+1,izl(i)) $
;         + hxo(i) * hyo(i) * hzo(i) * by(ixl(i)+1,iyl(i)+1,izl(i)+1)
;      bzl(i)=hxu(i) * hyu(i) * hzu(i) * bz(ixl(i),iyl(i),izl(i)) $
;         + hxu(i) * hyu(i) * hzo(i) * bz(ixl(i),iyl(i),izl(i)+1) $
;         + hxu(i) * hyo(i) * hzu(i) * bz(ixl(i),iyl(i)+1,izl(i)) $
;         + hxu(i) * hyo(i) * hzo(i) * bz(ixl(i),iyl(i)+1,izl(i)+1) $
;         + hxo(i) * hyu(i) * hzu(i) * bz(ixl(i)+1,iyl(i),izl(i)) $
;         + hxo(i) * hyu(i) * hzo(i) * bz(ixl(i)+1,iyl(i),izl(i)+1) $
;         + hxo(i) * hyo(i) * hzu(i) * bz(ixl(i)+1,iyl(i)+1,izl(i)) $
;         + hxo(i) * hyo(i) * hzo(i) * bz(ixl(i)+1,iyl(i)+1,izl(i)+1) 
    endif
      rixl=float(ixl)+hxo
      riyl=float(iyl)+hyo
      rizl=float(izl)+hzo
      bxl=interpolate(bx,rixl,riyl,rizl)
      byl=interpolate(by,rixl,riyl,rizl)
      bzl=interpolate(bz,rixl,riyl,rizl)

   babs=sqrt(bxl*bxl+byl*byl+bzl*bzl)
   xline(*,ks)=xline(*,ks-1)+float(inarea)*del*bxl/babs
   yline(*,ks)=yline(*,ks-1)+float(inarea)*del*byl/babs
   zline(*,ks)=zline(*,ks-1)+float(inarea)*del*bzl/babs
   xl(*)=xline(*,ks) & yl(*)=yline(*,ks) & zl(*)=zline(*,ks) 

   for i=0,nltot-1 do $
      if ( xl(i) lt xmin or xl(i) gt xmax $
        or yl(i) lt ymin or yl(i) gt ymax $
        or yl(i) lt ymin or yl(i) gt ymax) then inarea(i)=0
   inatot=total(inarea)
   ks=ks+1
   nstep=nstep+inarea
  endwhile
;  for j=0,nltot-1 do $
;  for i=0,nstep(j) do print, j, i, xline(j,i), yline(j,i), zline(j,i)
  for i=0,nltot-1 do print, i, nstep(i)




end


