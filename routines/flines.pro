PRO FLINES, BX, BY, BZ, X, Y, Z, XIN, YIN, ZIN, $ 
            XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, DELS, $
            xline, yline, zline, nstep, inatot


  maxnstep=801
  s=size(bx)
  nx=s(1) & ny=s(2)  & nz=s(3) 
  t=size(xin)
  nl=t(1) & nltot=2*nl
  print, nl, nltot
  ixl=intarr(nltot) & iyl=ixl & izl=iyl
  del=fltarr(nltot)
  xline=fltarr(nltot,maxnstep) & yline=xline & zline=xline
  nstep=intarr(nltot) & inarea=intarr(nltot)
  for i=0,nl-1 do begin 
    xline(i,0)=xin(i) & xline(nl+i,0)=xin(i) & endfor
  for i=0,nl-1 do begin 
    yline(i,0)=yin(i) & yline(nl+i,0)=yin(i) & endfor
  for i=0,nl-1 do begin 
    zline(i,0)=zin(i) & zline(nl+i,0)=zin(i) & endfor
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
;------------COMPUTATION OF FIELD LINES------------
  while (ks lt maxnstep and inatot gt 0) do begin
    pmnull=replicate(1.0,nltot)
    if min(zl) lt 0.0 then begin
      negs= where(zl lt 0.0)
      pmnull(negs)=-pmnull(negs)
    endif
;    print, zl,pmnull
    for i=0,nltot-1 do $
    if (inarea(i) eq 1) then begin
      dumm=where(x lt xl(i),idumm)
      ixl(i)=idumm-1
      dumm=where(y lt pmnull(i)*yl(i),idumm)
      iyl(i)=idumm-1
      dumm=where(z lt pmnull(i)*zl(i),idumm)
      izl(i)=idumm-1
    endif
    hxo=( xl-x(ixl) )/( x(ixl+1)-x(ixl) )
    hyo=( pmnull*yl-y(iyl) )/( y(iyl+1)-y(iyl) )
    hzo=( pmnull*zl-z(izl) )/( z(izl+1)-z(izl) )
    rixl=float(ixl)+hxo
    riyl=float(iyl)+hyo
    rizl=float(izl)+hzo
    bxl=pmnull*interpolate(bx,rixl,riyl,rizl)
    byl=interpolate(by,rixl,riyl,rizl)
    bzl=interpolate(bz,rixl,riyl,rizl)
    babs=sqrt(bxl*bxl+byl*byl+bzl*bzl)
    xl=xl+0.5*float(inarea)*del*bxl/babs
    yl=yl+0.5*float(inarea)*del*byl/babs
    zl=zl+0.5*float(inarea)*del*bzl/babs
;    print, nstep(1), '   coord:  ', xl(1), yl(1), zl(1)
;    print, 'icoor:  ', rixl(1), riyl(1), rizl(1)
;    print, 'B:  ', bxl(1), byl(1), bzl(1)

    for i=0,nltot-1 do $
    if ( xl(i) lt xmin or xl(i) gt xmax $
      or yl(i) lt ymin or yl(i) gt ymax $
      or zl(i) lt zmin or zl(i) gt zmax) then inarea(i)=0
    inatot=total(inarea)

    pmnull=replicate(1.0,nltot)
    if min(zl) lt 0.0 then begin
      negs= where(zl lt 0)
      pmnull(negs)=-pmnull(negs)
    endif
    for i=0,nltot-1 do $
    if (inarea(i) eq 1) then begin
      dumm=where(x lt xl(i),idumm)
      ixl(i)=idumm-1
      dumm=where(y lt pmnull(i)*yl(i),idumm)
      iyl(i)=idumm-1
      dumm=where(z lt pmnull(i)*zl(i),idumm)
      izl(i)=idumm-1
    endif
    hxo=( xl-x(ixl) )/( x(ixl+1)-x(ixl) )
    hyo=( pmnull*yl-y(iyl) )/( y(iyl+1)-y(iyl) )
    hzo=( pmnull*zl-z(izl) )/( z(izl+1)-z(izl) )
    rixl=float(ixl)+hxo
    riyl=float(iyl)+hyo
    rizl=float(izl)+hzo
    bxl=pmnull*interpolate(bx,rixl,riyl,rizl)
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
;  for i=0,nltot-1 do print, i, nstep(i)

end


