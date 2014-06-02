PRO FLFLUX, BX, BY, BZ, X, Y, Z, XIN, YIN, ZIN, $ 
            XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, DELS, $
            ff, nstep, inatot

  maxnstep=801
  s=size(bx)
  print, s
  nx=s(1) & ny=s(2)  & nz=s(3) 
  t=size(xin)
  nl=t(1) & nltot=2*nl
  print, nl, nltot
  ixl=intarr(nltot) & iyl=ixl & izl=iyl
  xl=fltarr(nltot) & yl=xl & zl=xl
  del=fltarr(nltot)
  sf=size(ff) 
  nxf=sf(1) & nyf=sf(2) & nzf=sf(3)
  ixf=intarr(nltot) & iyf=ixf & izf=ixf
  xf=findgen(nxf) & yf=findgen(nyf) & zf=findgen(nzf)
  xsize=xmax-xmin & ysize=ymax-ymin & zsize=zmax-zmin
  delxf=xsize/float(nxf-1)
  delyf=ysize/float(nzf-1)
  delzf=zsize/float(nzf-1)
  xf=xmin+xf*delxf & yf=ymin+yf*delyf & zf=zmin+zf*delzf
  
;  xline=fltarr(nltot,maxnstep) & yline=xline & zline=xline
  nstep=intarr(nltot) & inarea=intarr(nltot)
   xl(0:nl-1)=xin & xl(nl:nltot-1)=xin
   yl(0:nl-1)=yin & yl(nl:nltot-1)=yin
   zl(0:nl-1)=zin & zl(nl:nltot-1)=zin
  for i=0,nl-1 do del(i)=dels
  for i=nl,nltot-1 do del(i)=-dels
  for i=0,nltot-1 do begin & nstep(i)=0 & inarea(i)=1 & endfor
  xl1=xl & yl1=yl & zl1=zl 
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
; this is for the scalar field!!
;      dumm=where(xf lt xl(i),idumm)
;      ixf(i)=idumm-1
;      dumm=where(yf lt yl(i),idumm)
;      iyf(i)=idumm-1
;      dumm=where(zf lt zl(i),idumm)
;      izf(i)=idumm-1
    endif
    ixf=fix((xl-xmin)/xsize*(nxf-1))
    iyf=fix((yl-ymin)/ysize*(nyf-1))
    izf=fix((zl-zmin)/zsize*(nzf-1))
    hxf=( xl-xf(ixf) )/delxf
    hyf=( yl-yf(iyf) )/delyf
    hzf=( zl-zf(izf) )/delzf
    ff(ixf,iyf,izf)=ff(ixf,iyf,izf)+$
                    float(inarea)*(1.0-hxf)*(1.0-hyf)*(1.0-hzf)
    ff(ixf+1,iyf,izf)=ff(ixf+1,iyf,izf)+$
                    float(inarea)*hxf*(1.0-hyf)*(1.0-hzf)
    ff(ixf,iyf+1,izf)=ff(ixf,iyf+1,izf)+$
                    float(inarea)*(1.0-hxf)*hyf*(1.0-hzf)
    ff(ixf,iyf,izf+1)=ff(ixf,iyf,izf+1)+$
                    float(inarea)*(1.0-hxf)*(1.0-hyf)*hzf
    ff(ixf+1,iyf+1,izf)=ff(ixf+1,iyf+1,izf)+$
                    float(inarea)*hxf*hyf*(1.0-hzf)
    ff(ixf,iyf+1,izf+1)=ff(ixf,iyf+1,izf+1)+$
                    float(inarea)*(1.0-hxf)*hyf*hzf
    ff(ixf+1,iyf,izf+1)=ff(ixf+1,iyf,izf+1)+$
                    float(inarea)*hxf*(1.0-hyf)*hzf
    ff(ixf+1,iyf+1,izf+1)=ff(ixf+1,iyf+1,izf+1)+$
                    float(inarea)*hxf*hyf*hzf
; now we continue to integrate the field        
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
    xl1=xl1+float(inarea)*del*bxl/babs
    yl1=yl1+float(inarea)*del*byl/babs
    zl1=zl1+float(inarea)*del*bzl/babs
    xl=xl1 & yl=yl1 & zl=zl1 
    
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


