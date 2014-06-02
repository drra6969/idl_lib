; START OF READ 3D DATA
  nxf=101 & nyf=101 & nzf=101     ;grid for uniform interpol fields
  time=0.0
  
  openr, 8, 'ttt',/F77_UNFORMATTED
  readu, 8, nxf, nyf, nzf, time 
    print, 'dimension nx=',nxf,'   ny=',nyf,'   nz=',nzf
    print, 'time=',time
    xf=fltarr(nxf,/NOZERO)
    yf=fltarr(nyf,/NOZERO)
    zf=fltarr(nzf,/NOZERO)
    fieldf=fltarr(nxf,nyf,nzf,/NOZERO)
  readu, 8,  xf,yf,zf
    print, 'xmin=', xf(0), '  xmax=', xf(nxf-1)
    print, 'ymin=', yf(0), '  ymax=', yf(nyf-1)
    print, 'zmin=', zf(0), '  zmax=', zf(nzf-1)
  readu, 8,  fieldf

end
