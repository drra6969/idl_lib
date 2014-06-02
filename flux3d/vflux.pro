; START OF MAIN PROGRAM
;    program to show flux surfaces. The scalar field which represents  
;    the flux surface is actually computed in flflux. The method is slow 
;    compared to computation of flux surfaces by a fortran code. 

  COMMON VOLUME_DATA, ff
  nx=long(131) & ny=long(126) & nz=long(126) & time=0.0
  nxf=126 & nyf=126 & nzf=126
  ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
  ff=fltarr(nxf,nyf,nzf) 
  xf=findgen(nxf) & yf=findgen(nyf) & zf=findgen(nzf) 

  fnumber=1 & answer='l' & filenm='jkjksdjks'
  name='' & contin='' & again='y' & withps='n' & run=''

  print, 'Input filenumber'
  read, fnumber
  name='magtap0'+string(fnumber,'(i1)')
;  name='magveb'+string(fnumber,'(i1)')
  openr, 8, name,/F77_UNFORMATTED

  readu, 8,  nx,ny,nz,time
    print, nx,ny,nz,time
    x=fltarr(nx,/NOZERO) & dx=x & dxh=x
    y=fltarr(ny,/NOZERO) & dy=y & dyh=y
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z 
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
  readu, 8,  x,dx,dxh,dxh,dxh,dxh,dxh,y,dy,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
  readu, 8,  bx,by,bz
    print, 'xmin=', x(1), '  xmax=', x(nx-2)
    print, 'ymin=', y(1), '  ymax=', y(ny-2)
    print, 'zmin=', z(1), '  zmax=', z(nz-2)
  close, 8

  amin=min(bx) & amax=max(bx) & thresh=amin+0.5*(amax-amin)
  print,'bx:',amin,amax,thresh
  amin=min(by) & amax=max(by) & thresh=amin+0.5*(amax-amin)
  print,'by:',amin,amax,thresh
  amin=min(bz) & amax=max(bz) & thresh=amin+0.5*(amax-amin)
  print,'bz:',amin,amax,thresh
;  print, x
;  print, y
;  print, z

;----PARAMETER----
  xmin = -8 & ymin = -15 & zmin = -20
  xmax =  8 & ymax =  15 & zmax =  20
  if zmin lt z(1) then print, $
    'WARNING! FLINE SUBROUTINE assumes mirror symmetry at zmin!!!!'

;corners for startpoints
  c00=[-1.0,0.0,-20.] & c10=[-1.0,2.0,-20.] 
  c01=[1.0,0.0,-20.] 
; number of startpoint in dir 1 and dir 2
  nstart1 = 21
  nstart2 = 21
  nl = nstart1*nstart2
  dst1=(c10-c00)/float(nstart1-1)
  dst2=(c01-c00)/float(nstart2-1)
  xin=findgen(nl) & yin=xin & zin=xin
  instart=findgen(nstart1)
  for i=0, nstart2-1 do begin
    xin((i*nstart1):((i+1)*nstart1-1))=c00(0)+dst1(0)*instart $
                         +dst2(0)*float(i)
    yin((i*nstart1):((i+1)*nstart1-1))=c00(1)+dst1(1)*instart $
                         +dst2(1)*float(i)
    zin((i*nstart1):((i+1)*nstart1-1))=c00(2)+dst1(2)*instart $
                         +dst2(2)*float(i)
  endfor
  s=size(xin) & print,s
  nl=s(1)
  dels=0.25  & print,'GO'
  s=size(ff) & print,s
  
  FLFLUX, BX, BY, BZ, X, Y, Z, XIN, YIN, ZIN, $ 
          XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, DELS, $
          ff, nstep, inatot
  print,'flines completed'
;  SLICER
   PLOT3D
	
end


