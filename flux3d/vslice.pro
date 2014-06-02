; START OF MAIN PROGRAM

  COMMON field, ff, xmin,xmax,ymin,ymax,zmin,zmax
  mx=long(131) & my=long(126) & mz=long(126) 
  time=0.0 & fnumber=1
  filenm='jkjksdjks'
  print, 'number of datasets'
  read, noset
  
    print, 'Input filenumber (2 digits!)'
    read, fnumber
    name='mflux'+string(fnumber,'(i2)')
    openr, 8, name,/F77_UNFORMATTED
    readu, 8, mx,my,mz,time
    print, mx,my,mz,time
    ff=fltarr(mx,my,mz) 

  for i=1, noset do begin
    x=fltarr(mx,/NOZERO) & dx=x & dxh=x
    y=fltarr(my,/NOZERO) & dy=y & dyh=y
    z=fltarr(mz,/NOZERO) & dz=z & dzh=z 
    readu, 8,  x,y,z
    nuno0=long(131)
    readu, 8, nuno0
    print, nuno0
    iff=lonarr(nuno0,/NOZERO) & wf=fltarr(nuno0,/NOZERO)  
    readu, 8, iff, wf
    close, 8
    print, size(iff)
    print, size(wf)
;    for i=0, 21 do print, iff(i)
  
    ixf=lonarr(nuno0,/NOZERO) & iyf=ixf & izf=ixf & ivf=ixf
    mv=long(mx*my)
    iff = iff - 1
    ivf = iff mod mv
    ixf = (ivf mod mx)
    iyf = ivf/mx
    izf = iff/mv
    ff(ixf,iyf,izf) = ff(ixf,iyf,izf) + wf
  
    print, 'max wf', max(wf)
    i1=where(wf gt 0.0, count)
    print, 'no ind wf>0.0', count
    i1=where(wf gt 0.01, count)  
    print, 'no ind wf>0.01', count
    i1=where(wf gt 0.1, count)  
    print, 'no ind wf>0.1', count
    i1=where(wf gt 1.0, count)  
    print, 'no ind wf>1.', count
    i1=where(wf gt 10.0, count)  
    print, 'no ind wf>10.', count
;    for i=0, 21 do print, ixf(i),iyf(i),izf(i),ivf(i),iff(i),wf(i)
;    for i=nuno0-11,nuno0-1 do $
;           print, ixf(i),iyf(i),izf(i),ivf(i),iff(i),wf(i)
;   
;    name='' & contin='' & again='y' & withps='n' & run=''
;    answer='l'

    print, mx,my,mz,time
    print, 'xmin=', x(0), '  xmax=', x(mx-1)
    print, 'ymin=', y(0), '  ymax=', y(my-1)
    print, 'zmin=', z(0), '  zmax=', z(mz-1)
;----PARAMETER----
    xmin = x(0)    & ymin = y(0)    & zmin = z(0)
    xmax = x(mx-1) & ymax = y(my-1) & zmax = z(mz-1)


    if i lt noset then begin
      print, 'Input filenumber (2 digits!)'
      read, fnumber
      name='mflux'+string(fnumber,'(i2)')
      openr, 8, name,/F77_UNFORMATTED
      readu, 8, mx,my,mz,time
      print, mx,my,mz,time
    endif
    
  endfor

;----PARAMETER----
;  xmin = -3 & ymin = -40 & zmin = -40
;  xmax =  3 & ymax =  40 & zmax =  40
;  if zmin lt z(1) then print, $
;    'WARNING! FLINE SUBROUTINE assumes mirror symmetry at zmin!!!!'

;  SLICER
   PLOT3D

	
end


