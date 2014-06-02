; START OF MAIN PROGRAM

  nx=long(131) & ny=long(126) & nz=long(126) & time=0.0
  bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
  filenm='jkjksdjks'

  fnumber=1 & answer='l'
  name='' & contin='' & again='y' & withps='n' & run=''

  print, 'Input filenumber'
  read, fnumber
  name='pagtap'+string(fnumber,'(i1)')
;  name='magveb'+string(fnumber,'(i1)')
  openr, 8, name,/F77_UNFORMATTED

  readu, 8, nx, ny, nz, time 
    print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz
    xv=fltarr(nx*ny,/NOZERO) & dx=xv & dxh=xv
    yv=fltarr(nx*ny,/NOZERO) & dyv=yv & dyh=yv
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z 
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
  readu, 8,  xv,dx,dxh,dxh,dxh,dxh,dxh,yv,dy,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
    x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO) 
    x=xv(0:(nx-1)) & for j=0,ny-1 do y(j)=yv(j*nx)
  readu, 8,  bx,by,bz
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
  xmin = -4 & ymin = 0 & zmin = 0
  xmax =  4 & ymax = 15 & zmax =  100
  if zmin lt z(1) then print, $
    'WARNING! FLINE SUBROUTINE assumes mirror symmetry at zmin!!!!'
;  l1=[-1.5, 0.0, 0.] & l2=[-1.5, 8.0, 0.]
;  sline, l1, l2, 4, 0.05, xin1,yin1,zin1
;  l1=[-0.0, 0.0, 0.5] & l2=[-0.0, 0.0, 2.5]
;  sline, l1, l2, 4, 0.05, xin2,yin2,zin2
;  l1=[0.0, 0.0, 15.0] & l2=[3, 0.0, 15.0]
;  sline, l1, l2, 4, 0.05, xin3,yin3,zin3
  l1=[-0.3, -98.0, 6.] & l2=[0.3, -98.0, 6.]
  sline, l1, l2, 7, 0.05, xin1,yin1,zin1
  l1=[1.0, -98.0, 20.0] & l2=[2.5, -98.0, 20.0]
  sline, l1, l2, 4, 0.05, xin2,yin2,zin2
  l1=[-1.0, -98.0, 20.0] & l2=[-2.5, -98.0, 20.0]
  sline, l1, l2, 4, 0.05, xin3,yin3,zin3
  
;  ce=[-3.0, 0.0, 0.0] & d1=[1.0, 0.0, 0.0] & d2=[0.0, 0.0, 1.0]
;  scross, ce,d1,d2,0.5,0.5,3,0.0, xin2,yin2,zin2
;  xin=[xin1,xin2]
;  yin=[yin1,yin2]
;  zin=[zin1,zin2]
  xin=[xin1,xin2,xin3]
  yin=[yin1,yin2,yin3]
  zin=[zin1,zin2,zin3]

  s=size(xin)
  print,s
  nl=s(1)
  dels=0.25 
  print,'GO'
  FLINES, BX, BY, BZ, X, Y, Z, XIN, YIN, ZIN, $ 
            XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, DELS, $
            xline, yline, zline, nstep, inatot

  nstepmax=max(nstep)
  print, inatot
  for i=0,2*nl-1 do print, i, nstep(i)
;  for j=0,2*nl-1 do $
;  for i=0,nstep(j) do print, j,i,xline(j,i),yline(j,i),zline(j,i)

;       LOADCT, 32
       nframes=5
;      xanimate, set=[512,512,nframes]
;      xang=25
;      yang=50
      for ijk=0,nframes-1 do begin
        xang=20
	yang=60+ijk*20
	zang=0
	persp=6
        !P.CHARTHICK=2.
 back:  if (withps eq 'y') then !P.THICK=3.
        if (withps eq 'y') then !P.CHARTHICK=3.
	box1,xmin,xmax,ymin,ymax,zmin,zmax,xang,yang,zang,persp
        for i=0,2*nl-1 do begin
          ndim=nstep(i)
          if ndim ge 2 then begin
            xt=xline(i,0:(ndim-1))
            yt=yline(i,0:(ndim-1))
            zt=zline(i,0:(ndim-1))
            if (withps eq 'y') then !P.THICK=3.
            draw,xt,yt,zt,0
;            projx,xmin,yt,zt
            !P.THICK=1.
            if yang le 90 then projy,ymin,xt,zt
            if yang gt 90 then projy,ymax,xt,zt
            projz,zmin,xt,yt
;            plots,xt,yt,zt,linestyle=3, $
;            COLOR = BYTSCL(xt,max=xmax+1.,min=xmin-1.),/t3d,/data
;            COLOR = BYTSCL(xt, TOP=!D.X COLORS-1),/t3d,/data
          endif
        endfor
        if withps eq 'y' then begin
          device, /close
          !P.THICK=1.
          !P.CHARTHICK=2.
        endif
        print,'Do you want to create a PS plot?'
	read,withps
	if (withps eq 'y') then begin
	  print,'Please give filename for PS file'
	  read,filenm
     	  set_plot,'PS'
          device,filename=filenm
          device,/inches,xsize=8.,scale_factor=0.8,xoffset=0.5
          device,/inches,ysize=8.,scale_factor=0.8,yoffset=0.5
	  goto,back 
	endif else begin
	  set_plot,'x'
	endelse
;        xanimate, frame=ijk, window=!d.window
      endfor
	print,'Do you want to repeat plots?'
	read,answer
  
  if (answer eq 'y')  then goto,back 
	
end


