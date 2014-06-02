; START OF MAIN PROGRAM

  nx=long(131) & ny=long(126) & nz=long(126) & nzz=long(126) & time=0.0
  bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
  filenm='jkjksdjks'

  fnumber=1 & answer='l'
  name='' & contin='' & again='y' & withps='n' & run=''

  print, 'Input filenumber'
  read, fnumber
  name='pagtap'+string(fnumber,'(i1)')
;  name='magveb'+string(fnumber,'(i1)')
  openr, 8, name,/F77_UNFORMATTED

  readu, 8, nx, ny, nz, time, nzz
    print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz
    nv=nx*ny
    xv=fltarr(nv,/NOZERO) & dx=xv & dxh=xv
    yv=fltarr(nv,/NOZERO) & dy=yv & dyh=yv
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z 
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
  readu, 8,  xv,dx,dxh,dxh,dxh,dxh,dxh,yv,dy,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
    x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO) 
    x=xv(0:(nx-1)) & for j=0,ny-1 do y(j)=yv(j*nx)


;  readu, 8, nx, ny, nz, time, nzz
;    print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz,'   nzz=',nzz
;    print, 'time=',time
;    nv=nx*ny  & xvec=fltarr(nv,/NOZERO) & dxvec=xvec & yvec=xvec & dyvec=xvec
;    x=fltarr(nx,/NOZERO) & dx=x & dxh=xvec
;    y=fltarr(ny,/NOZERO) & dy=y & dyh=yvec
;    z=fltarr(nz,/NOZERO) & dz=z & dzh=z & u1=z & u2=z & u3=z
;  readu, 8,  xvec,dxvec,dxh,dxh,dxh,dxh,dxh,yvec,dyvec,dyh,dyh,dyh,dyh,dyh,$
;             z,dz,dzh,dzh,dzh,dzh,dzh
;    x=xvec(0:nx-1) & dx=dxvec(0:nx-1)
;    for i=0,ny-1 do y(i)=yvec(i*nx)  &   for i=0,ny-1 do dy(i)=dyvec(i*nx) 
;
;    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 



  print, x
  print, y
  print, z
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
  xmax =  4 & ymax = 20 & zmax =  100
  if zmin lt z(1) then print, $
    'WARNING! FLINE SUBROUTINE assumes mirror symmetry at zmin!!!!'
;  nl=12  
;  xin=findgen(nl) & yin=xin & zin=xin
;  xstart=0 & ystart=0.0 & zstart=0. 
;  dxin=0.5 & dyin=0.0 & dzin=0.0
;  xstart1=0.0 & ystart1=0.0 & zstart1=-14 
;  dxin1=0.0 & dyin1=0.0 & dzin1=1.0
;;  xstart=3.3 & ystart=-115.0 & zstart=20.0 
;;  dxin=0.0 & dyin=0.0 & dzin=0.7
;  xin=xin*dxin+xstart  
;  yin=yin*dyin+ystart
;  zin=zin*dzin+zstart 
;  for i=0,5 do begin
;    xin(6+i)=float(i)*dxin1+xstart1
;    yin(6+i)=float(i)*dyin1+ystart1
;    zin(6+i)=float(i)*dzin1+zstart1
;  endfor
;  l1=[0.0, 0.0, 12.0] & l2=[0.0, 0.0, 20.0]
;  sline, l1, l2, 5, 0.1, xin1,yin1,zin1
;  l1=[-1.05, 39.0, 45.0] & l2=[-1.15, 39.0, 45.0]
;  sline, l1, l2, 5, 0.05, xin1,yin1,zin1
;  l1=[-2.6, 39.0, 45.0] & l2=[-2.8, 39.0, 45.0]
;  sline, l1, l2, 5, 0.05, xin2,yin2,zin2
;  l1=[2.09, 39.0, -45.0] & l2=[2.14, 39.0, -45.0]
;  sline, l1, l2, 5, 0.05, xin1,yin1,zin1
;  l1=[-2.05, 39.0, 45.0] & l2=[-2.1, 39.0, 45.0]
;  sline, l1, l2, 5, 0.05, xin,yin,zin
  l1=[0.0, 1.5, 45.0] & l2=[0.0, 5.0, 45.0]
  sline, l1, l2, 4, 0.05, xin1,yin1,zin1
  l1=[-2.0,15.0, 5.] & l2=[2.0, 15.0, 5.]
  sline, l1, l2, 4, 0.05, xin2,yin2,zin2
;  l1=[-1.3, 39.0, 45.0] & l2=[-1.9, 39.0, 45.0]
;  sline, l1, l2, 5, 0.05, xin3,yin3,zin3
  
;  ce=[-3.0, 0.0, 0.0] & d1=[1.0, 0.0, 0.0] & d2=[0.0, 0.0, 1.0]
;  scross, ce,d1,d2,0.5,0.5,3,0.0, xin2,yin2,zin2
  xin=[xin1,xin2]
  yin=[yin1,yin2]
  zin=[zin1,zin2]
;  xin=[xin1,xin2,xin3]
;  yin=[yin1,yin2,yin3]
;  zin=[zin1,zin2,zin3]

  s=size(xin)
  print,s
  nl=s(1)
  dels=0.20 
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
       nframes=9
;      xanimate, set=[512,512,nframes]
;      xang=25
;      yang=45
      for ijk=0,nframes-1 do begin
        xang=20
	yang=10+ijk*20
	zang=0
	persp=6
 back:	boxy,xmin,xmax,ymin,ymax,zmin,zmax,xang,yang,zang,persp
        for i=0,2*nl-1 do begin
          ndim=nstep(i)
          if ndim ge 2 then begin
            xt=xline(i,0:(ndim-1))
            yt=yline(i,0:(ndim-1))
            zt=zline(i,0:(ndim-1))
            draw,xt,yt,zt,0
;            projx,xmin,yt,zt
            if yang le 90 then projy,ymin,xt,zt
            if yang gt 90 then projy,ymax,xt,zt
            projz,zmin,xt,yt
;            plots,xt,yt,zt,linestyle=3, $
;            COLOR = BYTSCL(xt,max=xmax+1.,min=xmin-1.),/t3d,/data
;            COLOR = BYTSCL(xt, TOP=!D.X COLORS-1),/t3d,/data
          endif
        endfor
        if answer eq 'y' then device, /close
        print,'Do you want to create a PS plot?'
	read,answer
	if (answer eq 'y') then begin
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


