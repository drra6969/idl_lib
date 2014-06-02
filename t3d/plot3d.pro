; Conventions:
;
; Faces = faces of cube.  Numbered 0 to 5:
;		0 = X = c0(0)  (Corner 0)
;		1 = Y = c0(1)
;		2 = Z = c0(2)
;		3 = X = c1(0)  (Corner 1)
;		4 = Y = c1(1)
;		5 = Z = c1(2)
; Orientations:
;	6 = reset to default.
;	0 = xy exchange, 1 = xz exchange, 2 = yz exchange.
;	3 = x reverse, 4 = y reverse, 5 = z reverse.
; Vertex indices:	Faces
;	0	0,0,0   0,1,2
;	1	N,0,0	1,2,3
;	2	0,N,0	0,2,4
;	3	N,N,0	2,3,4
;	4	0,0,N	0,1,5
;	5	N,0,N	1,3,5
;	6	0,N,N	0,4,5
;	7	N,N,N	3,4,5
;
; Edge Index	Vertex Indices
;	0	0-1
;	1	1-2
;	2	2-3
;	3	3-1
;	4	0-4
;	5	1-5
;	6	2-6
;	7	3-7
;	8	4-5
;	9	5-6
;	10	6-7
;	11	7-4
;
; Modes: (From Slicer, Not supported)
;	0 = Slices
;	1 = Cube
;	2 = Cut
;	3 = Isosurface
;	4 = Probe
;	5 = rotations
;
; Tasks (menu)
;         o -> Orientation
;         c -> Color Table
;         s -> Slice
;         i -> Isosurface
;         a -> Animation
;         p -> Postscript
;         u -> Undo last Plot
;         e -> Erase
;         q -> Terminate

PRO orient
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu
;  Set the New Orientation
  t3d,/reset
;  print,'in orient',az
  if n_elements(iaxes) gt 0 then begin
	if iaxes le 2 then begin
		j = 2 * iaxes
		ll = ([0,1,0,2,1,2])(j:j+1)  ;axes to swap
		t = axex(ll(0)) & axex(ll(0)) = axex(ll(1)) & 
		axex(ll(1)) = t
	endif else if iaxes eq 6 then begin
		axex = [0,1,2]		;default transformation
		axrev = intarr(3)
	endif else axrev(iaxes-3) = 1-axrev(iaxes-3)  ;reverse
  endif		
  d = [ 0., dims(0), 0., dims(1), 0., dims(2)]
  f = zscale
  if f gt 1. then begin
    x = (f-1.)/2.
    d(0) = [-x*dims(0), (x+1)*dims(0), -x*dims(1), (x+1)*dims(1)]
  endif else begin
    x = (1-f)/2
    d(4) = [-x*dims(2), (x+1)*dims(2)]
  endelse
  for i=0,2 do if axrev(i) then begin	;Swap endpoints for rev axes
	j=i*2
	t = d(j) & d(j) = d(j+1) & d(j+1) = t
  endif
  !x.type = 0		;make sure its linear
  scale3, xrange=d(0:1), yrange=d(2:3), zrange=d(4:5), $
          ax = ax, az = az
;  scale3, xrange=d(0:1), yrange=d(2:3), zrange=d(4:5),ax=0,az=0
  h=[0.5,0.5,0.5]
  t3d,trans=-h
  t3d,perspective=persp
  t3d,trans=h

  k = 1		;current y axes
  if axex(0) ne 0 then begin	;swap x?
	if axex(0) eq 1 then begin & t3d, /XYEXCH & k = 0
	endif else t3d,/XZEXCH
  endif
  if k ne axex(1) then t3d,/YZEXCH
;  erasep
  draw_orientation
  inverse = invert(!p.t)  
  end



PRO draw_orientation	
; draw the orientation cube in the small window
; Draw the outline of the 3 frontmost faces of the main cube.
; Draw the idir axes plane in green.
; If the mode is cut or cube, draw the selected cube.  
; Draw the back faces in blue, and label the selection points.
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu
;  print,'in draw_orientation'
  mode=1 & win =1
;  print,'mode=',mode
  wset, win
  device, set_graph = 3				;Copy mode
  erase
  z = [0,0,0]
  d1 = dims-1
  nlines = 6
  p = z
  d = (d1-z) / float(nlines-1)
  p(idir) = (d1(idir) + z(idir))/2.
  d(idir) = 0.0
  for i=0,nlines-1 do begin		;draw idir direction
	xx = replicate(p(0),2)
	yy = replicate(p(1),2)
	zz = replicate(p(2),2)
	if idir ne 0 then plots, [z(0),d1(0)], yy,zz, /T3D, /DATA
	if idir ne 1 then plots, xx, [z(1),d1(1)],zz, /T3D, /DATA
	if idir ne 2 then plots, xx, yy, [z(2),d1(2)],/T3D, /DATA
	p = p + d
  endfor
;    draw_cube, p0cube(*,0), p0cube(*,1), indgen(6),win  ;all faces
;    draw_cube, p0cube(*,0), p0cube(*,1), 
;         where(facevs eq v_close)/4,win  
;    for i=0,1 do begin		;Label them
;		p = convert_coord(p0cube(*,i), /T3D, /TO_DEV, /DATA)
;		xyouts, p(0), p(1), strtrim(i,2), /device
;		endfor
  px = convert_coord([d1(0)/5,-d1(1)/20,-d1(2)/20], $
        /T3D, /TO_DEV, /DATA)
  py = convert_coord([-d1(0)/20,d1(1)/5,-d1(2)/20], $
        /T3D, /TO_DEV, /DATA)
  pz = convert_coord([-d1(0)/20,-d1(1)/20,d1(2)/3], $
        /T3D, /TO_DEV, /DATA)
  xyouts, px(0), px(1), 'X', /device
  xyouts, py(0), py(1), 'Y', /device
  xyouts, pz(0), pz(1), 'Z', /device
  draw_cube, [0,0,0], dims-1, indgen(6), win
                      ; where(facevs eq v_close)/4,win  
                                           ;draw close faces
  win=0
  wset, win
end




PRO draw_cube, c0, c1, faces, win
;	draw a cube whose opposite corners are [c0(0),c0(1),c0(2)],
;	and [c1(0), c1(1), p2(3)].
;	color = drawing color.
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu
  if n_elements(win) gt 0 then  wset, win
;  print,'in draw_cube'
  pp0 = intarr(3,8)
  pp1 = float(p0)
  cc = [[c0],[c1]]
  for i=0,7 do begin
	pp0(0,i) = [cc(0,i and 1),cc(1,(i/2 and 1)),cc(2,(i/4 and 1))]
	pp1(0,i) = convert_coord(pp0(*,i), /T3D,/TO_DEVICE,/DATA)
	endfor
  f = facevs
  flags = bytarr(8,8)	;line flags, dont draw same line twice
  for i=0,n_elements(faces)-1 do begin
    ff = [ f(*,faces(i)), f(0,faces(i))]  ;Vertex indices
    for j=0,3 do begin
	k = ff(j) < ff(j+1) & l = ff(j) > ff(j+1)
	if not flags(k,l) then plots, pp1(*,[k,l]), /dev
	flags(k,l) = 1
	endfor
    endfor
end
 



PRO draw_slice	;draw a slice.
;  idir = axis, 0 for x, 1 for Y, 2 for Z. slice = plane number.
  COMMON field, a, xmin,xmax,ymin,ymax,zmin,zmax
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu
  print,'in slice'
  set_plot,'Z'
  z_last = tvrd()			;save previous image & z
  zb_last = tvrd(CHANNEL=1, /WORDS)
  interp=1
  d0 = [0,0,0]
  d1 = dims -1
  d0(idir) = islice
  d1(idir) = islice
  slmax=max(a(d0(0):d1(0), d0(1):d1(1), d0(2):d1(2)))
  slmin=min(a(d0(0):d1(0), d0(1):d1(1), d0(2):d1(2)))
  sldel=slmax-slmin
  p = bytscl(a(d0(0):d1(0), d0(1):d1(1), d0(2):d1(2)), $
	     max=slmax, min=slmin,top=192)
  t = 0
  d1 = dims-1
  s = replicate(islice, 4)
  case idir of
  0:	polyfill, s, [0,d1(1),d1(1),0],[0,0,d1(2),d1(2)],/T3D,$
		pat=reform(p, dims(1), dims(2), /OVER), $
		image_coord = [0,0, d1(1),0, d1(1),d1(2), 0,d1(2)], $
		image_interp= interp, trans=t
  1:	polyfill, [0,d1(0),d1(0),0],s,[0,0,d1(2),d1(2)],/T3D,$
		pat=reform(p, dims(0), dims(2), /OVER), $
		image_coord = [0,0, d1(0),0, d1(0),d1(2), 0,d1(2)], $
		image_interp= interp, trans=t
  2:	polyfill,[0,d1(0),d1(0),0],[0,0,d1(1),d1(1)],s,/T3D,$
		pat=reform(p, dims(0), dims(1), /OVER),$
		image_coord = [0,0, d1(0),0, d1(0),d1(1), 0,d1(1)], $
		image_interp= interp, trans=t
  endcase
  show
end



PRO histo
  COMMON field, a, xmin,xmax,ymin,ymax,zmin,zmax
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu
  wset, 1
  type = size(a)
  int = type(type(0) + 1) le 3  ;True if int type
  j = (amax -amin)/100. ;bin size
  if int then j = j > 1
  h = histogram(a, max=amax, min = amin, bin=j)
  if int then j = fix(j + .99)
  k = sort(h)
  n = n_elements(h)
  x = findgen(n) * j + amin < amax
  plot,x,h, xst = 9, yst=8, ymargin=[2,0], $
      yrange= [0,h(k(n-8))], yticks=4, chars=.75, xticks=4
  wset, 0
end



PRO isosurf
  COMMON field, a, xmin,xmax,ymin,ymax,zmin,zmax
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu
;  print,'in isosurf'
  set_plot,'Z'
  print, 'Computing Polygons'
  shade_volume, a, isop.value, verts, polys, $
	low = isop.hi_lo
  if n_elements(verts) eq 0 then begin
	print, 'No surface at this value'
	set_plot,oldev
  endif else begin & print, $
	strtrim((size(verts))(2),2)+' Vertices, ' + $
	strtrim((size(polys))(1)/4,2) + ' Polygons.'
    z_last = tvrd()			;Save old display
    zb_last = tvrd(CHANNEL=1, /WORDS)
    b = polyshade(verts,polys,/T3D,top=192)
    verts = 0 & polys = 0		;Free space
    show
  endelse
end



PRO anim
  COMMON field, a, xmin,xmax,ymin,ymax,zmin,zmax
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu
  print,'in anim'

  nf1=33 & nf2=32 & nf3=32 & 
  nftot=15          ;or  nftot=nf1+nf2+nf3 
  ax0=ax & ax1=80 & ax2=150 & ax3=ax
  az0=az & az1=210 & az2=250 & az3=az+360
  dx1=(ax1-ax0)/(nf1-1) & dx2=(ax2-ax1)/nf2 & dx3=(ax3-ax2)/nf3
  dz1=(az1-az0)/(nf1-1) & dz2=(az2-az1)/nf2 & dz3=(az3-az2)/nf3
  axf=fltarr((nftot)) & azf=axf & perspf=axf
;  axf(0:(nf1-1))=ax0+dx1*findgen(nf1)
;  axf(nf1:(nf1+nf2-1))=ax1+dx2*findgen(nf2)+dx2
;  axf((nf1+nf2):(nf1+nf2+nf3-1))=ax2+dx3*findgen(nf3)+dx3
;  azf(0:(nf1-1))=az0+dz1*findgen(nf1)
;  azf(nf1:(nf1+nf2-1))=az1+dz2*findgen(nf2)+dz2
;  azf((nf1+nf2):(nf1+nf2+nf3-1))=az2+dz3*findgen(nf3)+dz3
  pi=2.0*asin(1.0) & ei=findgen(nftot)
  axf(*) = ax0+40.0*( sin( 2*pi*ei(*)/float(nftot)-pi/8.0 ) $
                      + sin(pi/8.0) )
  azf(*) = az0+360.0/nftot*ei(*)+1.2*sin( 4*pi*ei(*)/float(nftot) )
  perspf(*)= 1.5 + 0.6*sin( 4*pi*ei(*)/float(nftot) )

  xanimate, set=[resolution(0),resolution(1),nftot]
  for ip=0,nftot-1 do begin
     ax=axf(ip) & az=azf(ip) & persp=perspf(ip)
     print,'ax=',ax, '  az=',az, '  persp=', persp
     orient
     erasep
     isop.hi_lo=0
     isosurf
     isop.hi_lo=1
     isosurf
;     draw_slice
     imgif = tvrd()
     gifname='gifout'+string(ip,'(i2.2)')
     WRITE_GIF, gifname, imgif
     xanimate, frame=ip, window=!d.window
  endfor
  xanimate,10
end



PRO gif
  COMMON field, a, xmin,xmax,ymin,ymax,zmin,zmax
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu
   print,'in gif'
   imgif = tvrd()
   gifname='im.gif'
   WRITE_GIF, gifname, imgif
end



PRO post
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu
  print,'in post'
;  move the Z buffer to postscript device.  leave device set to X.
; if parameter is present, show it rather than reading the Z buffer
;  set_plot,'z'
  image = tvrd()
  print, 'in ps'
  sss=size(image)
;  print,'size',sss
;    set_plot,/interpolate, 'PS'
    
    set_plot, 'PS'
    !p.color=255
    device,/color,bits_per_pixel=8,filename='p.ps',$
           xsize=12.5,ysize=12.5,xoffset=1.5,yoffset=1.5
;    device,/encapsul,bits_per_pixel=8,filename='p.ps',$
;           xsize=sss(1)/1000. ,ysize=sss(2)/1000. 
    tv, image
    device,/close
    
  set_plot, 'x'
  print, 'ende ps'
;  sl.cube_on = 0
  end



PRO postbw
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu
;  print,'in post'
;  move the Z buffer to postscript device.  leave device set to X.
; if parameter is present, show it rather than reading the Z buffer
;  set_plot,'z'
  image = tvrd()
  print, 'in ps grey'
  sss=size(image)
  print,'size',sss
    set_plot, 'PS'
    device,bits_per_pixel=8,filename='p.ps',$
           xsize=12.5,ysize=12.5,xoffset=2,yoffset=2
;    device,/encapsul,bits_per_pixel=8,filename='p.eps',$
;           xsize=sss(1)/1000. ,ysize=sss(2)/1000. 
;    device,xsize=8,ysize=8,xoffset=2,yoffset=2
;    device,xsize=14,ysize=14,xoffset=0,yoffset=2
    ind1=where(image eq max(image))
;    print, 'ind1=',ind1
    ind2=where(image eq min(image))
    print, 'ind2=',ind2
    maxim=max(image)  &  minim=min(image)
    print, '1. max(image):', max(image), '  min(image):', min(image)
    image1=image
    print, '2. max(image):', max(image), '  min(image):', min(image)
    image(ind1) = minim
    print, '3. max(image):',max(image), '  min(image):',min(image)
    maxim=max(image)  &  image(ind2)=maxim+1
    print, '4. max(image):', max(image), '  min(image):',min(image)
    tvscl, image
    device,/close

    
  set_plot, 'x'
  print, 'ende ps, greyscale'
;  sl.cube_on = 0
  end



PRO undo
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu
  print,'in undo'
  if n_elements(zb_last) le 1 then return
  set_plot,'Z'
  tmp = tvrd(/WORDS, CHANNEL=1)  ;Read depth buffer & swap
  tv, zb_last, CHANNEL=1, /WORDS
  zb_last = temporary(tmp)
  tmp = tvrd()		;Swap them
  tv, z_last
  show, z_last
  z_last = tmp
end



PRO erasep
;	call with no params to erase all.
  COMMON field, a, xmin,xmax,ymin,ymax,zmin,zmax
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu
;  print,'in erasep'
  set_plot,'Z'
  erase
  !P.CHARSIZE=3.0
  !P.CHARTHICK=2.5
  !P.THICK=1.5
  !X.THICK=2.5
  !Y.THICK=2.5
  !Z.THICK=2.5
  d1 = dims-1
  p1 = convert_coord(p0, /T3D, /TO_DEVICE, /DATA)  ;save dev coords
;  for i=0,7 do print, 'coord corner i=',i,' is:', $
;                    p1(0,i),p1(1,i),p1(2,i)
  dxd=max(p1(0,*))-min(p1(0,*))
  dyd=max(p1(1,*))-min(p1(1,*))
  s = strarr(8)
  for i=0,7 do begin
;	s(i) = string(pb(*,i),format ="(' (',i0,',',i0,',',i0,')')")
	s(i) = string(pb(*,i),format ="(i0,',',i0,',',i0)")
	endfor
  junk = max(p1(2,*), j)
  v_close = j			;index of closest vertex
;  print, 'v_close',v_close
  p = where(facevs eq v_close)/4   ;indices of closest verts
;  print, 'p facevs eq v_close',p
  for i=0,5 do begin		;draw faces
	k= (where(p eq i))(0) lt 0 	;1 = not close, 0 = close
	draw_cube, [0,0,0], dims-1, i
	endfor
  xstr=1.0+0.2*zscale & xsh=0.3*dxd/zscale

  if pmenu ne 'a' then begin $
;  for i=0,7 do xyouts, (xstr*p1(0,i)-xsh), $
;               (1.08*p1(1,i)-0.065*dyd), charsize=1.5,/DEVICE,s(i)
    junk = min(p1(1,*), j)
    if p0(1,j) eq 0 then dyx=-0.06*d1(1) $
      else dyx=0.06*d1(1) 
    xco0=[0.05*d1(0),p0(1,j)+dyx,(p0(2,j)-0.07*d1(2))]
    xco1=[0.92*d1(0),p0(1,j)+dyx,(p0(2,j)-0.07*d1(2))]
    xcox=[0.4*d1(0),p0(1,j)+1.5*dyx,(p0(2,j)-0.09*d1(2))]
    px0=convert_coord(xco0,/T3D, /TO_DEV, /DATA)
    px1=convert_coord(xco1,/T3D, /TO_DEV, /DATA)
    px = convert_coord(xcox,/T3D, /TO_DEV, /DATA)
    spx0 = string(xmin,format ="(i0)")
    spx1 = string(xmax,format ="(i0)")
    xyouts, px0(0), px0(1), spx0, charsize=1.5,/device
    xyouts, px1(0), px1(1), spx1, charsize=1.5,/device
    xyouts, px(0),px(1),'X',charsize=2.0,charthick=3,/device

    if p0(0,j) eq 0 then dxy=-0.06*d1(0) $
      else dxy=0.06*d1(0) 
    yco0=[p0(0,j)+dxy,0.08*d1(1),(p0(2,j)-0.07*d1(2))]
    yco1=[p0(0,j)+dxy,0.94*d1(1),(p0(2,j)-0.07*d1(2))]
    ycoy=[p0(0,j)+1.5*dxy,0.4*d1(1),(p0(2,j)-0.09*d1(2))]
    py0=convert_coord(yco0,/T3D, /TO_DEV, /DATA)
    py1=convert_coord(yco1,/T3D, /TO_DEV, /DATA)
    py = convert_coord(ycoy,/T3D, /TO_DEV, /DATA)
    spy0 = string(ymin,format ="(i0)")
    spy1 = string(ymax,format ="(i0)")
    xyouts, py0(0), py0(1), spy0, charsize=1.5,/device
    xyouts, py1(0), py1(1), spy1, charsize=1.5,/device
    xyouts, py(0),py(1),'Y',charsize=2.0,charthick=3,/device

    junk = max(p1(0,*), j)
;    print, 'min(p1(1,*), j)', j, min(p1(1,*))
    if p0(0,j) eq 0 then dxz=-0.04*d1(0) $
      else dxz=0.04*d1(0) 
    if p0(1,j) eq 0 then dyz=-0.04*d1(1) $
      else dyz=0.04*d1(1) 
    zco0=[p0(0,j)+dxz,p0(1,j)+dyz,0.05*d1(2)]
    zco1=[p0(0,j)+dxz,p0(1,j)+dyz,0.93*d1(2)]
    zcoz=[p0(0,j)+2*dxz,p0(1,j)+2*dyz,0.4*d1(2)]
    pz0=convert_coord(zco0,/T3D, /TO_DEV, /DATA)
    pz1=convert_coord(zco1,/T3D, /TO_DEV, /DATA)
    pz = convert_coord(zcoz,/T3D, /TO_DEV, /DATA)
    spz0 = string(zmin,format ="(i0)")
    spz1 = string(zmax,format ="(i0)")
    xyouts, pz0(0), pz0(1), spz0, charsize=1.5,/device
    xyouts, pz1(0), pz1(1), spz1, charsize=1.5,/device
    xyouts, pz(0),pz(1),'Z',charsize=2.0,charthick=3,/device
  endif 
  if pmenu eq 'a' then begin $
    px = convert_coord([d1(0)/5,-d1(1)/16,-d1(2)/16], $
          /T3D, /TO_DEV, /DATA)
    py = convert_coord([-d1(0)/16,d1(1)/5,-d1(2)/16], $
          /T3D, /TO_DEV, /DATA)
    pz = convert_coord([-d1(0)/16,-d1(1)/16,d1(2)/3], $
          /T3D, /TO_DEV, /DATA)
    xyouts, px(0),px(1),'X',charsize=2.0,charthick=3,/device
    xyouts, py(0),py(1),'Y',charsize=2.0,charthick=3,/device
    xyouts, pz(0),pz(1),'Z',charsize=2.0,charthick=3,/device
  endif 
  
  z_last = tvrd()
  zb_last = tvrd(CHANNEL=1, /WORDS)
  show
;  print,'end erase'
end




PRO show, image
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu
;  print,'in show'
  if n_params() eq 0 then begin
	set_plot,'Z'
	image = tvrd()
	endif
  set_plot,'x'
  tv, image
  end



;----------------------------------------------------------
; START OF MAIN SUBROUTINE
PRO PLOT3D
  COMMON field, a, xmin,xmax,ymin,ymax,zmin,zmax
  COMMON heini,dims,amin,amax,axex,axrev,p0cube,p0,p1,pb, $
         v_close,vfaces,facevs,z_last,zb_last,mode, $
         idir,islice,iaxes,ax,az,zscale,persp,isop, $
         resolution, pmenu

mode = 0
mode_names = [ 'Slices', 'Isosurface', $
               'Colors', 'Rotations', 'Postscript']
nmodes = n_elements(mode_names)		;# of modes
isop = { ISOP, hi_lo : 1, value: 50.0, window : 0, drawable : 0L, $
         slider : 0L, xs : fltarr(2) }
  axex=intarr(3)               ;TRUE to reverse axes
  axrev=intarr(3)              ;Axis permutations
  p0cube=intarr(3,2) 	       ;Corner coords of cube selection
  cube_on=0		       ;If cube is on
  cube_ip=0		       ;Corner of cube
  p0=fltarr(3,8)	       ;Data coords of cube corners
  p1=fltarr(3,8)	       ;Device coords of cube corners
  pb=fltarr(3,8)	       ;Phys coords of cube corners
  v_close=0 		       ;Index of closest vertex
  vfaces=intarr(3,8)	       ;Face index vs vertex index
  facevs=intarr(4,6)	       ;Vertex index vs faces
  xyz=intarr(2,3) & pxyz=fltarr(2,3) ;corners of region
  
    s=size(a)    
    dims=s(1:3) & d1=dims-1
    amax=max(a) & amin=min(a) & thresh=amin+0.5*(amax-amin)    
    print,'rho:',amin,amax,thresh
;    if amax-amin gt 10.0 then begin
;      amax=1.0
;      amin=0.0
;      print, 'max and min rho reset to  min=',amin,'   max=',amax
;    endif
    set_plot,'x'
    device, get_screen = resolution
    print, resolution
;    resolution(0) = 5 * resolution(0) / 9
;    resolution(1) = 4 * resolution(0) / 5
    resolution(0) = 7 * resolution(0) / 9
    resolution(1) = 6 * resolution(0) / 7
;    resolution(0) = 6 * resolution(0) / 9
;    resolution(1) = 5 * resolution(0) / 6
    print, resolution
    set_plot,'Z'
    z_last = 0
    zb_last = 0    
    device, set_resolution = resolution
    set_plot,'x'
    window, 0, XSIZE=resolution(0), YSIZE=resolution(1)
    window, 1, XPOS=45, YPOS=800, XSIZE=200, YSIZE=200
    wset, 0
;  Faces vs vertex index
   vfaces = [[0,1,2], [1,2,3], [0,2,4], [2,3,4], $
             [0,1,5], [1,3,5], [0,4,5], [3,4,5]]
;  Vertex indices vs faces  (clockwise order).
   facevs = [ [0,2,6,4], [0,4,5,1], [2,0,1,3], $ 
                 [1,5,7,3], [3,7,6,2], [6,7,5,4]]
; vertex numbers vs Edge index (12 edges)
   edges = [[0,1],[1,3],[2,3],[0,2], $
               [0,4],[1,5],[2,6],[3,7], $
	       [4,5],[5,7],[6,7],[4,6]]
   p0cube = [[dims/4], [3 * dims/4]]
   xyz(0,*)=fix([xmin,ymin,zmin])
   xyz(1,*)=fix([xmax,ymax,zmax])
   pxyz(0,*)=([0,0,0])
   pxyz(1,*)=d1
   phelp=intarr(3,8) 
   for i=0,7 do phelp(*,i) = $		;Data coords of all corners
	           [(i and 1), (i and 2)/2 , (i and 4)/4]
   for k=0,2 do pb(k,*)=xyz(phelp(k,*),k)
   for k=0,2 do p0(k,*)=pxyz(phelp(k,*),k)
   orthop = [ 0., 0., 1., -dims(2)/2.]	;Initial orthogonal plane

    pmenu='o' & oldmenu=pmenu
    porient='n' & b2orient=1 & iaxes=6
    ax=20.0 & az=75.0 & zscale=1.2 & persp=1.2 & ino=0.0
    pslice='n' & kslice='n' & idir=0 & islice=dims(idir)/2
    pcolor='n' & piso='n' & isoin=58.0 & isolevel=0.05
    isop.value=isolevel/100.0*(amax-amin)+amin & isop.hi_lo=1
    panim='n'
    ppost='n' & pundo='n' & perase='n'
    pquit='n'
    
    orient
    erasep
    
menu:
  print, 'Menu Options:'
  print, '         o -> Orientation'
  print, '         c -> Color Table'
  print, '         s -> Slice'
  print, '         i -> Isosurface'
  print, '         a -> Animation'
  print, '         g -> GIF output'
  print, '         p -> Postscript Color'
  print, '         w -> Postscript Blackand White'
  print, '         u -> Undo last Plot'
  print, '         e -> Erase'
  print, '         q -> Terminate'
  print, '    return -> no changes applied'
  print, 'Present Choice: ', pmenu
  read, pmenu & if pmenu eq '' then pmenu=oldmenu
  if (pmenu ne 'o') and (pmenu ne 'c') and (pmenu ne 's') and $
     (pmenu ne 'i') and (pmenu ne 'a') and (pmenu ne 'p') and $
     (pmenu ne 'g') and (pmenu ne 'b')  and (pmenu ne 'w') and $
     (pmenu ne 'u') and (pmenu ne 'e') and (pmenu ne 'q') $
                                           then goto, menu
  oldmenu=pmenu
  case pmenu of
  'o': begin
     morient: 
       print, 'Present Orientation: iaxes=',iaxes
       print, '                        ax=',ax
       print, '                        az=',az
       print, '                    zscale=',zscale
       print, '               perspective=',persp
       print, 'Orientation Options:'
       print, '    return -> Orientation OK' 
       print, '         a -> Axis manipulation with'
       print, '  iaxes: 0=xy exchange 1=xz exchange 2=yz exchange'
       print, '         3=x reverse   4=y reverse   5=z reverse'
       print, '         x -> Rotation Around x Axis'
       print, '         z -> Rotation Around z Axis'
       print, '         s -> zscale>0!'
       print, '         p -> Perspective'
       read, porient 
       if (porient ne 'x') and (porient ne 'z') and $
          (porient ne 'a') and (porient ne 's') and $
          (porient ne 'p') and (porient ne '' ) $
                           then goto, morient
       if (porient ne '' ) then begin
         print, 'Input Value?' 
         case porient of
           'a': read, iaxes 
           'x': read, ax 
           'z': read, az 
           's': read, zscale 
           'p': read, persp 
         endcase
         if (iaxes lt 0) or (iaxes gt 6) then begin
           print, 'iaxes=', iaxes, ' out of range; RESEST to 6'
           iaxes=6 & endif
         if zscale le 0 then begin
           print, 'zscale =',zscale,' < 0 !'
           zscale=1.0 & endif
         goto, morient 
       endif
       orient
       erasep
     end
  'c': begin & xloadct,file='/home/ao/idl_lib/colortab.priv',block=1 & orient & end
  's': begin
     mslice:
       print, 'Present Settings: '
       print, '        Cut at plane=',idir
       print, '               index=',islice
       print, '            maxindex=',dims(idir)-1
       print, 'Slice Options:'
       print, '    return -> Settings OK, GO' 
       print, '         x -> cut through y,z plane'
       print, '         y -> cut through x,z plane'
       print, '         z -> cut through x,y plane'
       print, '         i -> new index of plane'
       print, 'Input Value?' & read, pslice 
       if (pslice ne 'x') and (pslice ne 'y') and $
          (pslice ne 'z') and (pslice ne 'i') and $
          (pslice ne '' ) then goto, mslice
       if (pslice ne '' ) then begin
         if (pslice eq 'x') then idir=0
         if (pslice eq 'y') then idir=1
         if (pslice eq 'z') then idir=2
         print, 'Input Plane Index or Return'
         read, kslice
         if (kslice ne '' ) then begin
           islice=fix(kslice)
           if (islice lt 0) or (islice gt (dims(idir)-1)) then begin
             print, 'index:',islice,' not possible; RESET!'
             islice=dims(idir)/2 & endif
         endif
       endif
       draw_slice
     end         
  'i': begin
     miso:
       histo
       print, 'Present Settings: '
       print, '      --> Histogram'
       print, '      Min =',amin, '   Max =',amax
       print, '      Surface at level=',isop.value
       print, '             hilo[0,1]=',isop.hi_lo
       print, 'Options:'
       print, '    return -> Settings OK, GO' 
       print, '         s -> Surface Level'
       print, '         h -> Surface cut low or high=[0,1]'
       print, 'Input s or h?' & read, piso 
       if (piso ne 's') and (piso ne 'h') and $
          (piso ne '' ) then goto, miso
       if (piso ne '' ) then begin
         case piso of
          's': begin
             print, 'Input Surface Level in %'
             read, isolevel
             if (isolevel lt 0) or (isolevel gt 100) then begin
               print, 'level',isolevel,' not possible; RESET to 50!'
               isolevel=0.05
             endif
             isop.value=isolevel/100.0*(amax-amin)+amin
             end
          'h': begin
             print, 'Input High(1) or Low(0) Side'
             read, hl
             isop.hi_lo=hl
             if (isop.hi_lo ne 0) and (isop.hi_lo ne 1) then begin
               print, 'Input HiLo not possible; RESET to 0'
               isop.hi_lo=0
             endif
             end
         endcase
         goto, miso 
       endif
       orient
       isosurf
     end
  'a': anim  
  'g': gif
  'p': post
  'w': postbw
  'u': undo
  'e': erasep
  'q': stop
  endcase
  goto, menu 

end


