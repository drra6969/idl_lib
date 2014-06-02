; START OF MAIN PROGRAM

  nx=21 & ny=21 & nz=2 
  nxf=51 & nzf=51
  mx=1 & my=1  & mz=1 
  b2=bytarr(2)
  x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO) & z=fltarr(nz,/NOZERO)
  ioxf=fltarr(nxf) & iozf=fltarr(nzf)
  f1=fltarr(nx,nz) & f2=f1 & f3=f1
  fa=fltarr(nxf,nzf) & fb=fa & fc=fa

  pot=fltarr(nx,ny,nz,/NOZERO)
  time=0.0 & fnumber=1
  name='' & contin='' & again='y' & withps='n' & run=''

  dumm='' & rest= nx mod 9 & nrows=(nx-rest) / 9 
  print, 'nrows= ', nrows, '         rest= ', rest
  xdx=fltarr(2,nx) & ydy=fltarr(2,ny) & zdz=fltarr(2,nz) 

  hf=fltarr(9,nz) & hfrest=fltarr(rest,nz)

  print, 'Input filenumber'
  read, fnumber
  name='mpot'+string(fnumber,'(i1)')
  openr, 8, name
  readf, 8, dumm
  readf, 8, mx,my,mz,time
  if (mx ne nx) or (my ne ny) or (mz ne nz) then begin
     print, 'nx <> mx   or    ny <> my   or    nz <> mz ='
     print, 'expected dimension nx=',mx,'   ny=',my,'   nz=',mz
  endif
  readf, 8, dumm
  readf, 8, dumm
  readf, 8, xdx
  readf, 8, dumm
  readf, 8, dumm
  readf, 8, ydy
  readf, 8, dumm
  readf, 8, dumm
  readf, 8, zdz
  x=xdx(0,0:nx-1) & g1=xdx(1,0:nx-1)
  y=ydy(0,0:ny-1) & h1=ydy(1,0:ny-1)
  z=zdz(0,0:nz-1) & i1=zdz(1,0:nz-1)
;rho
 for iy=0,ny-1 do begin
  readf, 8, dumm
  readf, 8, dumm
  for k=0, nrows-1  do begin
    readf, 8, dumm
    readf, 8, hf
    for ix=0,8 do $
     for iz=0,nz-1 do pot(ix+k*9,iy,iz)=hf(ix,iz)
  endfor
  readf, 8, dumm
;  print, dumm
  readf, 8, hfrest
  for ix=0,rest-1 do $
   for iz=0,nz-1 do pot(ix+nrows*9,iy,iz)=hfrest(ix,iz)
  print, 'pot',iy,y(iy)
 endfor

  close, 8

;   print, mx,my,mpos,mrow
;  print, x
;  print, y

;----PARAMETER----
  xmin = -60  &   ymin = 0
  xmax =  30 &   ymax = 200
;  xmin = -14  &   ymin = y(2)
;  xmax =  14  &   ymax = 150  

;  xminn = xmin &  yminn = ymin  ;for interpolated fields(vector)
;  xmaxn = xmax &  ymaxn = ymax   
  xminn = xmin+1 &  yminn = ymin+1  ;for interpolated fields(vector)
  xmaxn = xmax-1 &  ymaxn = ymax-1   
  fall = 'PP'

; generation of new grid for vectorpotential
  xf=findgen(nxf) & yf=findgen(nzf)
  dxf=(xmax-xmin)/float(nxf-1) & xf=xf*dxf+xmin
;  print, dxf,xf
  dzf=(zmax-zmin)/float(nzf-1) & zf=zf*dzf+zmin
;  print, dzf,zf
 
  in=-1 & k=0
  repeat begin
    in=in+1
    while xf(in) gt x(k+1) do k=k+1
    ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
;  print, in, xf(in), k, x(k), ioxf(in)      
  endrep until in eq nxf-1
  in=-1 & k=0
  repeat begin
    in=in+1
    while zf(in) gt z(k+1) do k=k+1
    iozf(in) = float(k) + (zf(in)-z(k))/(z(k+1)-z(k))        
;    print, in, znf(in), k, z(k), iozf(in)      
  endrep until in eq nzf-1

gridindex:
  print, plane, 'Coordinates:
  for i=0,nplane-1,2 do print, i,'  ',plane,'=',coord(i)
  print, 'Input - i =Grid Index of Chosen Plane(>0 and <',nplane-1,')'
  print, 'Options:   integer -> grid index'
  print, '            return -> no changes applied'
  print, '                 q -> terminate'
  print, 'Present Choice: ', igrid
  read, newgrid 
  if newgrid eq 'q' then stop
  if newgrid ne '' then igrid=fix(newgrid)
  if newgrid eq '' then print,'choice=',igrid,' not altered'
  if (igrid le 0) or (igrid ge ny-1) then igrid=(nplane+1)/2  

    cutata='z=' & cutatb=string(y(igrid),'(f7.2)')
    xtit='x'    & ytit='y'    & xchoice=x   & ychoice=y
    xpmin=xmin  & xpmax=xmax  & ypmin=ymin  & ypmax=ymax
    f1=pot(*,igrid,*) & npl=1
    if igrid+1 le ny-1 then  begin & f2=pot(*,igrid+1,*) & npl=2 & endif
    if igrid+2 le ny-1 then  begin & f3=pot(*,igrid+2,*) & npl=3 & endif
;    fc1=f1(*,igrid,*) & fc1=reform(fc1) & fc1=rotate(fc1,4)
    xplot=x  &  yplot=y

grausig:
  print, 'With postscript (output in p3.ps)? '
  read, withps
  if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='p3.ps'
        if (xpmax-xpmin) ge (ypmax-ypmin) then begin 
          device,/portrait
          device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5 
          device,/inches,ysize=10.,scale_factor=1.0,yoffset=0.5
        endif
        if (xpmax-xpmin) lt (ypmax-ypmin) then begin 
          device,/landscape
;          device,/inches,ysize=8.,scale_factor=1.0,xoffset=0.5
;          device,/inches,xsize=10.0,scale_factor=1.0,yoffset=0.5
        endif
        device,/times,/bold,font_index=3
  endif
  xpos =xpmax+0.02*(xpmax-xpmin)
  yposa=ypmin+0.3*(ypmax-ypmin)
  yposb=ypmin+0.22*(ypmax-ypmin)
  ypos1=ypmin+0.4*(ypmax-ypmin)
  ypos2=ypmin+0.6*(ypmax-ypmin)
  ypos3=ypmin+0.75*(ypmax-ypmin)
  ypos4=ypmin+0.67*(ypmax-ypmin)
  ypos5=ypmin+0.95*(ypmax-ypmin)
  ypos6=ypmin+0.87*(ypmax-ypmin)
  xpost=xpmin+0.2*(xpmax-xpmin)
  ypost=ypmax+0.01*(ypmax-ypmin)
  if (xpmax-xpmin) ge (ypmax-ypmin) then begin 
   if (xpmax-xpmin)/(ypmax-ypmin) gt 2.0 then $
                              hoch=0.2 else hoch=0.25
   if (xpmax-xpmin)/(ypmax-ypmin) lt 3.0 then $
     breit=1.25*(xpmax-xpmin)/(ypmax-ypmin)*hoch else breit=.75
      !P.MULTI=[0,0,3]
      pos1=[0.1,0.0,0.1+breit,0.0+hoch]
      pos2=[0.1,0.35,0.1+breit,0.35+hoch]
      pos3=[0.1,0.7,0.1+breit,0.7+hoch]
  endif
  if (xpmax-xpmin) lt (ypmax-ypmin) then begin 
   if (ypmax-ypmin)/(xpmax-xpmin) gt 2.0 then $
                              breit=0.2 else breit=0.25
   if (ypmax-ypmin)/(xpmax-xpmin) lt 3.0 then $
     hoch=1.25*(ypmax-ypmin)/(xpmax-xpmin)*breit else hoch=.75
      !P.MULTI=[0,npl,0]
      pos1=[0.0,0.1,0.0+breit,0.1+hoch]
      pos2=[0.35,0.1,0.35+breit,0.1+hoch]
      pos3=[0.7,0.1,0.7+breit,0.1+hoch]
  endif
       
       
  print, 'Plot First Page (Contour Lines)?'
  print, 'Options:         y or return'
  print, 'Or:              s -> plot with smoothing'
  print, '                 f -> back to fieldgroup'
  print, '                 c -> back to cut'
  print, '                 i -> back to grid index'
  print, '                 q -> terminate'
  read, contin
  if contin eq 'q' then stop
  if contin eq 'i' then goto, gridindex
  if (contin eq '' or contin eq 'y' or contin eq 's') then begin
    !P.REGION=[0.,0.,1.0,1.25]
    !P.CHARSIZE=2.0

    if contin eq 's' then fp1=smooth(f1,3) else fp1=f1 
    fpmax=max(fp1) & fpmin=min(fp1)
    if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/20. & if delf lt 0.000000001 then delf=0.0001
    !P.POSITION=pos1
    contour,fp1,xchoice,ychoice,levels=findgen(21)*delf+fpmin,$
    c_linestyle=findgen(21)*delf+fpmin lt fptr,$
    xrange=[xpmin,xpmax],yrange=[ypmin,ypmax],$
    title=head1,xstyle=1,ystyle=1,$
    xtitle=xtit,ytitle=ytit,font=2
    xyouts,xpos,yposa,cutata,font=2
    xyouts,xpos,yposb,cutatb,font=2
    xyouts,xpos,ypos1,'t='+string(time,'(i3)'),font=2
    xyouts,xpos,ypos2,' '+run,font=2
    xyouts,xpos,ypos3,'Max=',font=2
    xyouts,xpos,ypos4,' '+string(fpmax,'(f7.3)'),font=2
    xyouts,xpos,ypos5,'Min=',font=2
    xyouts,xpos,ypos6,' '+string(fpmin,'(f7.3)'),font=2

    if npl eq 2 then begin
     if contin eq 's' then fp2=smooth(f2,3) else fp2=f2 
     fpmax=max(fp2) & fpmin=min(fp2)
     if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
     delf=(fpmax-fpmin)/20. & if delf lt 0.000000001 then delf=0.0001
     !P.POSITION=pos2
     contour,fp2,xchoice,ychoice,levels=findgen(21)*delf+fpmin,$
     c_linestyle=findgen(21)*delf+fpmin lt fptr,$
     xrange=[xpmin,xpmax],yrange=[ypmin,ypmax],$
     title=head2,xstyle=1,ystyle=1,$
     xtitle=xtit,ytitle=ytit,font=2
     xyouts,xpos,yposa,cutata,font=2
     xyouts,xpos,yposb,cutatb,font=2
     xyouts,xpos,ypos1,'t='+string(time,'(i3)'),font=2
     xyouts,xpos,ypos2,' '+run,font=2
     xyouts,xpos,ypos3,'Max=',font=2
     xyouts,xpos,ypos4,' '+string(fpmax,'(f7.3)'),font=2
     xyouts,xpos,ypos5,'Min=',font=2
     xyouts,xpos,ypos6,' '+string(fpmin,'(f7.3)'),font=2
    endif

    if npl eq 2 then begin
     if contin eq 's' then fp3=smooth(fc3,3) else fp3=fc3 
     fpmax=max(fp3) & fpmin=min(fp3)
     if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
     delf=(fpmax-fpmin)/20. & if delf lt 0.000000001 then delf=0.0001
     !P.POSITION=pos3
     contour,fp3,xchoice,ychoice,levels=findgen(21)*delf+fpmin,$
     c_linestyle=findgen(21)*delf+fpmin lt fptr,$
     xrange=[xpmin,xpmax],yrange=[ypmin,ypmax],$
     title=head3,xstyle=1,ystyle=1,$
     xtitle=xtit,ytitle=ytit,font=2
     xyouts,xpos,yposa,cutata,font=2
     xyouts,xpos,yposb,cutatb,font=2
     xyouts,xpos,ypos1,'t='+string(time,'(i3)'),font=2
     xyouts,xpos,ypos2,' '+run,font=2
     xyouts,xpos,ypos3,'Max=',font=2
     xyouts,xpos,ypos4,' '+string(fpmax,'(f7.3)'),font=2
     xyouts,xpos,ypos5,'Min=',font=2
     xyouts,xpos,ypos6,' '+string(fpmin,'(f7.3)'),font=2
    endif

    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        print,'postscipt file p3.ps closed'
        set_plot,'x' & withps='' & endif
    endif
  endif

  print, 'Plot 2. Page (Surface Plots)?'
  print, 'Options:         y or return'
  print, 'Or:              s -> plot with smoothing'
  print, '                 i -> back to grid index'
  print, '                 p -> back to postscript'
  print, '                 q -> terminate'
  read, contin
  if contin eq 'q' then stop
  if contin eq 'i' then goto, gridindex
  if contin eq 'p' then goto, grausig
  if (contin eq '' or contin eq 'y' or contin eq 's') then begin
      fa=interpolate(f1,ioxf,iozf,/grid)
      if npl eq 2 then fb=interpolate(f2,ioxf,iozf,/grid)
      if npl eq 3 then fc=interpolate(f3,ioxf,iozf,/grid)
      xsu=xf & ysu=zf & endif 
    erase
    if contin eq 's' then begin
      fa=smooth(fa,3) 
      if npl eq 2 then fb=smooth(fb,3) 
      if npl eq 3 then fc=smooth(fc,3) & endif
    surface,fa,xsu,ysu,position=[0.1,0.1,0.45,0.4],ax=35,$
      ztitle=head1,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    if npl eq 2 then $
    surface,fb,xsu,ysu,position=[0.6,0.35,0.95,0.65],ax=35,$
      ztitle=head2,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    if npl eq 3 then $
    surface,fc,xsu,ysu,position=[0.1,0.65,0.45,0.95],ax=35,$
      ztitle=head3,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        print,'postscipt file p3.ps closed'
        set_plot,'x' & withps='' & endif
;    endif
  endif

  print, 'Further plots?'
  print, 'Options:         y or return'
  print, '                 i -> back to grid index'
  print, '                 p -> back to postscript'
  print, '                 q -> terminate'
  read, contin
  if contin eq 'p' then goto, grausig
  if (contin eq '' or contin eq 'y' or contin eq 'i') then  goto, gridindex


end

