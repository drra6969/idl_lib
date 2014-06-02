; START OF MAIN PROGRAM

  nx=101 & ny=21 & nz=2 
  nxf=51 & nyf=51 & nzf=51
  mx=1 & my=1  & mz=1 

  time=0.0 & fnumber=1
  name='' & contin='' & again='y' & withps='n' & run=''
  dumm='' 

  print, 'Input filenumber'
  read, fnumber
  name='mpot'+string(fnumber,'(i1)')
  openr, 8, name
  readf, 8, dumm
    readf, 8, nx,ny,nz,time
      x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO) 
      z=fltarr(nz,/NOZERO)
      pot=fltarr(nx,ny,nz,/NOZERO)
      rest= nx mod 9 & nrows=(nx-rest) / 9 
      print, 'nrows= ', nrows, '         rest= ', rest
      xdx=fltarr(nx) & ydy=fltarr(ny) & zdz=fltarr(nz) 
      hf=fltarr(9,nz) & if (rest gt 0) then hfrest=fltarr(rest,nz)
      ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
      f1=fltarr(nx,nz) & f2=f1 & f3=f1
      fa=fltarr(nxf,nzf) & fb=fa & fc=fa

    readf, 8, dumm & readf, 8, dumm & readf, 8, xdx
    readf, 8, dumm & readf, 8, dumm & readf, 8, ydy
    readf, 8, dumm & readf, 8, dumm & readf, 8, zdz
    x=xdx(0:nx-1) & y=ydy(0:ny-1) & z=zdz(0:nz-1) 
;rho
    for iy=0,ny-1 do begin
     readf, 8, dumm
     readf, 8, dumm
     for k=0, nrows-1  do begin
       readf, 8, dumm & readf, 8, hf
       for ix=0,8 do $
        for iz=0,nz-1 do pot(ix+k*9,iy,iz)=hf(ix,iz)
     endfor
     if (rest gt 0) then begin 
       readf, 8, dumm
       readf, 8, hfrest
       for ix=0,rest-1 do $
        for iz=0,nz-1 do pot(ix+nrows*9,iy,iz)=hfrest(ix,iz)
;         print, 'pot',iy,y(iy)
     endif
    endfor
  close, 8

;  print, x
;  print, y
;  print, z

;----PARAMETER----
  print, ' max boundaries of plot: xmin/max=', x(0), x(nx-1) 
  print, '                         ymin/max=', y(0), y(ny-1) 
  print, '                         zmin/max=', z(0), z(nz-1)
  xmin = x(0)    &  ymin = y(0)    &  zmin = z(0)
  xmax = x(nx-1) &  ymax = y(ny-1) &  zmax = z(nz-1)
;  xmin = -2. &  ymin = 30.0 &  zmin = z(0)
;  xmax = 2. &  ymax = 38.0 &  zmax = z(nz-1)

  print, ' Actual boundaries of plot: xmin/max=', xmin, xmax 
  print, '                            ymin/max=', ymin, ymax 
  print, '                            zmin/max=', zmin, zmax 

; generation of new grid for contourplots
  xf=findgen(nxf) & yf=findgen(nyf) & zf=findgen(nzf)
  dxf=(xmax-xmin)/float(nxf-1) & xf=xf*dxf+xmin
  dyf=(ymax-ymin)/float(nyf-1) & yf=yf*dyf+ymin
  dzf=(zmax-zmin)/float(nzf-1) & zf=zf*dzf+zmin
  xf(nxf-1)=xmax & yf(nyf-1)=ymax & zf(nzf-1)=zmax 
 
  in=-1 & k=0
  repeat begin
    in=in+1
    while xf(in) gt x(k+1) do k=k+1
    ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
  endrep until in eq nxf-1
  in=-1 & k=0
  repeat begin
    in=in+1
    while yf(in) gt y(k+1) do k=k+1
    ioyf(in) = float(k) + (yf(in)-y(k))/(y(k+1)-y(k))        
  endrep until in eq nyf-1
  in=-1 & k=0
  repeat begin
    in=in+1
    while zf(in) gt z(k+1) do k=k+1
    iozf(in) = float(k) + (zf(in)-z(k))/(z(k+1)-z(k))        
  endrep until in eq nzf-1

  igrid=0 & newgrid='0' & withps='n' & closeps='n'
  whatcut='10' & plane='z' & head1='Energy'
  
cut:
  print, 'Input - What Cut Through The 3-D System:'
  print, 'Options: x -> y,z-plane, x=const'
  print, '         y -> z,x-plane, y=const'
  print, '         z -> x,y-plane, z=const'
  print, '    return -> no changes applied'
  print, '         q -> terminate'
  print, 'Present Choice: ', plane
  read, whatcut
  if whatcut eq 'q' then stop
  if whatcut eq 'x' then plane='x'
  if whatcut eq 'y' then plane='y'
  if whatcut eq 'z' then plane='z'
  if whatcut eq '' then print,'choice=',plane,' not altered'
  if plane eq 'x' then begin & nplane=nx & coord=x & endif
  if plane eq 'y' then begin & nplane=ny & coord=y & endif
  if plane eq 'z' then begin & nplane=nz & coord=z & endif

gridindex:
  print, plane, 'Coordinates:
  for i=0,nplane-1 do print, i,'  ',plane,'=',coord(i)
  print, 'Input - i =Grid Index of Chosen Plane(>0 and <',nplane-1,')'
  print, 'Options:   integer -> grid index'
  print, '            return -> no changes applied'
  print, '                 c -> back to cut'
  print, '                 q -> terminate'
  print, 'Present Choice: ', igrid
  read, newgrid 
  if newgrid eq 'q' then stop
  if newgrid eq 'c' then goto, cut
  if newgrid ne '' then igrid=fix(newgrid)
  if newgrid eq '' then print,'choice=',igrid,' not altered'
  if (igrid lt 0) or (igrid gt nplane-1) then igrid=0
  npl=3
  if (igrid gt nplane-3) then npl=2
  if (igrid gt nplane-2) then npl=1

  cutatb1='  ' & cutatb2='  ' 
  if plane eq 'x' then begin
    cutata='x=' & cutatb0=string(x(igrid),'(f7.2)')
    if npl ge 2 then cutatb1=string(x(igrid+1),'(f7.2)')
    if npl eq 3 then cutatb2=string(x(igrid+2),'(f7.2)')
    xtit='y'    & ytit='z'    & xchoice=y   & ychoice=z
    xpmin=ymin  & xpmax=ymax  & ypmin=zmin  & ypmax=zmax 
    fc1=pot(igrid,*,*) & fc1=reform(fc1) 
    if npl ge 2 then begin
       fc2=pot(igrid+1,*,*) & fc2=reform(fc2) & endif
    if npl eq 3 then begin
      fc3=pot(igrid+2,*,*) & fc3=reform(fc3) & endif
    xplot=y  &  yplot=z
  endif
  if plane eq 'y' then begin
    cutata='y=' & cutatb0=string(y(igrid),'(f7.2)')
    if npl ge 2 then cutatb1=string(y(igrid+1),'(f7.2)')
    if npl eq 3 then cutatb2=string(y(igrid+2),'(f7.2)')
    xtit='z'    & ytit='x'    & xchoice=z   & ychoice=x
    xpmin=zmin  & xpmax=zmax  & ypmin=xmin  & ypmax=xmax 
    fc1=pot(*,igrid,*) & fc1=reform(fc1) & fc1=rotate(fc1,4)
    if npl ge 2 then begin
      fc2=pot(*,igrid+1,*) & fc2=reform(fc2) & fc2=rotate(fc2,4)
    endif  
    if npl eq 3 then begin
      fc3=pot(*,igrid+2,*) & fc3=reform(fc3) & fc3=rotate(fc3,4)
    endif  
    xplot=z  &  yplot=x
  endif
  if plane eq 'z' then begin
    cutata='z=' & cutatb0=string(z(igrid),'(f7.2)')
    if npl ge 2 then cutatb1=string(z(igrid+1),'(f7.2)')
    if npl eq 3 then cutatb2=string(z(igrid+2),'(f7.2)')
    xtit='x'    & ytit='y'    & xchoice=x   & ychoice=y
    xpmin=xmin  & xpmax=xmax  & ypmin=ymin  & ypmax=ymax 
    fc1=pot(*,*,igrid)    & fc1=reform(fc1)
    if npl ge 2 then begin
      fc2=pot(*,*,igrid+1) & c2=reform(fc2) & endif
    if npl eq 3 then begin
      fc3=pot(*,*,igrid+2) & fc3=reform(fc3) & endif
    xplot=x  &  yplot=y
  endif

grausig:
  print, 'With postscript (output in pot.ps)? '
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
                              hoch=0.2 else hoch=0.225
   if (xpmax-xpmin)/(ypmax-ypmin) lt 3.0 then $
     breit=1.125*(xpmax-xpmin)/(ypmax-ypmin)*hoch else breit=.75
      !P.MULTI=[0,0,3]
      pos1=[0.1,0.04,0.1+breit,0.04+hoch]
      pos2=[0.1,0.37,0.1+breit,0.37+hoch]
      pos3=[0.1,0.7,0.1+breit,0.7+hoch]
  endif
  if (xpmax-xpmin) lt (ypmax-ypmin) then begin 
   if (ypmax-ypmin)/(xpmax-xpmin) gt 2.0 then $
                              breit=0.2 else breit=0.25
   if (ypmax-ypmin)/(xpmax-xpmin) lt 3.0 then $
     hoch=1.25*(ypmax-ypmin)/(xpmax-xpmin)*breit else hoch=.75
      !P.MULTI=[0,npl,0]
      pos1=[0.04,0.1,0.04+breit,0.1+hoch]
      pos2=[0.37,0.1,0.37+breit,0.1+hoch]
      pos3=[0.7,0.1,0.7+breit,0.1+hoch]
      yposb=ypmin+0.25*(ypmax-ypmin)
      ypos4=ypmin+0.7*(ypmax-ypmin)
      ypos6=ypmin+0.9*(ypmax-ypmin)
  endif

  print, 'Plot First Page (Contour Lines)?'
  print, 'Options:         y or return'
  print, 'Or:              s -> plot with smoothing'
  print, '                 i -> back to grid index'
  print, '                 c -> back to cut'
  print, '                 q -> terminate'
  read, contin
  if contin eq 'q' then stop
  if contin eq 'i' then goto, gridindex
  if contin eq 'c' then goto, cut
  if (contin eq '' or contin eq 'y' or contin eq 's') then begin
    !P.REGION=[0.,0.,1.0,1.25]
    if npl eq 3 then !P.CHARSIZE=2.0
    if npl lt 3 then !P.CHARSIZE=1.0

    if contin eq 's' then fp1=smooth(fc1,3) else fp1=fc1 
    fpmax=max(fp1) & fpmin=min(fp1) & fabs=max([abs(fpmax),abs(fpmin)])
     if (fpmax*fpmin/fabs/fabs lt -0.2) then fptr=0.0 $
                                        else fptr=0.5*(fpmax+fpmin)
;    fptr=0.0 
    delf=(fpmax-fpmin)/10. & if delf lt 0.000000001 then delf=0.0001
    !P.POSITION=pos1
    contour,fp1,xchoice,ychoice,levels=findgen(11)*delf+fpmin, $
    c_linestyle=findgen(11)*delf+fpmin lt fptr, $
    xrange=[xpmin,xpmax],yrange=[ypmin,ypmax], $
    title=head1,xstyle=1,ystyle=1, $
    xtitle=xtit,ytitle=ytit,font=2
    xyouts,xpos,yposa,cutata,font=2
    xyouts,xpos,yposb,cutatb0,font=2
    xyouts,xpos,ypos1,'t='+string(time,'(i3)'),font=2
    xyouts,xpos,ypos2,' '+run,font=2
    xyouts,xpos,ypos3,'Max=',font=2
    xyouts,xpos,ypos4,' '+string(fpmax,'(f7.3)'),font=2
    xyouts,xpos,ypos5,'Min=',font=2
    xyouts,xpos,ypos6,' '+string(fpmin,'(f7.3)'),font=2

    if npl ge 2 then begin
     if contin eq 's' then fp2=smooth(fc2,3) else fp2=fc2 
     fpmax=max(fp2) & fpmin=min(fp2) & fabs=max([abs(fpmax),abs(fpmin)])
     if (fpmax*fpmin/fabs/fabs lt -0.2) then fptr=0.0 $
                                        else fptr=0.5*(fpmax+fpmin)
;     fptr=0.0 
     delf=(fpmax-fpmin)/10. & if delf lt 0.000000001 then delf=0.0001
     !P.POSITION=pos2
     contour,fp2,xchoice,ychoice,levels=findgen(11)*delf+fpmin,$
     c_linestyle=findgen(11)*delf+fpmin lt fptr, $
     xrange=[xpmin,xpmax],yrange=[ypmin,ypmax], $
     title=head1,xstyle=1,ystyle=1, $
     xtitle=xtit,ytitle=ytit,font=2
     xyouts,xpos,yposa,cutata,font=2
     xyouts,xpos,yposb,cutatb1,font=2
     xyouts,xpos,ypos1,'t='+string(time,'(i3)'),font=2
     xyouts,xpos,ypos2,' '+run,font=2
     xyouts,xpos,ypos3,'Max=',font=2
     xyouts,xpos,ypos4,' '+string(fpmax,'(f7.3)'),font=2
     xyouts,xpos,ypos5,'Min=',font=2
     xyouts,xpos,ypos6,' '+string(fpmin,'(f7.3)'),font=2
    endif

    if npl eq 3 then begin
     if contin eq 's' then fp3=smooth(fc3,3) else fp3=fc3 
     fpmax=max(fp3) & fpmin=min(fp3) & fabs=max([abs(fpmax),abs(fpmin)])
     if (fpmax*fpmin/fabs/fabs lt -0.2) then fptr=0.0 $
                                        else fptr=0.5*(fpmax+fpmin)
;     fptr=0.0 
     delf=(fpmax-fpmin)/10. & if delf lt 0.000000001 then delf=0.0001
     !P.POSITION=pos3
     contour,fp3,xchoice,ychoice,levels=findgen(11)*delf+fpmin, $
     c_linestyle=findgen(11)*delf+fpmin lt fptr, $
     xrange=[xpmin,xpmax],yrange=[ypmin,ypmax], $
     title=head1,xstyle=1,ystyle=1, $
     xtitle=xtit,ytitle=ytit,font=2
     xyouts,xpos,yposa,cutata,font=2
     xyouts,xpos,yposb,cutatb2,font=2
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
        print,'postscipt file pot.ps closed'
        set_plot,'x' & withps='' & endif
    endif
  endif

  print, 'Plot 2. Page (Surface Plots)?'
  print, 'Options:         y or return'
  print, 'Or:              s -> plot with smoothing'
  print, '                 c -> back to cut'
  print, '                 i -> back to grid index'
  print, '                 p -> back to postscript'
  print, '                 q -> terminate'
  read, contin
  if contin eq 'q' then stop
  if contin eq 'c' then goto, cut
  if contin eq 'i' then goto, gridindex
  if contin eq 'p' then goto, grausig
  if (contin eq '' or contin eq 'y' or contin eq 's') then begin
    if plane eq 'x' then begin
      fa=interpolate(fc1,ioyf,iozf,/grid)
      if npl ge 2 then fb=interpolate(fc2,ioyf,iozf,/grid)
      if npl eq 3 then fc=interpolate(fc3,ioyf,iozf,/grid)
      xsu=yf & ysu=zf & endif 
    if plane eq 'y' then begin
      fa=interpolate(fc1,iozf,ioxf,/grid)
      if npl ge 2 then fb=interpolate(fc2,iozf,ioxf,/grid)
      if npl eq 3 then fc=interpolate(fc3,iozf,ioxf,/grid)
      xsu=zf & ysu=xf & endif 
    if plane eq 'z' then begin
      fa=interpolate(fc1,ioxf,ioyf,/grid)
      if npl ge 2 then fb=interpolate(fc2,ioxf,ioyf,/grid)
      if npl eq 3 then fc=interpolate(fc3,ioxf,ioyf,/grid)
      xsu=xf & ysu=yf & endif 
    erase
    if contin eq 's' then begin
      fa=smooth(fa,3)  
      if npl ge 2 then fb=smooth(fb,3)  
      if npl eq 3 then fc=smooth(fc,3)  
    endif
    surface,fa,xsu,ysu,position=[0.1,0.1,0.35,0.7,0,0.4],ax=45, $
      ztitle=head1,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    if npl ge 2 then $
      surface,fb,xsu,ysu,position=[0.6,0.35,0.85,0.95,0,0.4],ax=45, $
      ztitle=head1,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    if npl eq 3 then $
      surface,fc,xsu,ysu,position=[0.1,0.65,0.35,1.25,0,0.4],ax=45, $
      ztitle=head1,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        print,'postscipt file p3.ps closed'
        set_plot,'x' & withps='' & endif
    endif
  endif

  print, 'Further plots?'
  print, 'Options:         y or return'
  print, '                 c -> back to cut'
  print, '                 i -> back to grid index'
  print, '                 p -> back to postscript'
  print, '                 q -> terminate'
  read, contin
  if contin eq 'p' then goto, grausig
  if (contin eq 'i') then  goto, gridindex
  if (contin eq '' or contin eq 'y' or contin eq 'c') then  goto, cut

      
end

