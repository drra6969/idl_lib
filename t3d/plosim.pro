; START OF MAIN PROGRAM

  nxn=25 & nyn=25 & nzn=25     ;grid for arrow presentation
  nxf=41 & nyf=41 & nzf=41     ;grid for uniform interpol fields
  nx=long(131) & ny=long(126) & nz=long(126) 
  iox=fltarr(nxn) & ioy=fltarr(nyn) & ioz=fltarr(nzn)
  ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
  b2=bytarr(2)
  time=0.0 & fnumber=1
  name='' & contin='' & again='y' & withps='n' & run=''  
  closeps=''
    
;-----------PARAMETER----------------
  xmin = -10.0 & ymin = -10.0 & zmin = -10.0
  xmax =  10.0 & ymax =  10.0 & zmax =  10.0
;---------- VELOCIY vectors---------
  xn=findgen(nxn) & yn=findgen(nyn) & zn=findgen(nzn)
  dxn=(xmax-xmin)/float(nxn-1) &  xn=xn*dxn+xmin
  dyn=(ymax-ymin)/float(nyn-1) &  yn=yn*dyn+ymin
  dzn=(zmax-zmin)/float(nzn-1) &  zn=zn*dzn+zmin

;----------INPUT--------------------
  print, 'Input filenumber'
  read, fnumber
  print, 'Which case?'
  read, run
  name='magtap'+string(fnumber,'(i1)')
  openr, 8, name,/F77_UNFORMATTED
  readu, 8, nx, ny, nz, time
   print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz
   print, 'time=',time
  
  rho=fltarr(nx,ny,nz,/NOZERO) 
  x=fltarr(nx,/NOZERO) & dx=0.1 & dxh=0.1
  y=fltarr(ny,/NOZERO) & dy=0.1 & dyh=0.1
  z=fltarr(nz,/NOZERO) & dz=0.1 & dzh=0.1 
  
  readu, 8,  x,dx,dxh,dxh,y,dy,dyh,dyh,z,dz,dzh,dzh
; print, x,y,z
  readu, 8, rho
  print, 'xmin=', x(1), '  xmax=', x(nx-2)
  print, 'ymin=', y(1), '  ymax=', y(ny-2)
  print, 'zmin=', z(1), '  zmax=', z(nz-2)
  close, 8

  amin=min(rho) & amax=max(rho) &  thresh=amin+0.5*(amax-amin)
  print,'rho:',amin,amax,thresh

;----------GRID for uniform interpol------------
  xf=findgen(nxf) & yf=findgen(nyf) & zf=findgen(nzf) 
  dxf=(xmax-xmin)/float(nxf-1) &  xf=xf*dxf+xmin
  dyf=(ymax-ymin)/float(nyf-1) &  yf=yf*dyf+ymin
  dzf=(zmax-zmin)/float(nzf-1) &  zf=zf*dzf+zmin 
  in=-1 &  k=0
  repeat begin &    in=in+1
    while xf(in) gt x(k+1) do k=k+1
    ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
  endrep until in eq nxf-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while yf(in) gt y(k+1) do k=k+1
    ioyf(in) = float(k) + (yf(in)-y(k))/(y(k+1)-y(k))        
  endrep until in eq nyf-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while zf(in) gt z(k+1) do k=k+1
    iozf(in) = float(k) + (zf(in)-z(k))/(z(k+1)-z(k))        
  endrep until in eq nzf-1

  f1=rho 
  head1='Density' 
  plane='a' & newgrid1='10' & newgrid2='10' & newgrid3='10' 
  igrid1=10 & igrid2=10 & igrid3=10 & whatcut='10'

;----------START of PROGRAM menu----------
cut:
  print, 'Input - What Cut Through The 3-D System:'
  print, 'Options: a -> cuts in three directions'
  print, '         x -> z,x-plane, x=const'
  print, '         y -> x,y-plane, y=const'
  print, '         z -> y,z-plane, z=const'
  print, '    return -> no changes applied'
  print, '         q -> terminate'
  print, 'Present Choice: ', plane
  read, whatcut
  if whatcut eq 'q' then stop
  if whatcut eq 'a' then plane='a'
  if whatcut eq 'x' then plane='x'
  if whatcut eq 'y' then plane='y'
  if whatcut eq 'z' then plane='z'
  if whatcut eq '' then print,'choice=',plane,' not altered'
  if plane eq 'x' then begin & nplane=nx & coord=x & endif
  if plane eq 'y' then begin & nplane=ny & coord=y & endif
  if plane eq 'z' then begin & nplane=nz & coord=z & endif

gridindex:
  if plane ne 'a' then begin 
    print, plane, 'Coordinates:'
    for i=0,nplane-1,2 do print, i,'  ',plane,'=',coord(i)
  endif
  print, 'Input - i = Grid Index of Chosen Planes(>0 and < Max n_plane)'
  print, 'Options:  3 integers -> 3 grid indices'
  print, '              return -> no changes applied'
  print, '                   c -> back to cut'
  print, '                   q -> terminate'
  print, 'Present Choice: ', igrid1,igrid2,igrid3
  read, newgrid1
  if newgrid1 eq 'q' then stop
  if newgrid1 eq 'c' then goto, cut
  if newgrid1 ne '' then begin
    read, newgrid2, newgrid3 
    igrid1=fix(newgrid1)
    igrid2=fix(newgrid2) & igrid3=fix(newgrid3)
  endif
  if newgrid1 eq '' then print,'choice=',$
     igrid1,igrid2,igrid3,' not altered'

  if plane eq 'a' then begin
    cutata1='x =' & cutata2='y =' & cutata3='z ='
    cutatb1=string(x(igrid1),'(f7.2)')
    cutatb2=string(y(igrid2),'(f7.2)')
    cutatb3=string(z(igrid3),'(f7.2)')
    xtit1='y'    & ytit1='z'    & xchoice1=y   & ychoice1=z
    xpmin1=ymin  & xpmax1=ymax  & ypmin1=zmin  & ypmax1=zmax 
    xtit2='z'    & ytit2='x'    & xchoice2=z   & ychoice2=x
    xpmin2=zmin  & xpmax2=zmax  & ypmin2=xmin  & ypmax2=xmax
    xtit3='y'    & ytit3='x'    & xchoice3=y   & ychoice3=x
    xpmin3=zmin  & xpmax3=zmax  & ypmin3=xmin  & ypmax3=xmax 
    fc1=reform(f1(igrid1,*,*)) 
    fc2=reform(f1(*,igrid2,*)) & fc2=rotate(fc2,4)
    fc3=reform(f1(*,*,igrid3)) & fc3=rotate(fc3,4)
    V3D22D, 'x', x(igrid1), xn,yn,zn, vecx1,vecy1,xar1,yar1
    V3D22D, 'y', y(igrid2), xn,yn,zn, vecx2,vecy2,xar2,yar2
    V3D22D, 'z', z(igrid3), xn,yn,zn, vecx3,vecy3,xar3,yar3
  endif
  if plane eq 'x' then begin
    cutata='x ='  
    cutatb1=string(x(igrid1),'(f7.2)')
    cutatb2=string(x(igrid2),'(f7.2)')
    cutatb3=string(x(igrid3),'(f7.2)')
    xtit='y'    & ytit='z'    & xchoice=y   & ychoice=z
    xpmin=ymin  & xpmax=ymax  & ypmin=zmin  & ypmax=zmax 
    fc1=f1(igrid1,*,*) & fc1=reform(fc1) 
    fc2=f2(igrid2,*,*) & fc2=reform(fc2) 
    fc3=f3(igrid3,*,*) & fc3=reform(fc3) 
    V3D22D, 'x', x(igrid1), xn,yn,zn, vecx1,vecy1,xar1,yar1
    V3D22D, 'x', x(igrid2), xn,yn,zn, vecx2,vecy2,xar2,yar2
    V3D22D, 'x', x(igrid3), xn,yn,zn, vecx3,vecy3,xar3,yar3
  endif
  if plane eq 'y' then begin
    cutata='y =' 
    cutatb1=string(y(igrid1),'(f7.2)')
    cutatb2=string(y(igrid2),'(f7.2)')
    cutatb3=string(y(igrid3),'(f7.2)')
    xtit='z'    & ytit='x'    & xchoice=z   & ychoice=x
    xpmin=zmin  & xpmax=zmax  & ypmin=xmin  & ypmax=xmax
    fc1=f1(*,igrid1,*) & fc1=reform(fc1) & fc1=rotate(fc1,4)
    fc2=f1(*,igrid2,*) & fc2=reform(fc2) & fc2=rotate(fc2,4)
    fc3=f1(*,igrid3,*) & fc3=reform(fc3) & fc3=rotate(fc3,4)
    V3D22D, 'y', y(igrid1), xn,yn,zn, vecx1,vecy1,xar1,yar1
    V3D22D, 'y', y(igrid2), xn,yn,zn, vecx2,vecy2,xar2,yar2
    V3D22D, 'y', y(igrid3), xn,yn,zn, vecx3,vecy3,xar3,yar3
  endif
  if plane eq 'z' then begin
    cutata='z =' 
    cutatb1=string(z(igrid1),'(f7.2)')
    cutatb2=string(z(igrid2),'(f7.2)')
    cutatb3=string(z(igrid3),'(f7.2)')
    xtit='y'    & ytit='x'    & xchoice=y   & ychoice=x
    xpmin=zmin  & xpmax=zmax  & ypmin=xmin  & ypmax=xmax 
    fc1=f1(*,*,igrid1) & fc1=reform(fc1) & fc1=rotate(fc1,4)
    fc2=f1(*,*,igrid2) & fc2=reform(fc2) & fc2=rotate(fc2,4)
    fc3=f1(*,*,igrid3) & fc3=reform(fc3) & fc3=rotate(fc3,4)
    V3D22D, 'z', z(igrid1), xn,yn,zn, vecx1,vecy1,xar1,yar1
    V3D22D, 'z', z(igrid2), xn,yn,zn, vecx2,vecy2,xar2,yar2
    V3D22D, 'z', z(igrid3), xn,yn,zn, vecx3,vecy3,xar3,yar3
  endif

; first plot...
grausig:
  print, 'With postscript (output in p3.ps)? '
  read, withps
  if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='p3.ps'
          device,/landscape
;          device,/inches,ysize=8.,scale_factor=1.0,xoffset=0.5
;          device,/inches,xsize=10.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
  endif

;----------START of PLOTTING data------------
plotlustig:
  if plane eq 'a' then begin
   xtit=xtit1 & ytit=ytit1 & xchoice=xchoice1 & ychoice=ychoice1
   xpmin=xpmin1 & xpmax=xpmax1 & ypmin=ypmin1 & ypmax=ypmax1
  endif
     xpxt=0.85 & ypxt=-0.15
     xpyt=-0.125 & ypyt=0.8
     xprest=0.125 & ypdist=-0.3 & ypsep=-0.1
     xposxt = xpmin+xpxt*(xpmax-xpmin)
     yposxt = ypmin+ypxt*(ypmax-ypmin)
     xposyt = xpmin+xpyt*(xpmax-xpmin)
     yposyt = ypmin+ypyt*(ypmax-ypmin)
     xpos = xpmin+xprest*(xpmax-xpmin)
     ypost=ypmin+ypdist*(ypmax-ypmin)
     yposcut=ypost+ypsep*(ypmax-ypmin)
     yposmin=yposcut+ypsep*(ypmax-ypmin)
     yposrun=yposmin+ypsep*(ypmax-ypmin)
      hoch=0.35 & breit=0.26 & inter=0.05
      xl1=0.05      & xr1=xl1+breit 
      xl2=xr1+inter & xr2=xl2+breit 
      xl3=xr2+inter & xr3=xl3+breit 
      !P.MULTI=[0,3,0]
      pos1=[xl1,0.3,xr1,0.3+hoch]
      pos2=[xl2,0.3,xr2,0.3+hoch]
      pos3=[xl3,0.3,xr3,0.3+hoch]
       
;----------First Page--------------------
  print, 'Plot First Page (Contour Lines)?'
  print, 'Options:         y or return'
  print, 'Or:              s -> plot with smoothing'
  print, '                 c -> back to cut'
  print, '                 i -> back to grid index'
  print, '                 q -> terminate'
  read, contin
  if contin eq 'q' then stop
  if contin eq 'i' then goto, gridindex
  if contin eq 'c' then goto, cut
  if (contin eq '' or contin eq 'y' or contin eq 's') then begin
    !P.REGION=[0.,0.,1.0,1.0]
    !P.FONT=2
    !P.CHARSIZE=2.0
    !P.CHARTHICK=1
    !X.THICK=2
    !Y.THICK=2
    !Z.THICK=2

    if plane eq 'a' then begin
     xtit=xtit1 & ytit=ytit1 & xchoice=xchoice1 & ychoice=ychoice1
     xpmin=xpmin1 & xpmax=xpmax1 & ypmin=ypmin1 & ypmax=ypmax1
     cutata=cutata1
     xposxt = xpmin+xpxt*(xpmax-xpmin)
     yposxt = ypmin+ypxt*(ypmax-ypmin)
     xposyt = xpmin+xpyt*(xpmax-xpmin)
     yposyt = ypmin+ypyt*(ypmax-ypmin)
     xpos = xpmin+xprest*(xpmax-xpmin)
     ypost=ypmin+ypdist*(ypmax-ypmin)
     yposcut=ypost+ypsep*(ypmax-ypmin)
     yposmin=yposcut+ypsep*(ypmax-ypmin)
     yposrun=yposmin+ypsep*(ypmax-ypmin)
    endif
    
    if contin eq 's' then fp1=smooth(fc1,3) else fp1=fc1 
    fpmax=max(fp1) & fpmin=min(fp1)
    if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/20. & if delf lt 0.000000001 then delf=0.0001
    !P.POSITION=pos1
     contour,fp1,xchoice,ychoice,levels=findgen(21)*delf+fpmin,$
       c_linestyle=findgen(21)*delf+fpmin lt fptr,$
       xrange=[xpmin,xpmax],yrange=[ypmin,ypmax],$
       title=head1,xstyle=1,ystyle=1
    xyouts,xposxt,yposxt,xtit
    xyouts,xposyt,yposyt,ytit
    xyouts,xpos,ypost,'t ='+string(time,'(i3)')
    xyouts,xpos,yposcut,cutata+cutatb1
    xyouts,xpos,yposmin,'Min/Max ='+string(fpmin,'(f5.2)')$
                        +'/'+string(fpmax,'(f5.2)')
    xyouts,xpos,yposrun,run
 
    if plane eq 'a' then begin
     xtit=xtit2 & ytit=ytit2 & xchoice=xchoice2 & ychoice=ychoice2
     xpmin=xpmin2 & xpmax=xpmax2 & ypmin=ypmin2 & ypmax=ypmax2
     cutata=cutata2
     xposxt = xpmin+xpxt*(xpmax-xpmin)
     yposxt = ypmin+ypxt*(ypmax-ypmin)
     xposyt = xpmin+xpyt*(xpmax-xpmin)
     yposyt = ypmin+ypyt*(ypmax-ypmin)
     xpos = xpmin+xprest*(xpmax-xpmin)
     ypost=ypmin+ypdist*(ypmax-ypmin)
     yposcut=ypost+ypsep*(ypmax-ypmin)
     yposmin=yposcut+ypsep*(ypmax-ypmin)
     yposrun=yposmin+ypsep*(ypmax-ypmin)
    endif
    
    if contin eq 's' then fp2=smooth(fc2,3) else fp2=fc2 
    fpmax=max(fp2) & fpmin=min(fp2)
    if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/20. & if delf lt 0.000000001 then delf=0.0001
    !P.POSITION=pos2
     contour,fp2,xchoice,ychoice,levels=findgen(21)*delf+fpmin,$
       c_linestyle=findgen(21)*delf+fpmin lt fptr,$
       xrange=[xpmin,xpmax],yrange=[ypmin,ypmax],$
       title=head1,xstyle=1,ystyle=1,font=2
    xyouts,xposxt,yposxt,xtit,font=2
    xyouts,xposyt,yposyt,ytit,font=2
    xyouts,xpos,ypost,'t ='+string(time,'(i3)'),font=2
    xyouts,xpos,yposcut,cutata+cutatb1,font=2
    xyouts,xpos,yposmin,'Min/Max ='+string(fpmin,'(f5.2)')$
                        +'/'+string(fpmax,'(f5.2)'),font=2
    xyouts,xpos,yposrun,run

    if plane eq 'a' then begin
     xtit=xtit3 & ytit=ytit3 & xchoice=xchoice3 & ychoice=ychoice3
     xpmin=xpmin3 & xpmax=xpmax3 & ypmin=ypmin3 & ypmax=ypmax3
     xposxt = xpmin+xpxt*(xpmax-xpmin)
     yposxt = ypmin+ypxt*(ypmax-ypmin)
     xposyt = xpmin+xpyt*(xpmax-xpmin)
     yposyt = ypmin+ypyt*(ypmax-ypmin)
     xpos = xpmin+xprest*(xpmax-xpmin)
     ypost=ypmin+ypdist*(ypmax-ypmin)
     yposcut=ypost+ypsep*(ypmax-ypmin)
     yposmin=yposcut+ypsep*(ypmax-ypmin)
     yposrun=yposmin+ypsep*(ypmax-ypmin)
    endif
    
    if contin eq 's' then fp3=smooth(fc3,3) else fp3=fc3 
    fpmax=max(fp3) & fpmin=min(fp3)
    if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/20. & if delf lt 0.000000001 then delf=0.0001
    !P.POSITION=pos3
     contour,fp3,xchoice,ychoice,levels=findgen(21)*delf+fpmin,$
       c_linestyle=findgen(21)*delf+fpmin gt fptr,$
       xrange=[xpmin,xpmax],yrange=[ypmin,ypmax],$
       title=head1,xstyle=1,ystyle=1,font=2
    xyouts,xposxt,yposxt,xtit,font=2
    xyouts,xposyt,yposyt,ytit,font=2
    xyouts,xpos,ypost,'t ='+string(time,'(i3)'),font=2
    xyouts,xpos,yposcut,cutata+cutatb1,font=2
    xyouts,xpos,yposmin,'Min/Max ='+string(fpmin,'(f5.2)')$
                        +'/'+string(fpmax,'(f5.2)'),font=2
    xyouts,xpos,yposrun,run
    
    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        print,'postscipt file p3.ps closed'
        set_plot,'x' & withps='' & endif
    endif
  endif


;----------Second Page--------------------
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
    !P.FONT=-1
    !P.CHARSIZE=4.0
    !P.CHARTHICK=1.5
    ang1=55 & ang2=40
    if plane eq 'a' then begin
      fa=interpolate(fc1,ioyf,iozf,/grid)
      fb=interpolate(fc2,iozf,ioxf,/grid)
      fc=interpolate(fc3,ioyf,ioxf,/grid)
    if contin eq 's' then begin
      fa=smooth(fa,3) & fb=smooth(fb,3) & fc=smooth(fc,3) & endif
        head=head1+'  '+cutata1+cutatb1
      surface,fa,yf,zf,position=[0.35,0.1,0.7,0.4],$
        ax=ang1,az=ang2,$
        ztitle=head,xstyle=1,ystyle=1,xtitle=xtit1,ytitle=ytit1
        head=head1+'  '+cutata2+cutatb2
      surface,fb,zf,xf,position=[0.6,0.65,0.95,0.95],$
        ax=ang1,az=ang2,$
        ztitle=head,xstyle=1,ystyle=1,xtitle=xtit2,ytitle=ytit2
        head=head1+'  '+cutata3+cutatb3
      surface,fc,yf,xf,position=[0.1,0.65,0.45,0.95],$
        ax=ang1,az=ang2,$
        ztitle=head,xstyle=1,ystyle=1,xtitle=xtit3,ytitle=ytit3
    endif
    if plane ne 'a' then begin
      if plane eq 'x' then begin
        fa=interpolate(fc1,ioyf,iozf,/grid)
        fb=interpolate(fc2,ioyf,iozf,/grid)
        fc=interpolate(fc3,ioyf,iozf,/grid)
        xsu=yf & ysu=zf & endif 
      if plane eq 'y' then begin
        fa=interpolate(fc1,iozf,ioxf,/grid)
        fb=interpolate(fc2,iozf,ioxf,/grid)
        fc=interpolate(fc3,iozf,ioxf,/grid)
        xsu=zf & ysu=xf & endif 
      if plane eq 'z' then begin
        fa=interpolate(fc1,ioxf,ioyf,/grid)
        fb=interpolate(fc2,ioxf,ioyf,/grid)
        fc=interpolate(fc3,ioxf,ioyf,/grid)
        xsu=xf & ysu=yf & endif 
      erase
      if contin eq 's' then begin
        fa=smooth(fa,3) & fb=smooth(fb,3) & fc=smooth(fc,3) & endif
        head=head1+'  '+cutata+cutatb1
      surface,fa,xsu,ysu,position=[0.35,0.1,0.7,0.4],$
        ax=ang1,az=ang2,$
        ztitle=head,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
        head=head1+'  '+cutata+cutatb2
      surface,fb,xsu,ysu,position=[0.6,0.65,0.95,0.95],$
        ax=ang1,az=ang2,$
        ztitle=head,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
        head=head1+'  '+cutata+cutatb3
      surface,fc,xsu,ysu,position=[0.1,0.65,0.45,0.95],$
        ax=ang1,az=ang2,$
        ztitle=head,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    endif
    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        print,'postscipt file p3.ps closed'
        set_plot,'x' & withps='' & endif
    endif
  endif


;----------Third Page--------------------
  print, 'Plot 3. Page (Contour Lines and Arrows)?'
  print, 'Options:         y or return'
  print, '                 c -> back to cut'
  print, '                 i -> back to grid index'
  print, '                 p -> back to postscript'
  print, '                 q -> terminate'
  read, contin
  if contin eq 'q' then stop
  if contin eq 'c' then goto, cut
  if contin eq 'i' then goto, gridindex
  if contin eq 'p' then goto, grausig
  if (contin eq '' or contin eq 'y') then begin
      !P.MULTI=[0,3,0]
      !P.REGION=[0.,0.,1.0,1.0]
      !P.FONT=2
      !P.CHARSIZE=2.0
      high=0.35 & breit=0.26 & inter=0.05
      xl1=0.05      & xr1=xl1+breit 
      xl2=xr1+inter & xr2=xl2+breit 
      xl3=xr2+inter & xr3=xl3+breit 
      pov1=[xl1,0.3,xr1,0.3+high]
      pov2=[xl2,0.3,xr2,0.3+high]
      pov3=[xl3,0.3,xr3,0.3+high]
      pov4=[xl1,0.52,xr1,0.52+high]
      pov5=[xl2,0.52,xr2,0.52+high]
      pov6=[xl3,0.52,xr3,0.52+high]
      
    if plane eq 'a' then begin
      xtit=xtit1 & ytit=ytit1 
      xpmin=xpmin1 & xpmax=xpmax1 & ypmin=ypmin1 & ypmax=ypmax1
      cutata=cutata1
     xposxt = xpmin+xpxt*(xpmax-xpmin)
     yposxt = ypmin+ypxt*(ypmax-ypmin)
     xposyt = xpmin+xpyt*(xpmax-xpmin)
     yposyt = ypmin+ypyt*(ypmax-ypmin)
     xpos = xpmin+xprest*(xpmax-xpmin)
     ypost=ypmin+ypdist*(ypmax-ypmin)
     yposcut=ypost+ypsep*(ypmax-ypmin)
     yposmin=yposcut+ypsep*(ypmax-ypmin)
     yposrun=yposmin+ypsep*(ypmax-ypmin)
    endif
    fmax=sqrt(max(vecx1^2+vecy1^2))
    !P.POSITION=pov1
    vect, vecx1, vecy1, xar1, yar1, length=1.5,$
      title=xtit+'/'+ytit+' Flow Vect'
    xyouts,xposxt,yposxt,xtit
    xyouts,xposyt,yposyt,ytit
    xyouts,xpos,ypost,'t ='+string(time,'(i3)')
    xyouts,xpos,yposcut,cutata+cutatb1
    xyouts,xpos,yposmin,'Max ='+string(fpmax,'(f5.2)')
    xyouts,xpos,yposrun,run

    if plane eq 'a' then begin
      xtit=xtit2 & ytit=ytit2 
      xpmin=xpmin2 & xpmax=xpmax2 & ypmin=ypmin2 & ypmax=ypmax2
      cutata=cutata2
     xposxt = xpmin+xpxt*(xpmax-xpmin)
     yposxt = ypmin+ypxt*(ypmax-ypmin)
     xposyt = xpmin+xpyt*(xpmax-xpmin)
     yposyt = ypmin+ypyt*(ypmax-ypmin)
     xpos = xpmin+xprest*(xpmax-xpmin)
     ypost=ypmin+ypdist*(ypmax-ypmin)
     yposcut=ypost+ypsep*(ypmax-ypmin)
     yposmin=yposcut+ypsep*(ypmax-ypmin)
     yposrun=yposmin+ypsep*(ypmax-ypmin)
    endif
    fmax=sqrt(max(vecx2^2+vecy2^2))
    !P.POSITION=pov2
    vect, vecx2, vecy2, xar2, yar2, length=1.5,$
      title=xtit+'/'+ytit+' Flow Vect'
    xyouts,xposxt,yposxt,xtit
    xyouts,xposyt,yposyt,ytit
    xyouts,xpos,ypost,'t ='+string(time,'(i3)')
    xyouts,xpos,yposcut,cutata+cutatb2
    xyouts,xpos,yposmin,'Max ='+string(fpmax,'(f5.2)')
    xyouts,xpos,yposrun,run

    if plane eq 'a' then begin
      xtit=xtit3 & ytit=ytit3 
      xpmin=xpmin3 & xpmax=xpmax3 & ypmin=ypmin3 & ypmax=ypmax3
      cutata=cutata3
     xposxt = xpmin+xpxt*(xpmax-xpmin)
     yposxt = ypmin+ypxt*(ypmax-ypmin)
     xposyt = xpmin+xpyt*(xpmax-xpmin)
     yposyt = ypmin+ypyt*(ypmax-ypmin)
     xpos = xpmin+xprest*(xpmax-xpmin)
     ypost=ypmin+ypdist*(ypmax-ypmin)
     yposcut=ypost+ypsep*(ypmax-ypmin)
     yposmin=yposcut+ypsep*(ypmax-ypmin)
     yposrun=yposmin+ypsep*(ypmax-ypmin)
    endif
    fmax=sqrt(max(vecx3^2+vecy3^2))
    !P.POSITION=pov3
    vect, vecx3, vecy3, xar3, yar3, length=1.5,$
      title=xtit+'/'+ytit+' Flow Vect'
    xyouts,xposxt,yposxt,xtit
    xyouts,xposyt,yposyt,ytit
    xyouts,xpos,ypost,'t ='+string(time,'(i3)')
    xyouts,xpos,yposcut,cutata+cutatb3
    xyouts,xpos,yposmin,'Max ='+string(fpmax,'(f5.2)')
    xyouts,xpos,yposrun,run

    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        print,'postscipt file p3.ps closed'
        set_plot,'x' & withps='' & endif
    endif
  endif
  
  print, 'Plot Again?'
  print, 'Options:         y or return'
  print, '                 p -> back to postscript'
  print, '                 i -> back to grid index'
  print, '                 c -> back to cut'
  print, '            n or q -> terminate'
  read, contin
  if (contin eq '' or contin eq 'y')   then goto, plotlustig
  if (contin eq 'q' or contin eq 'n')  then stop
  if contin eq 'c'                     then goto, cut
  if contin eq 'i'                     then goto, gridindex
  if contin eq 'p'                     then goto, grausig


end


