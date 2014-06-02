; START OF MAIN PROGRAM

  nx=21 & ny=21 & nz=2 
  nxf=51 & nyf=51 & nzf=51
  mx=1 & my=1  & mz=1 

  time=0.0 & fnumber=1
  name='' & contin='' & again='y' & withps='n' & run=''
  dumm='' 

  print, 'Input filenumber'
  read, fnumber
  name='pot'+string(fnumber,'(i2)')
  openr, 8, name

  close, 8

;  print, x
;  print, y
;  print, z

;----PARAMETER----
  print, ' max boundaries of plot: xmin/max=', x(0), x(nx-1) 
  print, '                         ymin/max=', y(0), y(ny-1) 
  xmin = x(0)    &  ymin = y(0)    
  xmax = x(nx-1) &  ymax = y(ny-1) 
;  xmin = -2. &  ymin = 30.0 &  zmin = z(0)
;  xmax = 2. &  ymax = 38.0 &  zmax = z(nz-1)

  print, ' Actual boundaries of plot: xmin/max=', xmin, xmax 
  print, '                            ymin/max=', ymin, ymax 

; generation of new grid for contourplots
  xf=findgen(nxf) & yf=findgen(nyf) 
  dxf=(xmax-xmin)/float(nxf-1) & xf=xf*dxf+xmin
  dyf=(ymax-ymin)/float(nyf-1) & yf=yf*dyf+ymin

grausig:
  print, 'With postscript (output in pot.ps)? '
  read, withps
  if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='po'+string(fnumber,'(i1.1)')+'.ps'
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
      !P.MULTI=[0,3,0]
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
    !P.CHARSIZE=1.0

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

    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        print,'postscipt file pot.ps closed'
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

