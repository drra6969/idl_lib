; program to plot the 1D MHD data

  nt=101 & nx=23
  ind=0
  xr=0.1 & rr=xr & pr=xr & bxr=xr & byr=xr &  bzr=xr
                           vxr=xr & vyr=xr & vzr=xr
  again='y' &  dumm='' &  whatindex='' & withps='n'  & anim='n' 
  names=strarr(15)
  names=replicate(' ',15)


  openr, 8, 'm1out'
  readf, 8, dumm
  readf, 8, dumm
  print,dumm
  nxst=STRMID(dumm,5,8) & nx=fix(nxst) & print,'nx =  ',nx
  x=fltarr(nx) & rho=fltarr(nt,nx) & p=rho 
        bx=rho & by=rho & bz=rho & vx=rho & vy=rho & vz=rho
  tarr=fltarr(nt)
  for k = 1, 10  do  readf, 8, dumm


  i=0
  while not eof(8) do begin
    readf, 8, dumm
    readf, 8, dumm
    print,dumm 
    timest=STRMID(dumm,30,8) & print, timest
    time=float(timest)  & print,'time= ',time
    tarr(i)=time
    readf, 8, dumm

    for k=0, nx-1 do begin
      readf, 8, xr,rr,pr,bxr,byr,bzr,vxr,vyr,vzr
      x(k)=xr & rho(i,k)=rr & p(i,k)=pr & 
      bx(i,k)=bxr & by(i,k)=byr & bz(i,k)=bzr
      vx(i,k)=vxr & vy(i,k)=vyr & vz(i,k)=vzr
    endfor
    i = i+1
  endwhile
  close, 8
  ntot=i-1
  np=nx-2
  xmin=x(1) & xmax=x(np) & del=xmax-xmin
  xpos=xmax+0.05*del

  print,'The maximum no of datasets is presently:',nt
  print,'To change it modify nt!!!'


 menu:
  if withps eq 'y' then $
       print, 'The postscript file is open!',$
              'You have to close the postscript file to generate new plots',$
              'on the screen. Otherwise all subsequent plots are appended ',$
              'to the postscript file.',$
              'To close the file choose c in the menu!'
  print,'anim0=',anim
  print, 'CHOOSE PROBE INDEX: '
  print, 'Present choice:  Probe index =',ind
  print, 'Options:   integer -> probe index'
  print, '            return -> no changes applied'
  print, '                 a -> generate an animation'
  print, '                 p -> postscrip output'
  print, '                 c -> close postscrip output'
  print, '                 q -> terminate'
  read, whatindex

  if whatindex eq '' then print,'index=',ind,' not altered'

  if whatindex eq 'p' then begin
       withps = 'y'
       !P.THICK=2.
       !X.THICK=1.5
       !Y.THICK=1.5
       !P.CHARTHICK=3.
       set_plot,'ps'
       device,filename='pl1d'+string(ind,'(i2.2)')+'.ps'
       device,/portrait
       device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
       device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
       withps='y' 
  endif

  if whatindex eq 'c' then begin
       device,/close  &  set_plot,'x'  &  withps='n'  &   endif

  if whatindex eq 'q' then stop

  if ( (whatindex ne '') and (whatindex ne 'a') and $
      (whatindex ne 'p') and (whatindex ne 'c') ) then begin
    ind = fix(whatindex)   &  if (ind gt ntot) then  ind = ntot
    print, 'Chosen dataset has index: ',ind
  endif

  if whatindex eq 'a' then anim='y'

  if withps eq 'n' then WINDOW, XSIZE=750, YSIZE=550, $
                                TITLE='One-Dimensional MHD'      

  if anim eq 'y' then begin
      xanimate,set=[750,550,(ntot+1),0]
      ind=-1
    animate:
      ind=ind+1
  endif

    erase
     !X.RANGE=[xmin,xmax]     
     !P.MULTI=[0,0,7]
     !P.CHARSIZE=2.0
     !P.FONT=-1
     !P.THICK=1
     !X.TICKS=4
     !Y.TICKS=4
;     !X.THICK=2
;     !Y.THICK=2
;     !Z.THICK=2
;     !Z.MINOR=-1


    fmax=max(rho(0:ntot,1:np)) & fmin=min(rho(0:ntot,1:np))
    if (fmax-fmin) lt 0.0000001 then fmax=fmin+0.1
    !P.POSITION=[0.15,0.8,0.75,0.925]
    plot, x(1:np), rho(ind,1:np),$
        xtitle='', ytitle='rho', yrange=[fmin,fmax], xtickname=names, $
        xstyle=1, ystyle=1
    xpos=xmax+0.05*del
    ypos=fmax-0.1*(fmax-fmin)
    xyouts,xpos,ypos,'time ='+string(tarr(ind),'(f8.1)'),font=3

    fmax=max(p(0:ntot,1:np)) & fmin=min(p(0:ntot,1:np))
    if (fmax-fmin) lt 0.0000001 then fmax=fmin+0.1
    !P.POSITION=[0.15,0.675,0.75,0.8]
    plot, x(1:np), p(ind,1:np),$
        xtitle='', ytitle='p', yrange=[fmin,fmax], xtickname=names, $
        xstyle=1, ystyle=1

    fmax=max(by(0:ntot,1:np)) & fmin=min(by(0:ntot,1:np))
    if (fmax-fmin) lt 0.0000001 then fmax=fmin+0.1
    !P.POSITION=[0.15,0.55,0.75,0.675]
    plot, x(1:np), by(ind,1:np),$
        xtitle='', ytitle='by', yrange=[fmin,fmax], xtickname=names, $
        xstyle=1, ystyle=1

    fmax=max(bz(0:ntot,1:np)) & fmin=min(bz(0:ntot,1:np))
    if (fmax-fmin) lt 0.0000001 then fmax=fmin+0.1
    !P.POSITION=[0.15,0.425,0.75,0.55]
    plot, x(1:np), bz(ind,1:np),$
        xtitle='', ytitle='bz', yrange=[fmin,fmax], xtickname=names, $
        xstyle=1, ystyle=1

    fmax=max(vx(0:ntot,1:np)) & fmin=min(vx(0:ntot,1:np))
    if (fmax-fmin) lt 0.0000001 then fmax=fmin+0.1
    !P.POSITION=[0.15,0.3,0.75,0.425]
    plot, x(1:np), vx(ind,1:np),$
        xtitle='', ytitle='vx', yrange=[fmin,fmax], xtickname=names, $
        xstyle=1, ystyle=1

    fmax=max(vy(0:ntot,1:np)) & fmin=min(vy(0:ntot,1:np))
    if (fmax-fmin) lt 0.0000001 then fmax=fmin+0.1
    !P.POSITION=[0.15,0.175,0.75,0.3]
    plot, x(1:np), vy(ind,1:np),$
        xtitle='x', ytitle='vy', yrange=[fmin,fmax], $
        xstyle=1, ystyle=1

 ;   bb=sqrt(by^2+bz^2)
    fmax=max(vz(0:ntot,1:np)) & fmin=min(vz(0:ntot,1:np))
    if (fmax-fmin) lt 0.0000001 then fmax=fmin+0.1
    !P.POSITION=[0.15,0.05,0.75,0.175]
    plot, x(1:np), vz(ind,1:np),$
        xtitle='x', ytitle='vz', yrange=[fmin,fmax], $
        xstyle=1, ystyle=1

  print,'anim3=',anim

  if (anim eq 'y') then begin
      xanimate,frame=ind,window=!d.window
      if (ind lt ntot) then goto, animate
      xanimate
      anim='n'
  endif
  print,'anim4=',anim

 goto, menu
 
end 

