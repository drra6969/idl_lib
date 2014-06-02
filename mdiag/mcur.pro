; START OF MAIN PROGRAM

  ntmax=201 & ny=81 & my=81
  ntf=81 & nyf=81
  xj=fltarr(ny,ntmax,/NOZERO) & jj=xj & bx=xj

  name='' & contin='' & again='y' & withps='n' & run=''
  dumm='' & rest= ny mod 9 & nrows=(ny-rest) / 9 
  print, 'nrows= ', nrows, '         rest= ', rest
  yh=fltarr(9,/NOZERO) & if (rest gt 0) then yrest=fltarr(rest,/NOZERO)
  tr=0.0 & fnumber=1

  y=fltarr(ny,/NOZERO) & t=fltarr(ntmax,/NOZERO)
  iotf=fltarr(ntf) & ioyf=fltarr(nyf)
  fa=fltarr(nyf,ntf) & fb=fa

;  print, 'what filenumber'
;  read, fnumber
;  openr, 8, 'magdcur'+string(fnumber,'(i1)')
  openr, 8, 'magdcur'
  
  readf, 8, dumm
  readf, 8, dumm
  readf, 8, dumm
  print, dumm
  readf, 8, my
  if (my ne ny) then print, 'ny <> my =', my

  for k=0, nrows-1  do begin
    readf, 8, yh
    for l=0, 8  do y(l+k*9)=yh(l)
  endfor
  if (rest gt 0) then begin
    readf, 8, yrest
;    print, yrest
    for l=0, rest-1  do y(l+nrows*9)=yrest(l)
;  print, y
  endif

  it=0
  while not eof(8) do begin
    readf, 8, tr
    t(it)=tr
    print, it, t(it)
    for k=0, nrows-1  do begin
      readf, 8, yh
      for l=0, 8  do xj(l+k*9,it)=yh(l)
    endfor
    if (rest gt 0) then begin
      readf, 8, yrest
      for l=0, rest-1  do xj(l+nrows*9,it)=yrest(l)
    endif

    for k=0, nrows-1  do begin
      readf, 8, yh
      for l=0, 8  do jj(l+k*9,it)=yh(l)
    endfor
    if (rest gt 0) then begin
      readf, 8, yrest
      for l=0, rest-1  do jj(l+nrows*9,it)=yrest(l)
    endif

    for k=0, nrows-1  do begin
      readf, 8, yh
      for l=0, 8  do bx(l+k*9,it)=yh(l)
    endfor
    if (rest gt 0) then begin
      readf, 8, yrest
      for l=0, rest-1  do bx(l+nrows*9,it)=yrest(l)
    endif

    if it lt ntmax then it=it+1 else stop, ' to many records '
  endwhile
  nt=it
  print, 'time interval: ', t(0), t(nt-1)
  print, 'y interval: ', y(0), y(ny-1)

  close, 8

  tmin =  0.  &  ymin =  -5.
  tmax = 160. &  ymax = 5.
  print, 'tmin=',tmin, '   tmax=',tmax 
  print, 'ymin=',ymin, '   ymax=',ymax 
  xtit='z'

; generation of new grid 
  tf=findgen(ntf) & yf=findgen(nyf)
  dtf=(tmax-tmin)/float(ntf-1)
  tf=tf*dtf+tmin
;  print, dtf,tf
  dyf=(ymax-ymin)/float(nyf-1)
  yf=yf*dyf+ymin
;  print, dyf,yf
 
  in=-1
  k=0
  repeat begin
    in=in+1
    while tf(in) gt t(k+1) do k=k+1
    iotf(in) = float(k) + (tf(in)-t(k))/(t(k+1)-t(k)) 
;  print, in, tf(in), k, t(k), iotf(in)      
  endrep until in eq ntf-1
  in=-1
  k=0
  repeat begin
    in=in+1
    while yf(in) gt y(k+1) do k=k+1
    ioyf(in) = float(k) + (yf(in)-y(k))/(y(k+1)-y(k))        
;    print, in, ynf(in), k, y(k), ioyf(in)      
  endrep until in eq nyf-1

  print, 'Which case?'
  read, run
  fall=run
;  fall='run:'+run

  while (again eq 'y') do begin

    print, 'With postscript?'
    read, withps
    if withps eq 'y' then begin 
       !P.CHARTHICK=3
        set_plot,'ps'
        device,filename='cur.ps'
        device,/landscape
;        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
;        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
    endif

    dx=0.55 & dy=0.9 & dz=0.6
    if withps eq 'y' then begin dx=0.3 & dy=0.6 & dz=0.4 & endif
    x1=0.2 & x2=x1+dx
    y1=0.1 & y2=y1+dy
    z1=0.1 & z2=z1+dz
    ang1=55 & ang2=10
    
    print, 'plot first page? xposition of current sheet'
    read, contin
    if (contin eq '' or contin eq 'y') then begin

; plot first page
;       !P.REGION=[0.,0.,1.0,1.25]
;       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=-1
       !P.THICK=1
       !X.TICKS=3
       !Y.TICKS=3
       !X.THICK=2
       !Y.THICK=2
       !Z.THICK=2
       !Z.MINOR=-1
      erase
      fa=interpolate(xj,ioyf,iotf,/grid)
      fa=smooth(fa,3)
      surface,fa,yf,tf,position=[x1, y1, x2, y2, z1, z2],$
        ax=ang1, az=ang2,$
        ztitle='x pos',xstyle=1,ystyle=1,zstyle=1,$
        xtitle=xtit,ytitle='time'

    endif

    print, 'plot second page? current in current sheet'
    read, contin
    if (contin eq '' or contin eq 'y') then begin

      erase
      fa=interpolate(jj,ioyf,iotf,/grid)
;      fa=smooth(fa,3)
      surface,-fa,yf,tf,position=[x1, y1, x2, y2, z1, z2],$
        ax=ang1, az=ang2,$
        ztitle='-J',xstyle=1,ystyle=1,zstyle=1,$
           xtitle=xtit,ytitle='time',/noerase

    endif

    print, 'plot 3. page? bx in current sheet'
    read, contin
    if (contin eq '' or contin eq 'y') then begin

      erase
      fa=interpolate(bx,ioyf,iotf,/grid)
;      fa=smooth(fa,3)
      surface,fa,yf,tf,position=[x1, y1, x2, y2, z1, z2],$
        ax=ang1, az=ang2,$
        ztitle='Bx',xstyle=1,ystyle=1,zstyle=1,$
           xtitle=xtit,ytitle='time',/noerase

    endif
    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then device,/close
    set_plot,'x'
    !P.CHARTHICK=1
    
  endwhile
 
end 

