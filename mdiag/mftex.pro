  time = fltarr(501,5)
  ptot=time & xp=time & zp=time & rho=time 
  bxp=time & byp=time & bzp=time 
  vxp=time & vyp=time & vzp=time 
  bmax=time & bmin=time & difb=time  
  xmax=time & xmin=time & difx=time 
  zmax=time & zmin=time & difz=time 
  delp=time
  again='y' & withps='n' & cas=strarr(5) & incase='1'
  cas(0)='1' & cas(1)='2' & cas(2)='3' & cas(3)='4' & cas(4)='5'
  head = strarr(4)
  fnumber=1 & nop=1 & itot=intarr(5)

  print, 'how many plots (n=1 digit<=5)'
  read, nop
  nop1=nop-1
  
 for iop=0, nop-1  do  begin 
  print, 'what filenumber'
  read, fnumber
  print, 'what case'
  read, incase
  if (incase ne '') then cas(iop)=incase
  
  openr, 2, 'magdfte'+string(fnumber,'(i1)')
  readf, 2, head
  print, head

  i=0
  while not eof(2) do begin
    readf, 2, times, ptots, xps, zps, rhos, bxps, byps, bzps
    readf, 2, bmaxs, xmaxs, zmaxs, vxps, vyps, vzps
    readf, 2, bmins, xmins, zmins, difbs, difxs, difzs 

    time(i,iop)=times & ptot(i,iop)=ptots & xp(i,iop)=xps   & zp(i,iop)=zps
    rho(i,iop)=rhos   & bxp(i,iop)=bxps   & byp(i,iop)=byps & bzp(i,iop)=bzps
    bmax(i,iop)=bmaxs & xmax(i,iop)=xmaxs & zmax(i,iop)=zmaxs
    vxp(i,iop)=vxps   & vyp(i,iop)=vyps   & vzp(i,iop)=vzps
    bmin(i,iop)=bmins & xmin(i,iop)=xmins & zmin(i,iop)=zmins
    difb(i,iop)=difbs & difx(i,iop)=difxs & difz(i,iop)=difzs

    itot(iop)=i
    if i lt 499 then i=i+1 else stop, ' to many records '
  endwhile
  close, 2
 endfor

     while again eq 'y' do begin
     
    dx=0.5 & dy=0.2 & dyint=0.12
    x1=0.2 & x2=x1+dx
    y11=0.1 & y21=y11+dy & y12=y21+dyint & y22=y12+dy
    y13=y22+dyint & y23=y13+dy 
     

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
     	set_plot,'ps'
        device,filename='fte.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif

  !P.CHARSIZE=2.0
  !P.REGION=[0., 0., 1.0, 1.25]
  !P.MULTI=[0, 0, 4]
  !P.POSITION=[x1, y11, x2, y21]
  
  ymin=min(zmin(*,0:nop1))
  ymax=max(zmin(*,0:nop1))
  tmin=0
  tmax=150
;  tmin=min(time(*,0:nop1))
;  tmax=max(time(*,0:nop1))
  trange=tmax-tmin
  plot, time(1:itot(0),0), zmax(1:itot(0),0),$
        title='Z Locations of BN Max',$
        xtitle='time', font=3,$
        xrange=[tmin,tmax], yrange=[ymin,ymax], $
        xstyle=1,ystyle=1
  for k=1,nop1 do  oplot, time(1:itot(k),k), zmax(1:itot(k),k), line=k
    xpos=tmax+0.05*trange
    iarr=max(where(time(*,0)<tmax))
    ypos=zmax(iarr,0) & xyouts,xpos,ypos,cas(0),font=3
    for k=1,nop1 do begin 
      iarr=max(where(time(*,k)<tmax))
      ypos=zmax(iarr,k) & xyouts,xpos,ypos,cas(k),font=3
    endfor

  !P.POSITION=[x1, y12, x2, y22]
  for k=0,nop1 do delp(1:itot(k),k)=(ptot(1:itot(k),k)-2.)/2.
  delp=(delp>0)
  plot, time(1:itot(0),0), delp(1:itot(0),0),$ 
        title='Total Pressure Perturbation',font=3, $ 
        xrange=[tmin,tmax], yrange=[0, max(delp(*,0:nop1))], $
        xstyle=1,ystyle=1
  for k=1,nop1 do  oplot, time(1:itot(k),k), delp(1:itot(k),k), line=k

  !P.POSITION=[x1, y13, x2, y23]
  plot, time(1:itot(0),0), 0.5*difb(1:itot(0),0),$
        title='BN Amplitude',$
        xtitle='time', font=3, $
        xrange=[tmin,tmax], yrange=[0,0.5*max(difb(*,0:nop1))], $
;        yrange=[0,0.4]
        xstyle=1,ystyle=1
  for k=1,nop1 do  oplot, time(1:itot(k),k), 0.5*difb(1:itot(k),k), line=k
    xpos=tmax+0.05*trange
    iarr=max(where(time(*,0)<tmax))
    ypos=0.5*difb(iarr,0) & xyouts,xpos,ypos,cas(0),font=3
    for k=1,nop1 do begin 
      iarr=max(where(time(*,k)<tmax))
      ypos=0.5*difb(iarr,k) & xyouts,xpos,ypos,cas(k),font=3
    endfor

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile

end

