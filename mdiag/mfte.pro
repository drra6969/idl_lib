  time = fltarr(501)
  ptot=time & xp=time & zp=time & rho=time 
  bxp=time & byp=time & bzp=time 
  vxp=time & vyp=time & vzp=time 
  bmax=time & bmin=time & difb=time  
  xmax=time & xmin=time & difx=time 
  zmax=time & zmin=time & difz=time 
  delp=time
  again='y' & withps='n' & fall=''
  head = strarr(4)

  openr, 2, 'magdfte'
  readf, 2, head
  print, head

  i=0
  while not eof(2) do begin
    readf, 2, times, ptots, xps, zps, rhos, bxps, byps, bzps
    readf, 2, bmaxs, xmaxs, zmaxs, vxps, vyps, vzps
    readf, 2, bmins, xmins, zmins, difbs, difxs, difzs 

    time(i)=times & ptot(i)=ptots & xp(i)=xps   & zp(i)=zps
    rho(i)=rhos   & bxp(i)=bxps   & byp(i)=byps & bzp(i)=bzps
    bmax(i)=bmaxs & xmax(i)=xmaxs & zmax(i)=zmaxs
    vxp(i)=vxps   & vyp(i)=vyps   & vzp(i)=vzps
    bmin(i)=bmins & xmin(i)=xmins & zmin(i)=zmins
    difb(i)=difbs & difx(i)=difxs & difz(i)=difzs

    itot=i
    if i lt 499 then i=i+1 else stop, ' to many records '
  endwhile
  close, 2

     print, 'Which case?'
     read, fall

     while again eq 'y' do begin

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
  !P.POSITION=[0.15, 0.07, 0.7, 0.27]
  plot, time(1:itot), zmax(1:itot),$
        title='Z Locations of BN Max and Min',$
        xtitle='time', font=3,$
        yrange=[0,200]
;        yrange=[min(zmin(1:itot)),max(zmax(1:itot))]
  oplot, time(1:itot), zmin(1:itot), line=2

  !P.POSITION=[0.15, 0.4, 0.7, 0.6]
  delp=(ptot-2.)/2.
  delp=(delp>0)
  plot, time(1:itot), delp(1:itot),$ 
  title='Total Pressure Perturbation',$ 
;  font=3, yrange=[0, .5]
  font=3, yrange=[0, max(delp(1:itot))]

  !P.POSITION=[0.15, 0.75, 0.7, 0.95]
  plot, time(1:itot), 0.5*difb(1:itot),$
        title='BN Amplitude',$
        xtitle='time', font=3,yrange=[0,0.4]
;        yrange=[0,.7]
;        yrange=[0,0.5*max(difb(1:itot))]
    xpos=time(itot)+0.05*time(itot)
    ypos=0.5*max(difb(1:itot))
    xyouts,xpos,ypos,fall,font=3

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile

end

