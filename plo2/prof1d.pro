; MAIN PROGRAM
;  Program to plot cuts through 2D simulation data along x 
;                       (perp to current sheet)
;   program reads data from 2D simulations(x/y) 

;----PARAMETER-------
  xmin = -60.0 &   ymin = 0.0
  xmax =  60.0 &   ymax = 120.0
;--------------------

    ixp=293
    dxp=0
;    ipp=46
;    dpp=24
    ipp=3
    dpp=0
   time=0.0 & fnumber=1
   name='' & contin='' & again='y' & withps='n' & run=''
   nx=long(303) & ny=long(303)
  cas=strarr(20) & for k=0,6 do cas(k)=string((k+1),'(i1)')

  print, 'how many plots'
  read, nop
  nop1=nop-1
  vxprof=fltarr(nx,nop) & bprof=vxprof & pprof=vxprof

; READ INPUT DATA OF DIMENSION NX, NY

 for iop=0, nop-1  do  begin 
   iop1=iop+1
   name='magtap'+string(iop1,'(i1)')
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,time
   print, 'dimension nx=',nx,'     ny=',ny
   cas(iop)=string(time,'(f4.0)')

   x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
   g1=fltarr(nx,/NOZERO) & g2=g1 & g3=g1 
   h1=fltarr(ny,/NOZERO) & h2=h1 & h3=h1 
   bx=fltarr(nx,ny,/NOZERO) & by=bx & bz=bx & vx=bx & vy=bx & vy=bx
   rho=bx & u=bx & res=bx
   bsq=bx & p=bx & jz=bx & ez=bx
   f1=bx & f2=bx & f3=bx 
   a=fltarr(nx,ny) 

   readu, 8,  x,g1,g2,g3,g3,g3,g3, y,h1,h2,h3,h3,h3,h3
   print, y(ipp+dpp*iop)
;  print, x
;  print, y
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz
   readu, 8,  rho,u,res
   close, 8
   vx=vx/rho & vy=vy/rho & vz=vz/rho 
   vxprof(*,iop)=vx(*,ipp+dpp*iop)
   bprof(*,iop)=sqrt(bx(*,ipp+dpp*iop)^2+by(*,ipp+dpp*iop)^2 $
                                      +bz(*,ipp+dpp*iop)^2)
   pprof(*,iop)=2*u(*,ipp+dpp*iop)^(5.0/3.0)
;   vxprof(*,iop)=pprof(*,iop)+bprof(*,iop)^2
 endfor
   
   
   while again eq 'y' do begin

    dx=0.57 & dy=0.23 & dyint=0.1
    x1=0.2 & x2=x1+dx
    y11=0.08 & y21=y11+dy & y12=y21+dyint & y22=y12+dy
    y13=y22+dyint & y23=y13+dy 
    pxmin=xmax & pxmax=xmin
    
    print, 'With postscript?'
    read, withps
     if withps eq 'y' then begin 
      !P.THICK=2
      set_plot,'ps'
      device,filename='sat.ps'
      device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
      device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
      device,/times,/bold,font_index=3
     endif

    !P.REGION=[0.,0.,1.0,1.25]
    !P.MULTI=[0,0,4]
    !P.CHARSIZE=2.0  
    !X.THICK=2
    !Y.THICK=2

    print, 'plot page?'
    
     !P.POSITION=[x1, y11, x2, y21]
     ixp=203
     dxp=0
     ymin=min(vxprof(*,0:nop1))
     ymax=max(vxprof(*,0:nop1))
     ydiff=ymax-ymin
     plot, x, vxprof(*,0),$
;        title='Total Pressure',$
        title='VX Profile',$
        xtitle='X', font=3,$
        xrange=[pxmin,pxmax], yrange=[ymin,ymax], $
        xstyle=1,ystyle=1
     for k=1,nop1 do  oplot, x, vxprof(*,k), line=k
      xpos=x(ixp)
      ypos=vxprof(ixp,0)-0.05*ydiff 
      xyouts,xpos,ypos,cas(0),font=3
      for k=1,nop1 do begin 
        xpos=x(ixp+dxp*k)
        ypos=vxprof(ixp+dxp*k,k)-0.05*ydiff 
        xyouts,xpos,ypos,cas(k),font=3
      endfor

     !P.POSITION=[x1, y12, x2, y22]
     ymin=min(bprof(*,0:nop1))
     ymax=max(bprof(*,0:nop1))
     plot, x, bprof(*,0),$
        title='B Profile',$
        xtitle='X', font=3,$
        xrange=[pxmin,pxmax], yrange=[ymin,ymax], $
        xstyle=1,ystyle=1
     for k=1,nop1 do  oplot, x, bprof(*,k), line=k
      xpos=x(ixp)
      ypos=bprof(ixp,0) & xyouts,xpos,ypos,cas(0),font=3
      for k=1,nop1 do begin 
        xpos=x(ixp+dxp*k)
        ypos=bprof(ixp+dxp*k,k) & xyouts,xpos,ypos,cas(k),font=3
      endfor

     !P.POSITION=[x1, y13, x2, y23]
     ymin=min(pprof(*,0:nop1))
     ymax=max(pprof(*,0:nop1))
     plot, x, pprof(*,0),$
        title='P Profile',$
        xtitle='X', font=3,$
        xrange=[pxmin,pxmax], yrange=[ymin,ymax], $
        xstyle=1,ystyle=1
     for k=1,nop1 do  oplot, x, pprof(*,k), line=k
      xpos=x(ixp)
      ypos=pprof(ixp,0) & xyouts,xpos,ypos,cas(0),font=3
      for k=1,nop1 do begin 
        xpos=x(ixp+dxp*k)
        ypos=pprof(ixp+dxp*k,k) & xyouts,xpos,ypos,cas(k),font=3
      endfor
  


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1
   endwhile

end

