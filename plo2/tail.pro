; MAIN PROGRAM
;   program reads data from 2D simulations(x/y) 
;   and substitutes the simulation y direction with 
;   z for plotting data from MP simulations
;      PLOT        SIMULATION
;     -------       -------
;  z !       !    y!       !
;    !       !     !       !
;    !       !     !       !
;    !       !     !       !
;    !       !     !       !
;    <-------       ------->
;      x                  x

; GRID FOR CONTOUR AND SURFACE PLOTS
   nxf = 61   &   nyf = 61
   ioxf=fltarr(nxf) & ioyf=fltarr(nyf)
   fa=fltarr(nxf,nyf) & fb=fa
  names=strarr(15)
  names=replicate(' ',15)

;----PARAMETER-------
  xmin = -0.0 & ymin = -10.0
  xmax = 100.0 & ymax = 10.0
;--------------------
   time=0.0 & fnumber=1
   name='' & contin='' & again='y' & withps='n' & run=''
   nx=long(153) & ny=long(153) & nyhalf=(ny+1)/2


; READ INPUT DATA OF DIMENSION NX, NY
   x=fltarr(nx,/NOZERO) & hx=x & y=fltarr(ny,/NOZERO)
   a=fltarr(nx,ny,/NOZERO) & p=a & bx=a & by=a & bz=a
  
   delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
   
; GRID FOR CONTOUR PLOTS
   x=findgen(nx) & y=findgen(ny)
   dx=(xmax-xmin)/float(nx-3) & x=x*dx+xmin-dx
   dy=(ymax-ymin)/float(ny-3) & y=y*dy+ymin-dy
  
; GRID FOR SURFACE PLOTS
   xf=findgen(nxf) & yf=findgen(nyf)
   dxf=(xmax-xmin)/float(nxf-1) & xf=xf*dxf+xmin
   dyf=(ymax-ymin)/float(nyf-1) & yf=yf*dyf+ymin
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

; TAILMODEL
;   1. PARAMETERS:
    rho0 = 0.25 & pinf=0.12 & bg=6. & dg=1. & xdist=80. & bz0=0.1

    cg = (1.-pinf)/( 2.^(bg+1.)*dg/bg-1.-tanh(dg) )
    ag = 1. - pinf + cg*(1.+tanh(dg))
    eg = pinf - cg
; parameter fuer joergs modell
      pfs=0.05 & iq=2 & b0=3. & l0=1.2 & n0=0.3 & alpha=0.3 & theta=0.25
      eta0 = 0.0359/(l0*sqrt(n0))
      kappa0 = 34.6*(l0)^0.5*(n0/theta)^0.25
      
      arg1 = (xdist + x)/xdist
      pq   = (1.-pfs)/arg1^iq + pfs
      pq1  = -iq*(1.-pfs)/arg1^(iq+1)
      kappa = -kappa0*pq1/(2.*xdist*pq^1.5)
      yd = x/xdist
;
;      ein gleichgewicht
        arg2 = tanh(dg*(yd - 1.))
        psheet = ag/arg1^bg + cg*arg2 + eg
        p1pu    = ( -ag*bg/arg1^(bg+1.) $
                  + cg*dg*(1.-arg2^2) )/xdist
        p2pu    = ( ag*bg*(bg+1)/arg1^(bg+2.) $
                  - 2.*cg*dg^2.*arg2*(1.-arg2^2) )/xdist^2.
        p3pu    = ( -ag*bg*(bg+1.)*(bg+2.)/arg1^(bg+3.) $
                  + 2.*cg*dg^3.*(3*arg2^2-1.)*(1.-arg2^2) ) $
                   /xdist^3.
;
;      gleichgewicht fuer joergsmodell
;
;      psheet = (1.- pfs)/arg1^iq + pfs
;      p1pu =  -iq*(1.-pfs)/arg1^(iq+1) /xdist
;      p2pu = iq*(iq+1)*(1.-pfs)/arg1^(iq+2) /xdist^2
;      p3pu = -iq*(iq+1)*(iq+2)*(1.-pfs)/arg1^(iq+3) /xdist^3
;      deltax = sqrt(-p1pu /(2.*alpha*psheet^2.5))

      lx = 1./sqrt(psheet)
      lnl1pu = -0.5*p1pu/psheet
      lnl2pu = -0.5*( p2pu/psheet - (p1pu/psheet)^2 )
      lnl3pu = -0.5*( p3pu/psheet - 3.*p1pu*p2pu/psheet^2 + 2.*(p1pu/psheet)^3 )
      bra   = lnl1pu^2 - lnl2pu
      brapu = 2.*lnl1pu*lnl2pu - lnl3pu

      yarg=p & tanhy=p & cosh2y=p
      for iy=0,ny-1 do begin
        yarg(*,iy)=y(iy)*sqrt(psheet(*))
      endfor
      tanhy=tanh(yarg) & cosh2y=1.-tanhy^2
      for iy=0,ny-1 do begin
        p(*,iy) = ( psheet(*) + lnl2pu(*)*yarg(*,iy)^2$
                    + bra(*)*yarg(*,iy)^3/3.*tanhy(*,iy) )*cosh2y(*,iy) + rho0
	by(*,iy)  = - lnl1pu(*)*(1. - yarg(*,iy)*tanhy(*,iy))$
           + ( lnl3pu(*)*yarg(*,iy)^2/2.$
               - lnl1pu(*)*bra(*)*yarg(*,iy)^4/6.*cosh2y(*,iy)$
               + ( brapu(*)-lnl1pu(*)*bra(*) )*yarg(*,iy)^3/6.*tanhy(*,iy) )$
               / psheet(*)
	bx(*,iy)  = sqrt(psheet(*))*tanhy(*,iy)$
                  - lx(*)*( lnl2pu(*)*yarg(*,iy)$
                       + bra(*)*( yarg(*,iy)^2/2.*tanhy(*,iy)$
                                    + yarg(*,iy)^3/6.*cosh2y(*,iy) ) )
        bz(*,iy) = bz0
      endfor
      bn=psheet & bn=by(*,nyhalf)
      bl=psheet & bl=bx(*,ny-2)


; VECTORPOTENTIAL:
    a=0.0*bx
    for k=3, ny-2, 2 do $
        a(1,k)=a(1,k-2)+bx(1,k-1)*(y(k)-y(k-2))
    for k=2, ny-3, 2 do $
        a(1,k)=a(1,k-1)+0.5*(bx(1,k-1)+bx(1,k))*(y(k)-y(k-1))
    for k=1, ny-1 do begin
     for l=3, nx-2,2 do begin
        a(l,k)=a(l-2,k)-by(l-1,k)*(x(l)-x(l-2))
     endfor  
    endfor
    for k=1, ny-1 do $
     for l=2, nx-3,2 do $
        a(l,k)=a(l-1,k)-0.5*(by(l-1,k)+by(l,k))*(x(l)-x(l-1))
    fmax=max(a((nx-1)/2,2:ny-1))
    fmin=min(a((nx-1)/2,2:ny-1))
    print, fmax, fmin

; CURRENT DENSITY J_Z AND ELECTRIC FIELD E_Z
  
; COORDINATES FOR PLOTS
   srat=1.0
   print, sizeratio  
     
    xleft=0.1 & dpx=0.85 & xinter=0.12 
    xa1=xleft & xe1=xleft+dpx  
    xa2=xe1+xinter & xe2=xa2+dpx
    xa3=xe2+xinter & xe3=xa3+dpx
    yinter=0.09 & dpy=0.2
    yup1=0.95 & ylo1=yup1-dpy
    yup2=ylo1-yinter & ylo2=yup2-dpy
    yup3=ylo2-yinter & ylo3=yup3-dpy
   
   ytit='y'
     

    print, 'Which case?'
    read, run

     while (again eq 'y') do begin

      !P.THICK=1.
      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
;        device,/landscape
        !P.THICK=2.
        device,/inches,xsize=8.,scale_factor=1.0
        device,/inches,ysize=10.0,scale_factor=1.0
;        device,/times,/bold,font_index=3
       endif

        xpos=xmax-0.01*delx/dpx
        ypos0=ymin+0.2*dely         ; location for 'time'
        ypos1=ypos0-0.025*dely/dpy   ; next line
        ypos2=ypos0+0.075*dely/dpy   ; location 'CASE'
        ypos3=ymin+.8*dely          ; location 'Max='
        ypos4=ypos3-0.025*dely/dpy   ; next line
        ypos5=ypos4-0.05*dely/dpy         ; location 'Min='
        ypos6=ypos5-0.025*dely/dpy   ; next line

       
  print, 'plot first page? Magnetic field, flow vectors'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot first page
;       !P.REGION=[0.,0.,1.0,1.25]
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=3
       !X.TICKS=5
       !Y.TICKS=4
       !Y.TICKlen=0.01
       !X.TICKlen=0.05
       !X.THICK=2
       !Y.THICK=2
       !X.RANGE=[xmin,xmax]
       !Y.RANGE=[ymin,ymax]

        nyhalf=(ny+1)/2
        fmax=max(a)
	amax=max(a(1:nx-2,nyhalf),indmax) & amin=min(a(1:nx-2,nyhalf),indmin) 
	del=(fmax-amin)/20.
        delf=amax-amin
        print, 'magnetic flux:', del
        lvec=findgen(21)*del+amin & lvec(20)=amax
        lvec=lvec(sort(lvec))

	!P.POSITION=[xa1,ylo1,xe1,yup1]
	contour,a,x,y,levels=lvec,$
        c_linestyle=lvec gt fb,$ 
        title=' Magnetic Field',xstyle=1,ystyle=1,$
        xtitle='x',ytitle=ytit

	!P.POSITION=[xa1,ylo2,xe1,yup2]
	contour,p,x,y,nlevels=28,$
        title=' Pressure',xstyle=1,ystyle=1,$
        xtitle='x',ytitle=ytit

        pmax=max(psheet(1:nx-2))
        !Y.TICKS=5
	!P.POSITION=[xa1,ylo3,xe1,yup3]
	plot, x(1:nx-2),psheet(1:nx-2),yrange=[0.,pmax],xstyle=1,ystyle=1,$
	xtitle='x',ytitle='P'
	oplot, x(1:nx-2),-10*bn(1:nx-2),linestyle=1
	oplot, x(1:nx-2),bl(1:nx-2),linestyle=2
	
  endif



     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end

