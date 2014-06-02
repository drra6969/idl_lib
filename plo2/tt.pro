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
  xmin = 0.01 & ymin = -1.0
  xmax = 10.0 & ymax = 1.0
;--------------------
   time=0.0 & fnumber=1
   name='' & contin='' & again='y' & withps='n' & run=''
   nx=long(103) & ny=long(103)

; READ INPUT DATA OF DIMENSION NX, NY
   x=fltarr(nx,/NOZERO) & hx=x & y=fltarr(ny,/NOZERO)
   a=fltarr(nx,ny,/NOZERO) & p=a & bx=a & by=a & bz=a
  
   delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
   
; GRID FOR CONTOUR PLOTS
   x=findgen(nx) & y=findgen(ny)
   dx=(xmax-xmin)/float(nx-1) & x=x*dx+xmin
   dy=(ymax-ymin)/float(ny-1) & y=y*dy+ymin
  
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
    gamma = 5./3. & gammainv = 1./gamma
    rho0 = 0.25 & pinf=0.2 & bg=6. & dg=1. & ydist=35.

    cg = (1.-pinf)/( 2.^(bg+1.)*dg/bg-1.-tanh(dg) )
    ag = 1. - pinf + cg*(1.+tanh(dg))
    eg = pinf - cg
; parameter fuer joergs modell
      pfs=0.1 & iq=2 & b0=3. & l0=1.2 & n0=0.3 & alpha=0.3 & theta=0.25
      eta0 = 0.0359/(l0*sqrt(n0))
      kappa0 = 34.6*(l0)^0.5*(n0/theta)^0.25
      
      arg1 = (ydist + y)/ydist
      pq   = (1.-pfs)/arg1^iq + pfs
      pq1  = -iq*(1.-pfs)/arg1^(iq+1)
      kappa = -kappa0*pq1/(2.*ydist*pq^1.5)
      yd = y/ydist
;
;      ein gleichgewicht
;        arg2 = tanh(dg*(yd - 1.))
;        psheet = ag/arg1^bg + cg*arg2 + e
;        p1pu    = ( -ag*bg/arg1^(bg+1.)
;     +             + cg*dg*(1.-arg2^2) )/ydist
;        p2pu    = ( ag*bg*(bg+1)/arg1^(bg+2.)
;     +             - 2.*cg*dg^2.*arg2*(1.-arg2^2) )/ydist^2.
;        p3pu    = ( -ag*bg*(bg+1.)*(bg+2.)/arg1^(bg+3.)
;     +             + 2.*cg*dg^3.*(3*arg2^2-1.)*(1.-arg2^2) )
;     +              /ydist^3.
;
;      gleichgewicht fuer joergsmodell
;
      psheet = (1.- pfs)/arg1^iq + pfs
      p1pu =  -iq*(1.-pfs)/arg1^(iq+1) /ydist
      p2pu = iq*(iq+1)*(1.-pfs)/arg1^(iq+2) /ydist^2
      p3pu = -iq*(iq+1)*(iq+2)*(1.-pfs)/arg1^(iq+3) /ydist^3
      deltax = sqrt(-p1pu /(2.*alpha*psheet^2.5))

      lx = 1./sqrt(psheet)
      lnl1pu = -0.5*p1pu/psheet
      lnl2pu = -0.5*( p2pu/psheet - (p1pu/psheet)^2 )
      lnl3pu = -0.5*( p3pu/psheet - 3.*p1pu*p2pu/psheet^2 + 2.*(p1pu/psheet)^3 )
      bra   = lnl1pu^2 - lnl2pu
      brapu = 2.*lnl1pu*lnl2pu - lnl3pu

      xarg  = x*sqrt(psheet)
      tanhx  = tanh(xarg)
      cosh2x = 1. - tanhx^2
      
      for ix=0,nx-1 do begin
        p(ix,*) = ( psheet(*) + lnl2pu(*)*xarg(ix)^2 $
                       + bra(*)*xarg(ix)^3/3.*tanhx(ix) )*cosh2x(ix) + rho0
      endfor

end


