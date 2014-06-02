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
   nxf = 61   &   nzf = 61
   ioxf=fltarr(nxf) & iozf=fltarr(nzf)
   fa=fltarr(nxf,nzf) & fb=fa
  names=strarr(15)
  names=replicate(' ',15)

;----PARAMETER-------
  xmin = 0.01 & zmin = -1.0
  xmax = 10.0 & zmax = 1.0
  nmin = 0   & nmax = 1 & kap = 1.7 & b = -0.7 & md = 1.0
;--------------------
   time=0.0 & fnumber=1
   name='' & contin='' & again='y' & withps='n' & run=''
   nx=long(103) & nz=long(103)

; READ INPUT DATA OF DIMENSION NX, NY
   x=fltarr(nx,/NOZERO) & hx=x & z=fltarr(nz,/NOZERO)
   a=fltarr(nx,nz,/NOZERO) & p=a & jz=a & ahelp=a
  
   delx=xmax-xmin & delz=zmax-zmin & sizeratio=(zmax-zmin)/(xmax-xmin)
   
; GRID FOR CONTOUR PLOTS
   x=findgen(nx) & z=findgen(nz)
   dx=(xmax-xmin)/float(nx-1) & x=x*dx+xmin
   dz=(zmax-zmin)/float(nz-1) & z=z*dz+zmin
  
; GRID FOR SURFACE PLOTS
   xf=findgen(nxf) & zf=findgen(nzf)
   dxf=(xmax-xmin)/float(nxf-1) & xf=xf*dxf+xmin
   dzf=(zmax-zmin)/float(nzf-1) & zf=zf*dzf+zmin
   in=-1 & k=0
   repeat begin
     in=in+1
     while xf(in) gt x(k+1) do k=k+1
     ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
   endrep until in eq nxf-1
   in=-1 & k=0
   repeat begin
     in=in+1
     while zf(in) gt z(k+1) do k=k+1
     iozf(in) = float(k) + (zf(in)-z(k))/(z(k+1)-z(k))        
   endrep until in eq nzf-1

; VECTORPOTENTIAL:
    pi=2*asin(1)
    a=0*a
    indg=where(x ge 0) 
    if (xmin lt 0) then indl=where(x lt 0) 
    for n=nmin,nmax do begin
      alpn=pi*(2.0*float(n)-1.0)/2.0 & lambn=sqrt(abs(alpn^2-kap^2))
      print, 'n, alphan, lambdan:', n, alpn, lambn
      indg=where(x gt 0) 
      hx(indg)=exp(-lambn*x(indg)) - exp(-abs(lambn*(2*b-x(indg))))
      if (xmin lt 0) then $
        hx(indl)=-exp(lambn*x(indl)) - exp(-abs(lambn*(2*b-x(indl))))
      hz=-0.5*md*cos(alpn*z)
      for i=0,nx-1 do ahelp(i,0:nz-1)=hx(i)*hz(0:nz-1)
      a=a+ahelp
    endfor
    fmax=max(a)
    fmin=min(a)
    print, fmax, fmin

; CURRENT DENSITY J_Z AND ELECTRIC FIELD E_Z
  
; COORDINATES FOR PLOTS
   srat=1.0
   print, sizeratio  
    dpx=0.9 & dpy=0.4
    xleft=0.06 & xinter=0.12 & hop=0.47*xinter
    xa1=xleft & xe1=xleft+dpx  
    xa2=xe1+xinter & xe2=xa2+dpx
    xa3=xe2+xinter & xe3=xa3+dpx
    ylo=0.06 & yup=ylo+dpy
   
   xpmin=xmin
   xpmax=xmax
   ytit='z'
     

    print, 'Which case?'
    read, run

     while (again eq 'y') do begin

      !P.THICK=1.
      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
        device,/landscape
        !P.THICK=2.
;        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
;        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
       endif

        xpos=xpmax-0.01*delx/dpx
        ypos0=zmin+0.2*delz         ; location for 'time'
        ypos1=ypos0-0.025*delz/dpy   ; next line
        ypos2=ypos0+0.075*delz/dpy   ; location 'CASE'
        ypos3=zmin+.8*delz          ; location 'Max='
        ypos4=ypos3-0.025*delz/dpy   ; next line
        ypos5=ypos4-0.05*delz/dpy         ; location 'Min='
        ypos6=ypos5-0.025*delz/dpy   ; next line

       
  print, 'plot first page? Magnetic field, flow vectors'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot first page
;       !P.REGION=[0.,0.,1.0,1.25]
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=3
       !X.TICKS=2
       !Y.TICKS=8
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
       !X.RANGE=[xpmin,xpmax]
       !Y.RANGE=[zmin,zmax]

        fa=interpolate(a,ioxf,iozf,/grid)
	bmax=max(fa,indmax) & bmin=min(fa,indmin) & del=(bmax-bmin)/16.
        ixmin=indmin mod nxf
	fmax=max(fa(ixmin,1:nzf-1))
        delf=fmax-fmin
        print, 'magnetic flux:', del
        fb=fmax+5.0
        lvec=findgen(17)*del+bmin & lvec(16)=fb
        lvec=lvec(sort(lvec))

	!P.POSITION=[xa1,ylo,xe1,yup]
	contour,fa,xf,zf,levels=lvec,$
        c_linestyle=lvec gt fb,$ 
        title=' Magnetic Field',xstyle=1,ystyle=1,$
        xtitle='x',ytitle=ytit

	!P.POSITION=[xa3,ylo,xe3,yup]
  endif


  print, 'continue with next plot, surface a'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

        !P.CHARSIZE=2.0
        !P.MULTI=[0,2,0,0,0]
        !P.FONT=-1
        
        f1=smooth(a,5)
        fa=interpolate(f1,ioxf,iozf,/grid)

    surface,fa,xf,zf,position=[0.6,0.2,0.85,1.2,0.4,0.9],ax=35,$
      ztitle='p_B',xstyle=1,ystyle=1,xtitle='x',ytitle=ytit,/noerase

  endif

     !P.FONT=3


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end

