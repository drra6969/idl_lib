
  nx=65 & ny=203 
  nxn=25 & nyn=25
  nxf=51 & nyf=51
  mx=65 & my=203 & mpos=1 & mrow=1 
  b2=bytarr(2)
  x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
  g1=fltarr(nx)
  g2=g1 & g3=g1 & g4=g1 & g5=g1 & g6=g1 & g7=g1 
  h1=fltarr(ny)
  h2=h1 & h3=h1 & h4=h1 & h5=h1 & h6=h1 & h7=h1
  iox=fltarr(nxn) & ioy=fltarr(nyn)
  fn1=fltarr(nxn,nyn) & fn2=fn1 & fn3=fn1 & fn4=fn1 
  ioxf=fltarr(nxf) & ioyf=fltarr(nyf)
  fa=fltarr(nx) & fa1=fa
  fb=fltarr(ny) & fb1=fb

  bx=fltarr(nx,ny,/NOZERO)
  by=bx & bz=bx & sx=bx & sy=bx & sy=bx
  rho=bx & u=bx & res=bx & prof=bx
  bsq=bx & p=bx & vz=bx & jz=bx & ez=bx
  f1=bx & f2=bx & f3=bx 
  a=fltarr(nx,ny) 
  time=0 & fnumber=1 & icut=0
  name='' & cutx=''& cuty=''  
  again='y' & withps='n' & run=''

  ISCHRITT=1 & IOUT=1 & ISAFE=1 & IEND=1 & INTART=1 & IVISRHO=1 
  NGLATT=1 & NOUT=1 & IFEHLER=1 & IGRID=1 & IDIAG=1 & FEHLER='' 
  DT=1.0 & ZEIT=DT & GAMMA=DT & ETA=DT &  VISX=DT & VISY=DT  
  PHI=DT & BMSH=DT & PMSP=DT & KAPPA=DT & RHO0=DT & DELRHO=DT 
  XRHO=DT & DXRHO=DT &  DELBZ=DT & BY0=DT & BZ0=DT & BZMSP=DT
  BZMSH=DT & PMSH=DT & BETASP=DT & BETASH=DT &  VAMSP=DT 
  VAMSH=DT & CSMSP=DT & CSMSH=DT 

  print, 'Input filenumber'
  read, fnumber
  name='MAGTAP'+string(fnumber,'(i1)')
  openr, 8, name,/F77_UNFORMATTED

  readu, 8,  mx,b2,my,b2,mpos,b2,mrow
  readu, 8,  x,g1,g2,g3,g4,g5,g6, y,h1,h2,h3,h4,h5,h6
  readu, 8,  bx,by,bz
  readu, 8,  sx,sy,sz
  readu, 8,  rho,u,res,prof
  close, 8

  print, mx,my,mpos,mrow
;  print, x
;  print, y

;----PARAMETER----
  xmin = -10  &   ymin =   0
  xmax =  10  &   ymax = 100
;  xmin = -14  &   ymin = y(2)
;  xmax =  14  &   ymax = 150  

;  xminn = xmin &  yminn = ymin  ;for interpolated fields(vector)
;  xmaxn = xmax &  ymaxn = ymax   
  xminn = xmin+1 &  yminn = ymin+1  ;for interpolated fields(vector)
  xmaxn = xmax-1 &  ymaxn = ymax-1   
  time = fnumber*25
  fall = 'PP'
  ixmin=0
  ixmax=nx-1
  iymin=0
  iymax=ny-1

; Computation of Vectorpotential a:
    a=0*bx
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
    print, fmax
    print, fmin

; Computation of current density jz and e field ez

  f1=shift(by,-1,0)-shift(by,1,0)
  f2=shift(bx,0,-1)-shift(bx,0,1)
  for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
  for i=0,nx-1 do f2(i,*) = h1*f2(i,*)
  jz=f1-f2 
  jz(0,*)=0. & jz(nx-1,*)=0. & jz(*,0)=0. &jz(*,ny-1)=0.
  ez=-(sx*by-sy*bx)/rho +res*jz
  rho=smooth(rho,3)
  sx=smooth(sx,3)
  bx=smooth(bx,3) 
  jz=smooth(jz,3)
  ez=smooth(ez,3) 

    print, 'Which case?'
    read, run
    fall='run:'+run

     while (again eq 'y' or again eq '') do begin

       !P.REGION=[0.,0.,1.0,1.25]
       !P.MULTI=[0,0,0,5,0]
       !P.CHARSIZE=1.0

      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='cut.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif

        xpos=xmax+0.05*(xmax-xmin)
        ypos1=ymin+.4*(ymax-ymin)
        ypos2=ymin+.6*(ymax-ymin)
        ypos3=ymin+.75*(ymax-ymin)
        ypos4=ymin+.72*(ymax-ymin)
        ypos5=ymin+.9*(ymax-ymin)
        ypos6=ymin+.87*(ymax-ymin)
        xpost=xmin+.2*(xmax-xmin)
        ypost=ymax+.01*(ymax-ymin)
       
  print, 'cut along x'
  read, cutx
  if (cutx eq '' or cutx eq 'y') then begin

    i=1
    while i le ny do begin 
          print, 'y(',i,')',y(i) 
          i=i+10 
    endwhile
    print,'which index iy'
    read, icut
    if icut le 0 then icut=0
    if icut gt ny-1 then icut=ny-1

    !P.POSITION=[0.1,0.05,0.75,0.2]
    plot, x, rho(*,icut),$
        title='Density',$
        xtitle='time', font=3, xrange=[xmin, xmax],yticks=3
 
    fa=abs(bx(*,icut))+(abs(bx(*,icut)) lt 0.03)*1.0
    fa=sx(*,icut)/sqrt(rho(*,icut))/fa
    !P.POSITION=[0.1,0.25,0.75,0.4]
    plot, x, fa,$
        title='VN / V_AN',$
        font=3, xrange=[xmin, xmax],yticks=3
 
    fa=sx(*,icut)
    !P.POSITION=[0.1,0.45,0.75,0.6]
    plot, x, fa,$
        title='VN * RHO',$
        font=3, xrange=[xmin, xmax],yticks=3

    fa=jz(*,icut)
    !P.POSITION=[0.1,0.65,0.75,0.8]
    plot, x, fa,$
        title='Current Density JZ',$
        font=3, xrange=[xmin, xmax],yticks=3

    fa=ez(*,icut)
    !P.POSITION=[0.1,0.85,0.75,1.0]
    plot, x, fa,$
        title='Electric Field EZ',$
        font=3, xrange=[xmin, xmax],yticks=3



   endif


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile

end

