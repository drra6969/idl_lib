
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
;     ------->      ------->
;          -x             x

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, x,y, xf,yf, xn,yn, iox,ioy, $
                    ioxf,ioyf, run, time

; GRID FOR VELOCITY VECTORS
   nxn = 21   &   nyn = 21
   fn1=fltarr(nxn,nyn) & fn2=fn1
; GRID FOR CONTOUR AND SURFACE PLOTS
   nxf = 201   &   nyf = 201
   fa=fltarr(nxf,nyf) & fb=fa

   
;----PARAMETER-------
  xmin = -20. & ymin =  0.0
  xmax = 20. & ymax = 200.0
;--------------------
   time=0.0 & fnumber=1
   name='' & contin='' & again='y' & withps='n' & closeps='n'
   run='' & plot='0'  & coltab='o' &  withgif = 'n' &  contonly = 'n'
   glatt='n' & colps ='n' & grayplot ='n' 
   names=strarr(15) & names=replicate(' ',15)
   nl1=17  &  nl2=9  &  phi=0.0  &  pi = 3.14159265536  &  phir = phi*pi/180.0
   xtit='x' & ytit='z' & smo='n'   & nsh=0  & nshtot=nsh  &  galpar='a'
   nx=long(103) & ny=long(103) & orient='l'
   wlong=900 & wshort=720 & wfact=1.0 &  wxf=1.0 & wyf=1.0 & psfact=0.9
   wsize=[700,900] & wsold=[0,0] & wino=0 & srat0=1.1

; READ INPUT DATA OF DIMENSION NX, NY
read2d, g1,g2,g3,h1,h2,h3, bx,by,bz, vx,vy,vz, rho,u,res
   bsq=bx & p=bx & jz=bx & ez=bx
   f1=bx & f2=bx & bzpro=bx & vzpro=bx & vypro=bx
   a=fltarr(nx,ny) 

;   print, 'Which case?'
;   read, run

   testbd1, nx,ny,x,y,xmin,xmax,ymin,ymax   
   xpmin=xmax  &  xpmax=xmin
;   xpmin=xmin  &  xpmax=xmax
   
; CURRENT DENSITY J_Z AND ELECTRIC FIELD E_Z
jande, nx,ny,g1,h1,bx,by,vx,vy,res,jz,ez
  
; VECTORPOTENTIAL:
vecpot, nx,ny,x,y,bx,by,a,fmin,fmax
    print, 'vectorpotential:',fmax, fmin
    
    p=2*u^(5.0/3.0)   & bsq=by*by+bx*bx+bz*bz
    ny=ny-2
    
; GRID FOR VELOCITY VECTORS 
grid2d, x,y,xmin,xmax,ymin,ymax,nxn,nyn,xn,yn,iox,ioy,dxn,dyn

; GRID FOR CONTOUR/SURFACE PLOTS
grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf





   nt=400 & ttime=fltarr(nt,/NOZERO) & ti=0.0
   mx=long(103)
   again='y' & withps='n' & contin='y'

; READ INPUT DATA OF DIMENSION NX, NY
   openr, 7, 'm2by',/F77_UNFORMATTED
   readu, 7,  mx
   print, 'dimension mx=',mx

   xx=fltarr(mx,/NOZERO) 
   bb=fltarr(mx,nt,/NOZERO)
   b1=xx

   readu, 7,  xx
   xxmin=x(1) & xxmax=x(nx-2)

   it=0
   while not eof(7) do begin
      readu, 7,  ti
      readu, 7,  b1
      ttime(it)=ti
      bb(*,it)=b1
      it=it+1
   endwhile
   close, 7
   itot=it-1
   bb=bb/7.5
   print,itot

icoo=''
ix1=11 & ix2=90  & nnx=ix2-ix1+1
iyy=41

     while again eq 'y' do begin
     print, 'Input iy'
     read, icoo
     if icoo ne '' then iyy=fix(icoo)
     print, 'iy:', iyy, '   y(iyy):', y(iyy)
     

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='flux.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif
; first page
  !P.REGION=[0.,0.,1.0,1.25]


    !P.CHARSIZE=1.0  
    !P.MULTI=[0,0,2]
    !P.POSITION=[0.2,0.6,0.8,0.9]
    amax=max(bb)
    amin=0.3
    plot, xx, bb(*,0), line=0,$
        title='Flux of By',$
        font=3, yrange=[amin,amax],xrange=[0,100],xtitle='z'
    for in=1, itot,4 do oplot, xx, bb(*,in), line=0



bbz=by(ix1:ix2,iyy) & bby=bz(ix1:ix2,iyy) 
bzmax=max(bbz) & bzmin=min(bbz) 
bymax=max(bby) & bymin=min(bby) 

    !P.POSITION=[0.33,0.2,0.7,0.5]
    plot, bby, bbz, line=0,$
        title='Bz vs By',$
        font=3, xrange=[-1.,1.],yrange=[-1.,1.],$
;        font=3, xrange=[bymin,bymax],yrange=[bzmin,bzmax],$
        xtitle='By',ytitle='Bz'



     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'

   endwhile


end

