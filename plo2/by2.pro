
; MAIN PROGRAM
;   program reads data from 2D simulations(x/y) 
;   and substitutes the simulation y direction with 
;   z for plotting data from MP simulations
;      PLOT        SIMULATION
;     -------       -------
;  y !       !    z!       !
;    !       !     !       !
;    !       !     !       !
;    !       !     !       !
;    !       !     !       !
;     ------->      ------->
;           x             y
;
;  to read unformatted magdb* files
;
COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, x,y, xf,yf, xn,yn, iox,ioy, $
                    ioxf,ioyf, run, time

; GRID FOR VELOCITY VECTORS
   nxn = 21   &   nyn = 21
   fn1=fltarr(nxn,nyn) & fn2=fn1
; GRID FOR CONTOUR AND SURFACE PLOTS
   nxf = 121   &   nyf = 121
   fa=fltarr(nxf,nyf) & fb=fa
  again='y' & withps='n' &  
;----PARAMETER-------
  xmin = -50. & ymin =  0.0
  xmax =  50. & ymax = 100.0
;--------------------
   nl1=17  &  nl2=9
; READ INPUT DATA OF DIMENSION NX, NY
   fnumber=100

while again eq 'y' do begin
      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps' 
        device,filename='bystack.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif

   print, 'Input filenumber'
   read, fnumber

   itot=50
;   WINDOW, XSIZE=750, YSIZE=550, TITLE='Magnetic Flux Along Y'      
;   xanimate,set=[750,550,(itot+1),0]
   ttime=fltarr(4)

;     ix0=(nx+1)/2
;     bb(ix0-1,*)=0.34 & bb(ix0,*)=0.34 & bb(ix0+1,*)=0.34 

     name='midlby'+string(fnumber,'(i3.3)')
     restore, file=name
     xpmin=xmin  &  xpmax=xmax



  xs=fltarr(2) & xe=xs & xsb=xs & xeb=xs
  yl=xs & yup=xs
  xs(0)=0.2    & xs(1)= xs(0)
  xwidth=0.6    & xdist=0.04 & xebd=0.02
  xe=xs+xwidth & xsb=xe+xdist & xeb=xsb+xebd
  yl(0)=0.6    & yl(1)=0.2
  yhi=0.3      
  yup=yl+yhi
  
  names=strarr(15)
  names=replicate(' ',15)

;   !P.REGION=[0.,0.,1.0,1.25]
  !P.REGION=[0.,0.,1.0,1.0]
  !P.CHARSIZE=1.3
  !P.FONT=3
  !P.THICK=1.
  !X.TICKS=4
  !Y.TICKS=4
  !Y.TICKlen=0.04
  !X.THICK=2
  !Y.THICK=2

  !X.RANGE=[xpmin,xpmax]
  !Y.RANGE=[ymin,ymax]
   !P.MULTI=[0,2,0,0,0]

;   xloadct,block=1

        
;   bmax=max(bby) & bmin=min(bby)
     


ytit='z'

        i=0
        !P.POSITION=[xs(i),yl(i),xe(i),yup(i)]
        amax=max(bb)
        amin=0.3
        plot, y, bb(1,*), line=0,$
        title='By',$
        font=3, yrange=[0.3,0.7],xrange=[0,100],xtitle='z',xstyle=1
        for in=2, nx-2,4 do oplot, y, 1.05*bb(in,*), line=0

        i=1
        !P.POSITION=[xs(i),yl(i),xe(i),yup(i)]
;        dx=shift(x,-1)-shift(x,1) & dx(0)=dx(1) & dx(nx-1)=dx(nx-2)
        bt=fltarr(ny)
        bt=total(bb,1)/float(nx-1)
        plot, y, 1.05*bt, line=0,thick=4,$
        title='By-average',xstyle=1,ystyle=1,$
        font=3, yrange=[0.3,0.7],xrange=[0,100],xtitle='z'
        

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'

   endwhile

end

