
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

;   print, 'Input filenumber'
;   read, fnumber

   itot=50
;   WINDOW, XSIZE=750, YSIZE=550, TITLE='Magnetic Flux Along Y'      
;   xanimate,set=[750,550,(itot+1),0]
   fnumber=intarr(4)
   ttime=fltarr(4)

   fnumber(0)=100
   fnumber(1)=200
   fnumber(2)=300
   fnumber(3)=390

     name='midlbx'+string(fnumber(0),'(i3.3)')
     restore, file=name
     ix0=(nx+1)/2
;     bb(ix0-1,*)=0.34 & bb(ix0,*)=0.34 & bb(ix0+1,*)=0.34 
     bbx=fltarr(nx,ny,4)
     bbx(*,*,0)=7.5*bb & ttime(0)=time
   xpmin=xmin  &  xpmax=xmax
   grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf

   for i=1,3 do begin
     name='midlbx'+string(fnumber(i),'(i3.3)')
     restore, file=name
;     bb(ix0-1,*)=0.34 & bb(ix0,*)=0.34 & bb(ix0+1,*)=0.34 
     bbx(*,*,i)=7.5*bb & ttime(i)=time
   endfor


  xs=fltarr(4) & xe=xs & xsb=xs & xeb=xs
  yl=xs & yup=xs
  xs(0)=0.07    & xs(1)=0.5 
  xs(2)=xs(0)  & xs(3)=xs(1)
  xwidth=0.3  & xdist=0.04 & xebd=0.02
  xe=xs+xwidth & xsb=xe+xdist & xeb=xsb+xebd
  yl(0)=0.6    & yl(1)=yl(0)
  yl(2)=0.2    & yl(3)=yl(2)
  yhi=0.27      
  yup=yl+yhi
  
  names=strarr(15)
  names=replicate(' ',15)

;   !P.REGION=[0.,0.,1.0,1.25]
  !P.REGION=[0.,0.,1.0,1.0]
  !P.CHARSIZE=1.5
  !P.FONT=3
  !P.THICK=1.
  !X.TICKS=4
  !Y.TICKS=4
  !Y.TICKlen=0.04
  !X.THICK=2
  !Y.THICK=2

  !X.RANGE=[xpmin,xpmax]
  !Y.RANGE=[ymin,ymax]
   !P.MULTI=[0,4,2,0,0]

;   xloadct,block=1

        
;   bmax=max(bby) & bmin=min(bby)
     

while again eq 'y' do begin
      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps' & ncol=!D.TABLE_SIZE & print,!D.TABLE_SIZE
         setcol, 'n'
          device,/color,bits_per_pixel=8
        device,filename='bxflux.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif

ytit='z'

for i=0, 3 do begin 

    fa=interpolate(bbx(*,*,i),ioxf,ioyf,/grid) 
    fa=smooth(fa,3)
;    fa(60,1)=bmin  &  fa(60,120)=bmax
    if ( (i eq 1) or (i eq 3) ) then ytit=' ' else ytit='z'
	fmax=max(fa) & fmin=min(fa)
        if ( (fmax-fmin) lt 0.000001) then begin
          fmax=fmax+0.05 & fmin=fmin-0.05 & endif

        xpos=0.65*xmax
        ypos=1.04*ymax
        !P.POSITION=[xs(i),yl(i),xe(i),yup(i)]
        IMAGE_C, fa
	bmax=max(fa) & bmin=min(fa) &  
        bav=0.5*(bmax+bmin)
        if ( (bmax-bmin) lt 0.000001) then begin
          bmax=bmax+0.0000005 & bmin=bmin-0.0000005 & endif
        del=(bmax-bmin)/(nl2-1.)
        cb=0.
	contour,fa,xf,yf,levels=findgen(nl2)*del+bmin,$
;        c_linestyle=findgen(nl2)*del+bmin lt .34,$
        c_linestyle=1,$
        title='Bx Flux',xstyle=1,ystyle=1,$
        ytitle='z',xtitle='y', /noerase
	xyouts,charsize=1.0,xpos,ypos,'time = '+string(ttime(i),'(f6.2)') 
;	xyouts,charsize=1.0,xpos(1),ypos(7), ytitl


;      if ( (i eq 1) or (i eq 3) ) then begin
        !P.POSITION=[xsb(i),yl(i),xeb(i),yup(i)]
	fmax=bmax & fmin=bmin
	ddel=(fmax-fmin) & del=ddel/float(nxf-1)
	ctab=findgen(nxf) & ctab=del*ctab+fmin & cbary=findgen(2,nxf)
	cbary(0,*)=ctab(*) & cbary(1,*)=ctab(*)
        IMAGE_C, cbary
	contour,cbary,[0,1],ctab,levels=findgen(nl1)*ddel/(nl1-1.)+fmin,$
	xstyle=1,ystyle=1,c_linestyle=1,xrange=[0,1],yrange=[fmin,fmax],$
        xtickname=names,xticks=1,ytickformat='(f5.2)',/noerase
;      endif

endfor

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'

   endwhile

end

