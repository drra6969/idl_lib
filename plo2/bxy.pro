
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
   nxf = 201   &   nyf = 201
   fa=fltarr(nxf,nyf) & fb=fa
   
;----PARAMETER-------
  xmin = -50. & ymin =  0.0
  xmax =  50. & ymax = 100.0
;--------------------
   time=0.0 & fnumber=1
   name='' & contin='' & again='y' & withps='n' & closeps='n'
   run='' & plot='0'  & coltab='o' &  withgif = 'n' &  contonly = 'n'
   glatt='n' & colps ='n' & grayplot ='n' 
   names=strarr(15) & names=replicate(' ',15)
   nl1=17  &  nl2=9  &  phi=0.0  &  pi = 3.14159265536  &  phir = phi*pi/180.0
   xtit='x' & ytit='z' & smo='n'   & nsh=0  & nshtot=nsh  &  galpar='a'
   nxx=long(103) & nx=long(103) & ny=long(103) & orient='l'
   wlong=900 & wshort=720 & wfact=1.0 &  wxf=1.0 & wyf=1.0 & psfact=0.9
   wsize=[700,900] & wsold=[0,0] & wino=0 & srat0=1.1
   input='y'

; READ INPUT DATA OF DIMENSION NX, NY
   openr, 7, 'mincoor',/F77_UNFORMATTED
   readu, 7,  nxx,nx,ny
   print, 'dimension nxx=',nxx,'dimension nx=',nx,'     ny=',ny

   xx=fltarr(nxx,/NOZERO) & gxx=xx
   x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
   g1=fltarr(nx,/NOZERO) 
   h1=fltarr(ny,/NOZERO) 
   bb=fltarr(nx,ny,/NOZERO)

   readu, 7,  xx,gxx, x,g1, y,h1
   close, 7

   testbd1, nx,ny,x,y,xmin,xmax,ymin,ymax   
   xpmin=xmin  &  xpmax=xmax
   grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf

   xs=0.2    & xe=0.7  & xsb=0.8  & xeb=0.9
   yl1=0.2   & yup1=.8
;  yl2=0.35  & yup2=.55
;  y13=0.1  & yup3=.3
  names=strarr(15)
  names=replicate(' ',15)

   fnumber=4
;   print, 'Input filenumber'
;   read, fnumber

   for fnumber=0,400,2 do begin

   name='magdby'+string(fnumber,'(i3.3)')
   openr, 8, name,/F77_UNFORMATTED
   readu, 8, nx,ny,time
   readu, 8, bb
   close, 8
   bb=bb/7./4.


   nameout='midlby'+string(fnumber,'(i3.3)')
   save,file=nameout,/xdr,nx,ny,time,x,y,bb

  endfor







;   !P.REGION=[0.,0.,1.0,1.25]
  !P.REGION=[0.,0.,1.0,1.0]
  !P.MULTI=[0,5,0,0,0]
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

   setcol, 'y'

   !P.CHARSIZE=2.0
   !P.MULTI=[0,3,0,0,0]
        
    fa=interpolate(bb,ioxf,ioyf,/grid) 
    fa=smooth(fa,3)
;    contsca2, fc,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo

	fmax=max(fa) & fmin=min(fa)
        if ( (fmax-fmin) lt 0.000001) then begin
          fmax=fmax+0.05 & fmin=fmin-0.05 & endif

        !P.POSITION=[xs,yl1,xe,yup1]
        IMAGE_C, fa
	bmax=max(fa) & bmin=min(fa) &  bav=0.5*(bmax+bmin)
        if ( (bmax-bmin) lt 0.000001) then begin
          bmax=bmax+0.0000005 & bmin=bmin-0.0000005 & endif
        del=(bmax-bmin)/(nl2-1.)
        cb=0.
	contour,fa,xf,yf,levels=findgen(nl2)*del+bmin,$
;        c_linestyle=findgen(nl2)*del+bmin lt cb,$
        c_linestyle=1,$
        title='By Flux',xstyle=1,ystyle=1,$
        ytitle='z',xtitle='y', /noerase
;	xyouts,charsize=1.1,xpos(0),ypos(8),'time = '+string(time,'(f6.2)') 
;	xyouts,charsize=1.0,xpos(1),ypos(7), ytitl

        !P.POSITION=[xsb,yl1,xeb,yup1]
	fmax=bmax & fmin=bmin
	ddel=(fmax-fmin) & del=ddel/float(nxf-1)
	ctab=findgen(nxf) & ctab=del*ctab+fmin & cbary=findgen(2,nxf)
	cbary(0,*)=ctab(*) & cbary(1,*)=ctab(*)
        IMAGE_C, cbary
	contour,cbary,[0,1],ctab,levels=findgen(nl1)*ddel/(nl1-1.)+fmin,$
	xstyle=1,ystyle=1,c_linestyle=1,xrange=[0,1],yrange=[fmin,fmax],$
        xtickname=names,xticks=1,ytickformat='(f5.2)',/noerase


end

