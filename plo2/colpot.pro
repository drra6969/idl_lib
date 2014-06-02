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

; GRID FOR CONTOUR AND SURFACE PLOTS
   nxf = 201   &   nyf = 201
   fa=fltarr(nxf,nyf) & fb=fa
   
;----PARAMETER-------
  xmin = -10. & ymin =  0.0
  xmax =  10. & ymax = 24.0
  xmin0 = xmin & ymin0 = ymin
  xmax0 = xmax & ymax0 = ymax
;--------------------
   time=0.0 & fnumber=1
   name='' & contin='' & again='y' & withps='n' & closeps='n'
   run='' & plot='0'  & coltab='o' &  withgif = 'n' &  contonly = 'n'
   glatt='n' & colps ='n' & grayplot ='n' 
   names=strarr(15) & names=replicate(' ',15)
   nl1=17  &  nl2=9  &  phi=0.0  &  pi = 3.14159265536  &  phir = phi*pi/180.0
   xtit='x' & ytit='y' & smo='n'   & nsh=0  & nshtot=nsh  &  galpar='a'
   nx=long(103) & ny=long(103) & orient='l' & aval=100.0
   wlong=900 & wshort=720 & wfact=1.2 &  wxf=1.0 & wyf=1.0 & psfact=0.9
   wsize=[700,900] & wsold=[0,0] & wino=0 & srat0=1.1

; READ INPUT DATA OF DIMENSION NX, NY
   read2dpot, g1,g2,g3,h1,h2,h3, bx,by,bz, vx,vy,vz, rho,u,res,fnumber
   
   bsq=bx & p=bx & jx=bx & jy=bx & jz=bx & ex=bx & ey=bx & ez=bx
   bzpro=bx & vzpro=bx & vypro=bx
   a=fltarr(nx,ny) 
   f1=bx & f2=bx & f3=bx & f4=bx  

;   print, 'Which case?'
;   read, run

   testbd, nx,ny,x,y,xmin,xmax,ymin,ymax   
;   xpmin=xmax  &  xpmax=xmin
   xpmin=xmin  &  xpmax=xmax
   print, 'after testbd',xpmin,xpmax

; GRID FOR CONTOUR/SURFACE PLOTS
  grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf

   print, 'X:', x(0),x(nx-1)
   print, 'Y:', y(0),y(ny-1)

;   !P.REGION=[0.,0.,1.0,1.25]
  !P.REGION=[0.,0.,1.0,1.0]
  !P.MULTI=[0,5,0,0,0]
  !P.CHARSIZE=1.5
  !P.FONT=3
  !P.THICK=1.
  !X.TICKS=3
  !Y.TICKS=4
  !Y.TICKlen=0.04
  !X.THICK=2
  !Y.THICK=2

menu:
        if withps eq 'y' then  begin 
           print, 'close postscript device? <y> or <n>'
           read, closeps
        endif
        if closeps eq 'y' then begin
            withps = 'n' & closeps='n'
            device,/close
            set_plot,'x'
            !P.THICK=1.
        endif
        if (withps eq 'y') and closeps ne 'y' then $
          print, 'postscript device is still open!'
  print, 'Last plot:',plot
  print, 'Input:'
  print, 'Options: <return> -> next plot, plasma velocity;'
  print, '         <s> -> smooth data'
  print, '         <t> -> switch off smoothing'
  print, '         <w> -> change window size'
  print, '         <x> -> new boundaries in x'
  print, '         <y> -> new boundaries in y'
  print, '         <z> -> original boundaries'
  print, '         <o> -> scale postscipt output'
  print, '         <p> -> postscript for present plot'
  print, '         <r> -> close postscript'
  print, '         <n> -> plot ID'
  print, '         <g> -> output on con*.gif'
  print, '         <k> -> contour plot only'
  print, '         <l> -> switch to color'
  print, '         <e> -> switch to color postscript'
  print, '         <f> -> switch to greyscale postscript (default)'
  print, '         <c> -> load colortable'
  print, '         <o> -> use original colortables'
  print, '         <q> -> terminate'
  read, contin
  print, 'contin:',contin
  if contin eq 'q' then stop
  if contin eq 's' then  smo = 'y'
  if contin eq 't' then  smo = 'n'
  if contin eq 'w' then begin
     print, '  input factor to re-scale window'
     read, wfact
  endif
  if contin eq 'x' then begin
     print, '  input xmin and xmax'
     read, xmin, xmax
     testbd, nx,ny,x,y,xmin,xmax,ymin,ymax   
     grid2d, x,y,xmin,xmax,ymin,ymax,nxn,nyn,xn,yn,iox,ioy,dxn,dyn
     grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf
     delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
     xpmin=xmin  &  xpmax=xmax
  endif
  if contin eq 'y' then begin
     print, '  input ymin and ymax'
     read, ymin, ymax
     testbd, nx,ny,x,y,xmin,xmax,ymin,ymax   
     grid2d, x,y,xmin,xmax,ymin,ymax,nxn,nyn,xn,yn,iox,ioy,dxn,dyn
     grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf
     delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
  endif
  if contin eq 'z' then begin
     xmin = xmin0 & ymin = ymin0
     xmax = xmax0 & ymax = ymax0
     testbd, nx,ny,x,y,xmin,xmax,ymin,ymax   
     grid2d, x,y,xmin,xmax,ymin,ymax,nxn,nyn,xn,yn,iox,ioy,dxn,dyn
     grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf
     delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
  endif

  if contin eq 'e' then colps='y'
  if contin eq 'f' then colps='n'
  if contin eq 'k' then contonly='y'
  if contin eq 'l' then contonly='n'
  if contin eq 'n' then begin
     print, 'input plot id to append to ps/gif files '
     read, run
  endif
  if contin eq 'p' then begin
      withps = 'y' 
      if contonly eq 'n' then begin 
        set_plot,'ps' & ncol=!D.TABLE_SIZE & print,!D.TABLE_SIZE
        !p.color=0
        if colps eq 'n' then $
          device,/color,bits_per_pixel=8,$
             filename='conp'+plot+string(time,'(i3.3)')+run+'g.ps'$
        else $
          device,/color,bits_per_pixel=8,$
             filename='conp'+plot+string(time,'(i3.3)')+run+'col.ps'
      endif
      if contonly eq 'y' then begin 
        set_plot,'ps' 
        device,bits_per_pixel=2,$
             filename='conp'+plot+string(time,'(i3.3)')+run+'c.ps'
      endif
      if srat gt srat0 then  begin
;        device,/encaps
        device,/landscape
        device,/inches,xsize=10.,scale_factor=1.0,xoffset= 0.3
        device,/inches,ysize=8.0,scale_factor=1.0,yoffset=10.25
      endif
      if srat le srat0 then  begin
        device,/encaps
        device,/portrait
        device,/inches,xsize=8.,scale_factor=1.0,xoffset= 0.2
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.75
      endif
      !P.THICK=2.
;        device,/times,/bold,font_index=3
  endif
  if contin eq 'r' then begin
     withps = 'n' & closeps='n'
     device,/close
     set_plot,'x'
     !P.THICK=1.
     goto, menu
  endif

  if contin eq 'c' then begin
     xloadct,file='/home/ao/idl_lib/colortab.priv', block=1
     coltab='c'
     goto, menu
  endif
  if contin eq 'o' then coltab='o'
  if contin eq 'g' then begin
    if (withps eq 'n') then begin
           tvlct,r,g,b,/get
           imag=tvrd()
           write_gif, 'conp'+plot+string(time,'(i3.3)')+run+'.gif',imag, r,g,b
    endif
    if (withps ne 'n') then print, 'switch off postscript output'
    goto, menu
  endif

  !X.RANGE=[xpmin,xpmax]
  !Y.RANGE=[ymin,ymax]

; COORDINATES FOR PLOTS
   delx=xmax-xmin & dely=ymax-ymin & srat=(ymax-ymin)/(xmax-xmin)
   if srat gt srat0 then orient='l' else orient='p'
   wxf=1. & wyf=1.  & xyrat=1.25
   if orient eq 'l' then begin
     ytit1=''
     xtit0=xtit
     psize=0.3 & sb0=2.4 & sb1=4.0
     if (srat lt sb0) then  begin
       wyf=(srat/sb0)^(0.7)
       dpx=psize
       if withps eq 'y' then  dpy=xyrat*srat*psize
       if withps eq 'n' then  dpy=xyrat*psize*sb0*(srat/sb0)^(0.3)
     endif
     if (srat ge sb0) and (srat lt sb1) then  begin
       wxf=(sb0/srat)^(0.7)
       dpy=xyrat*sb0*psize
       if withps eq 'y' then dpx=psize*sb0/srat
       if withps eq 'n' then dpx=psize*(sb0/srat)^0.3 
     endif
     if (srat ge sb1) then  begin
       wxf=(sb0/sb1)^(0.7)
       dpy=xyrat*sb0*psize
       if withps eq 'y' then dpx=psize*sb0/sb1
       if withps eq 'n' then dpx=psize*(sb0/sb1)^0.3 
     endif
     if withps eq 'y' then begin
       dpx=psfact*dpx & dpy=psfact*dpy
     endif 
     print, srat, dpx, dpy
     xinter=0.05 & cbthick=0.1*dpx & cbdist1=0.045 & cbdist2=0.055
     if withps eq 'n' then begin
      xinter=xinter/wxf & cbthick=cbthick/wxf 
      cbdist1=cbdist1/wxf & cbdist2=cbdist2/wxf
     endif
     xleft=0.5-0.5*xinter-dpx 
     xa1=xleft                & xe1=xleft+dpx  
     xa2=xe1+xinter           & xe2=xa2+dpx
     xcb1=xa1-cbdist1-cbthick & xcb2=xe2+cbdist2
     ylo=0.065                & yup=ylo+dpy
     pos1=[xa1,ylo,xe1,yup]  & pos2=[xcb1,ylo,xcb1+cbthick,yup]
     pos3=[xa2,ylo,xe2,yup]  & pos4=[xcb2,ylo,xcb2+cbthick,yup]

     wsize=[wlong,wshort]  &  wsize=fix(wfact*wsize)
     wsize(0)=wxf*wsize(0) & wsize(1)=wyf*wsize(1)
     if (wsize(0) ne wsold(0)) or (wsize(1) ne wsold(1))  then $
       window,wino,xsize=wsize(0),ysize=wsize(1),title=run 
     wsold = wsize
   endif
     
   if orient eq 'p' then begin
     ytit1=ytit
     xtit0=''
     psize=0.4 & sb0=0.65 & sb1=0.3
     if (srat gt sb0) then  begin
       wxf=(sb0/srat)^(0.7)
       dpy=psize
       if withps eq 'y' then  dpx=psize*xyrat/srat
       if withps eq 'n' then  dpx=psize*xyrat/sb0*(sb0/srat)^(0.3)
     endif
     if (srat gt sb1) and (srat le sb0) then  begin
       wyf=(srat/sb0)^(0.7)
       dpx=psize*xyrat/sb0
       if withps eq 'y' then  dpy=psize/sb0*srat
       if withps eq 'n' then  dpy=psize*(srat/sb0)^(0.3)
     endif
     if (srat le sb1) then  begin
       dpx=psize*xyrat/sb0
       wyf=(sb1/sb0)^(0.7)
       if withps eq 'y' then  dpy=psize/sb0*sb1
       if withps eq 'n' then  dpy=psize*(sb1/sb0)^(0.3)
     endif
     if withps eq 'y' then begin
       dpx=psfact*dpx & dpy=psfact*dpy
     endif 
     xleft=0.08 & cbdist1=0.07 & cbthick=0.03*dpx  & yinter=0.05 
     if withps eq 'n' then begin
      yinter=yinter/wyf & cbthick=cbthick/wxf 
      cbdist1=cbdist1/wxf 
     endif
     ylo=0.5-0.5*yinter-dpy
     print, srat, dpx, dpy
     xa1=xleft & xe1=xleft+dpx  & xcb1=xe1+cbdist1
     ylo1=ylo & yup1=ylo+dpy & ylo2=yup1+yinter & yup2=ylo2+dpy 
     pos1=[xa1,ylo2,xe1,yup2]  & pos2=[xcb1,ylo2,xcb1+cbthick,yup2]
     pos3=[xa1,ylo1,xe1,yup1]  & pos4=[xcb1,ylo1,xcb1+cbthick,yup1]

     wsize=[wshort,wlong]  &  wsize=fix(wfact*wsize)
     wsize(0)=wxf*wsize(0) & wsize(1)=wyf*wsize(1)
     if (wsize(0) ne wsold(0)) or (wsize(1) ne wsold(1))  then $
       window,wino,xsize=wsize(0),ysize=wsize(1),title=run 
     wsold = wsize
   endif
   
   xpos=findgen(4)
   xpos(3)=xpmax-0.005*delx/dpx
   xpos(0)=xpmax+0.006*delx/dpx
   xpos(1)=xpmin-0.03*delx/dpx
   xpos(2)=xpmax-0.12*delx
   ypos=findgen(10)
   ypos(0)=ymin+0.2*dely            ; location for 'time'
   ypos(1)=ypos(0)-0.025*dely/dpy   ; next line
   ypos(2)=ypos(0)+0.075*dely/dpy   ; location 'CASE'
   ypos(3)=ymin+.85*dely             ; location 'Max='
   ypos(4)=ypos(3)-0.025*dely/dpy   ; next line
   ypos(5)=ymin+.65*dely    ; location 'Min='
   ypos(6)=ypos(5)-0.025*dely/dpy   ; next line
   ypos(7)=ymin+.9*dely
   ypos(8)=ymax+.02*dely
   ypos(9)=ymin-.05*dely

   !P.CHARSIZE=2.0
   !P.MULTI=[0,4,0,0,0]
   grayplot=withps & if colps eq 'y' then grayplot='n'
   if coltab eq 'o' then $
     if withps ne 'y' then loadct,file='/home/ao/idl_lib/colortab.priv', 41 $
       else $
       if colps eq 'y' then loadct,file='/home/ao/idl_lib/colortab.priv', 45 $
         else loadct,file='/home/ao/idl_lib/colortab.priv', 51


 
    tit='Potential' & tit2='B_N'    
    if contonly eq 'y' then begin
     ctsca1, pot,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    end else begin
     scplot1, bx,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
   endelse
  goto, menu
	


end

