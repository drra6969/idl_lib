; START OF MAIN PROGRAM
;
; program to develob basic color tables

   n=17
   red=findgen(n) & green=red & blue=red
   choice='i' & choice0=choice
   igrid=1 & ig0=igrid & igval=''
   idval='111' & redval=0.5 
   isave=0 & fout=''
   fname='colpar' & fin=''
   ctno='' & ctnum=40 & ctnom='' 

   r = findgen(256) & g=r & b=r
   id=indgen(n) & id=id*256/8-1 & id(0)=0 & id(8)=255

   red(0)=0. & red(1)=0.5 & red(2)=0.0 & red(3)=0.25 & red(4)=0.25 
   red(5)=0.75 & red(6)=.25 & red(7)=0.75 & red(8)=1.0

   red(9)=1. & red(10)=1. & red(11)= & red(12)= 
   red(13)=1.0 & red(14)=1.0 & red(15)=1.0 & red(16)=1.0


   green(0)=0.75 & green(1)=0. & green(2)=0. & green(3)=0.25 & green(4)=0.0  
   green(5)=0. & green(6)=0.25 & green(7)=0.75 & green(8)=1.0

   green(9)= & green(10)=0.25 & green(11)= & green(12)=
   green(13)=0.5 & green(14)=0.0 & green(15)=0.5 & green(16)=1.0


   blue(0)=0. & blue(1)=0.5 & blue(2)=1.0 & blue(3)=1.0 & blue(4)=0.25
   blue(5)=0.75 & blue(6)=1.0 & blue(7)=1.0 & blue(8)=1.0

   blue(9)=0.75 & blue(10)=0.25 & blue(11)= & blue(12)=
   blue(13)=0.5 & blue(14)=0.0 & blue(15)=0.0 & blue(16)=0.0

menu1:
  print, 'Values of colorgrid:'
  print, 'Index, id, red, green, blue'
  for it=0, 8 do print, it,id(it),red(it),green(it),blue(it)
  print, 'Input: choice=',choice
  print, '      <i> -> change id value'
  print, '      <r> -> change red value'
  print, '      <g> -> change green value'
  print, '      <b> -> change blue value'
  print, '      <f> -> read table from file'
  print, '      <w> -> write table parameters to file'
  print, '      <s> -> save table to colortable'
  print, '      <q> -> quit'
  read, choice
  if (choice ne '') and (choice ne 'i')      $ 
     and (choice ne 'r') and (choice ne 'g') $ 
     and (choice ne 'b') and (choice ne 'f') $ 
     and (choice ne 'w') $ 
     and (choice ne 's') and (choice ne 'q') then goto, menu1
  if choice eq '' then choice=choice0
  choice0=choice

  if choice eq 'q' then stop

  if choice eq 's' then begin
    rm=bytscl(r) & gm=bytscl(g) & bm=bytscl(b)
    ctno=ctno+1
    print, 'Input number of colortable, default: ', ctnum
    read, ctno & if (ctno ne '') then ctnum=fix(ctno)
    ctname='colortab'+string(ctno,'(i3.3)')
    print, 'Input name of colortable, default: ', ctname
    read, ctnom & if (ctnom ne '') then ctname=ctnom
    modifyct, file='/users/ao/idl_lib/colortab0', ctnum, ctname, rm,gm,bm
  endif

  if (choice eq 'w') then begin 
    isave=isave+1 & fsave=fname+string(isave,'(i2.2)')
    print, 'Default file name to save parameters: ',fsave,'  ok?'
    read, fout
    if (fout ne '') and (fout ne 'y') then fsave=fout
    openw, 8, fsave
    printf, 8, n
    printf, 8, id,red,green,blue
    close, 8
  endif

  if (choice eq 'f') then begin
    finname=fname+string(isave,'(i2.2)')
    print, 'Default INPUT file name: ',finname
    print, 'Enter INPUT file name or return:'
    read, fin & if (fin ne '') then finname=fin
    openr, 2, finname
      readf, 2, n
      id=indgen(n) & red=findgen(n) & green=red & blue=red
      readf, 2, id,red,green,blue
    close, 2
  endif
  
menu2:
  if (choice eq 'i') or (choice eq 'r') $
     or (choice eq 'g') or  (choice eq 'b') then begin
  print, 'Input - i = Index color grid(>=0 and <=',n-1,')'
  print, 'Present Choice: ', igrid
  read, igval   
  if (igval eq '') then  igrid=ig0  else  igrid = fix(igval)
  if (igrid lt 1) or (igrid gt 9) then goto, menu2
  ig0 = igrid

  menui:
    if (choice eq 'i') then begin 
      print, 'Present color GRID value: ', id(igrid)
      print, 'Input - color grid value > ',id(igrid-1),' and <',id(igrid+1),')'
      read, idval
      if (idval ne '') then  id(igrid)=float(idval)
    endif
  menur:
    if (choice eq 'r') then begin 
      print, 'Present RED color value: ', red(igrid)
      print, 'Input - RED color value > ',0,' and <',1,')'
      read, redval
      if (redval ne '') then  red(igrid)=float(redval)
    endif
  menug:
    if (choice eq 'g') then begin 
      print, 'Present GREEN color value: ', green(igrid)
      print, 'Input - GREEN color value > ',0,' and <',1,')'
      read, greenval
      if (greenval ne '') then  green(igrid)=float(greenval)
    endif
  menub:
    if (choice eq 'b') then begin 
      print, 'Present BLUE color value: ', blue(igrid)
      print, 'Input - BLUE color value > ',0,' and <',1,')'
      read, blueval
      if (blueval ne '') then  blue(igrid)=float(blueval)
    endif

  endif

  setc, n, id, red, green, blue, r, g, b

;  print,'red:', r
;  print,'green:', g
;  print,'blue:', b





;do interpolation here! r,g,b -> ri,gi,bi
   nmax = !D.TABLE_SIZE
   print, 'table size: ',!D.TABLE_SIZE
   print, 'number of colors: ',!D.N_COLORS
   print, 'default color: ',!P.COLOR

   ri = findgen(nmax) & del=255./float(nmax-1) & ri=ri*del
   ri(nmax-1)=255.  &                         gi=ri & bi=ri
   ri = interpolate(r,ri)
   gi = interpolate(g,gi)
   bi = interpolate(b,bi)
;   print,ri

   r0=bytscl(ri)
   b0=bytscl(bi)
   g0=bytscl(gi)
   tvlct, r0,g0,b0

   ctab=findgen(129) & del=2./128 & ctab=del*ctab-1.0 
   cbary=fltarr(3,129) & for i=0,2 do cbary(i,*)=ctab

   !P.REGION=[0.,0.,1.0,1.0]
   !P.MULTI=[2,0,0,0,0]
   !P.CHARSIZE=1.7
   !P.FONT=3
   !X.TICKS=4
   !Y.TICKS=8
   !X.THICK=2
   !Y.THICK=2
   !P.POSITION=[0.1,0.1,0.5,0.9]

   IMAGE_C, cbary
   contour,cbary,levels=findgen(9)*0.25-1.0,$
	xstyle=1,ystyle=1,c_linestyle=1,xrange=[0,1],yrange=[-1,1],/noerase


  goto, menu1


end
