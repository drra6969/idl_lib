
; MAIN PROGRAM
   
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



     while again eq 'y' do begin

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
    !P.MULTI=[0,0,1]
    !P.POSITION=[0.2,0.6,0.8,0.9]
    amax=max(bb)
    amin=0.3
    plot, xx, bb(*,0), line=0,$
        title='Flux of By',$
        font=3, yrange=[amin,amax],xrange=[0,100],xtitle='z'
    for in=1, itot,4 do oplot, xx, bb(*,in), line=0

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'

   endwhile



end

