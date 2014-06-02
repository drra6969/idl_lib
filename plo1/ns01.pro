; START OF PROGRAM

  n=1000000.0 & tc=0.0001 & ts=0.0001 & nr=1601 
;-------------
; PARAMETER:
;-------------
  npmax=1000.0 & dnp=(npmax-1)/(nr-1) & np=1.0+findgen(nr)*dnp 

  f=1.0 
  alp=2./3. 
  tctot1=n^(2./3.)/np^alp*tc & tstot1=n/f/np*ts
  tot1=tctot1+tstot1 & ttot1=tot1*np/n/ts
  trat1=tctot1/tstot1

  alp=0.5 
  tctot2=n^(2./3.)/np^alp*tc & tstot2=n/f/np*ts
  tot2=tctot2+tstot2 & ttot2=tot2*np/n/ts
  trat2=tctot2/tstot2
  
  alp=0.0 
  tctot3=n^(2./3.)/np^alp*tc & tstot3=n/f/np*ts
  tot3=tctot3+tstot3 & ttot3=tot3*np/n/ts
  trat3=tctot3/tstot3
  

  alp=2./3. 
  f=np
  tt1=where(np^(1.-alp)/n^(1./3.) lt 0.999,dim1)
  f(tt1)=1.0 - np(tt1)^(1.-alp)/n^(1./3.)
  tctot1(tt1)=n^(2./3.)/np(tt1)^alp*tc & tstot1(tt1)=n/f(tt1)/np(tt1)*ts
  tot1(tt1)=tctot1(tt1)+tstot1(tt1) & ttot1(tt1)=tot1(tt1)*np(tt1)/n/ts

  alp=0.5 
  tt2=where(np^(1.-alp)/n^(1./3.) lt 0.999,dim2)
  f(tt2)=1.0 - (np(tt2)^(1.-alp))/n^(1./3.)
  tctot2(tt2)=n^(2./3.)/np(tt2)^alp*tc & tstot2(tt2)=n/f(tt2)/np(tt2)*ts
  tot2(tt2)=tctot2(tt2)+tstot2(tt2) & ttot2(tt2)=tot2(tt2)*np(tt2)/n/ts
  
  alp=0.0 
  tt3=where(np^(1.-alp)/n^(1./3.) lt 0.999,dim3)
  f(tt3)=1.0 - (np(tt3)^(1.-alp))/n^(1./3.)
  print, f(tt3)
  tctot3(tt3)=n^(2./3.)/np(tt3)^alp*tc & tstot3(tt3)=n/f(tt3)/np(tt3)*ts
  tot3(tt3)=tctot3(tt3)+tstot3(tt3) & ttot3(tt3)=tot3(tt3)*np(tt3)/n/ts
  

    again='y' & withps='n' & contin='y'
    while again eq 'y' do begin

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='arc.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif
; first page
  !P.REGION=[0.,0.,1.0,1.25]


  print, 'plot page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin
    !P.CHARSIZE=1  
    !P.MULTI=[0,0,9]
    !P.POSITION=[0.2,0.65,0.7,0.9]
    amax = (max([trat1,trat2,trat3]))
    amin = (min([trat1,trat2,trat3]))
    plot_io, np, trat1,$
        title='Time ratio, Tc/Ts',xtitle='Np',$
        font=3, yrange=[amin, amax], ystyle=1, line=0
    plot_io, np, trat2, font=3, yrange=[amin, amax], ystyle=1, line=1
    plot_io, np, trat3, font=3, yrange=[amin, amax], ystyle=1, line=2
    !P.POSITION=[0.2,0.35,0.7,0.6]
;    amax = (max([tot1(tt1),tot2(tt2),tot3(tt3)]))
    amax = 80
    amin = (min([tot1(tt1),tot2(tt2),tot3(tt3)]))
    plot_io, np, tot1(tt1),$
        title='Total Time Ttot',xtitle='Np',$
        font=3, yrange=[amin, amax], ystyle=1, line=0
    plot_io, np, tot2(tt2), font=3, yrange=[amin, amax], ystyle=1, line=1
    plot_io, np, tot3(tt3), font=3, yrange=[amin, amax], ystyle=1, line=2
    !P.POSITION=[0.2,0.05,0.7,0.3]
;    amax = (max([ttot1(tt1),ttot2(tt2),ttot3(tt3)]))
    amax = 20
    amin = (min([ttot1(tt1),ttot2(tt2),ttot3(tt3)]))
    plot_io, np, ttot1(tt1),$
        title='Time ratio, Ttot/Ttheory',xtitle='Np',$
        font=3, yrange=[amin, amax], ystyle=1, line=0
    plot_io, np, ttot2(tt2), font=3, yrange=[amin, amax], ystyle=1, line=1
    plot_io, np, ttot3(tt3), font=3, yrange=[amin, amax], ystyle=1, line=2

  endif


 
      print, 'again?'
      read, again



  if withps eq 'y' then device,/close
  set_plot,'x'

  endwhile

end


