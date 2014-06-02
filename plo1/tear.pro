; START OF PROGRAM

  nx=101 & nf=4

  k=fltarr(nx,/NOZERO) & q=fltarr(nf,nx,/NOZERO) & kmax=1.0
  pi=3.141593
  for i=0,nx-1 do k(i)=i*kmax/(nx-1)
;  print,'k:',k

    again='y' & withps='n' & contin='y'
    while again eq 'y' do begin

       !P.CHARSIZE=2.0
       !P.FONT=3
      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='tear.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif


  res=0.02  
  l=5.0 
  for j=0,nf-1 do begin
   q(j,0)=0.0
   for i=1,nx-1 do begin
    q(j,i)=0.5*k(i)^(2.0/3.0)*res^(1.0/3.0)
    delq=0.5*q(j,i)
    qold=0.0
    while (delq ge 0.01*res) and (q(j,i) ge 0.1*res) do begin
      qold=q(j,i)
      lam=q(j,i)^(1.5)/k(i)/res^(0.5)
      case j of
       0: fun=(1-k(i)^2)
       1: fun=k(i)^2*( exp(-k(i))*cosh(k(i)) - k(i) )$
                  /( k(i) - exp(-k(i))*sinh(k(i)) )
       2: fun=k(i)^2*( cosh(k(i)*l)*cosh(k(i)) - k(i)*sinh(k(i)*(l+1.0)) )$
                  /( -cosh(k(i)*l)*sinh(k(i)) + k(i)*cosh(k(i)*(l+1.0)) )
       3: fun=k(i)^2*( -sinh(k(i)*l)*cosh(k(i)) + k(i)*cosh(k(i)*(l+1.0)) )$
                  /( sinh(k(i)*l)*sinh(k(i)) - k(i)*sinh(k(i)*(l+1.0)) )
      endcase
      fleft=q(j,i)^2/res
      fright=lam^(0.5)/pi*(1-lam^2)*fun*Gamma( 0.25*(lam+1.0))$
                                       /Gamma( 0.25*(lam+3.0))
      if ( fleft lt fright ) then q(j,i)=q(j,i)+delq
      if ( fleft gt fright ) then q(j,i)=q(j,i)-delq
      delq=0.5*delq
    endwhile
;    print,'delq(',i,')=',delq,'   q(',i,')=',q(j,i)
   endfor
  endfor

; first page
  !P.REGION=[0.,0.,1.0,1.25]

  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin
    !P.CHARSIZE=1  
    !P.MULTI=[0,0,2]
    !P.POSITION=[0.15,0.1,0.75,0.4]
    amax = max(q)
    amin = min(q)
    plot, k, q(0,0:nx-1),$
        title='tearing growth rate',$
        xtitle='k', font=3, yrange=[min(amin), max(amax)]
    oplot, k, q(1,0:nx-1), line=1
    oplot, k, q(2,0:nx-1), line=2
    oplot, k, q(3,0:nx-1), line=3
    xpos=1.02*kmax
    ypos=amin+0.8*(amax-amin)
    ypos1=amin+0.65*(amax-amin)
	xyouts,xpos,ypos,'resistivity:'
	xyouts,xpos,ypos1,' '+string(res,'(f8.5)')


  res=0.01*res  
  l=l 

  for j=0,nf-1 do begin
  q(0)=0.0
  for i=1,nx-1 do begin
    q(j,i)=0.5*k(i)^(2.0/3.0)*res^(1.0/3.0)
    delq=0.5*q(j,i)
    qold=0.0
    while (delq ge 0.01*res) and (q(j,i) ge 0.1*res) do begin
      qold=q(j,i)
      lam=q(j,i)^(1.5)/k(i)/res^(0.5)
      case j of
       0: fun=(1-k(i)^2)
       1: fun=k(i)^2*( exp(-k(i))*cosh(k(i)) - k(i) )$
                  /( k(i) - exp(-k(i))*sinh(k(i)) )
       2: fun=k(i)^2*( cosh(k(i)*l)*cosh(k(i)) - k(i)*sinh(k(i)*(l+1.0)) )$
                  /( -cosh(k(i)*l)*sinh(k(i)) + k(i)*cosh(k(i)*(l+1.0)) )
       3: fun=k(i)^2*( -sinh(k(i)*l)*cosh(k(i)) + k(i)*cosh(k(i)*(l+1.0)) )$
                  /( sinh(k(i)*l)*sinh(k(i)) - k(i)*sinh(k(i)*(l+1.0)) )
      endcase
      fleft=q(j,i)^2/res
      fright=lam^(0.5)/pi*(1-lam^2)*fun*Gamma( 0.25*(lam+1.0))$
                                       /Gamma( 0.25*(lam+3.0))
      if ( fleft lt fright ) then q(j,i)=q(j,i)+delq
      if ( fleft gt fright ) then q(j,i)=q(j,i)-delq
      delq=0.5*delq
    endwhile
  endfor
  endfor

    !P.POSITION=[0.15,0.55,0.75,0.85]
    amax = max(q)
    amin = min(q)
    plot, k, q(0,0:nx-1),$
        title='tearing growth rate',$
        xtitle='k', font=3, yrange=[min(amin), max(amax)]
    oplot, k, q(1,0:nx-1), line=1
    oplot, k, q(2,0:nx-1), line=2
    oplot, k, q(3,0:nx-1), line=3
    xpos=1.02*kmax
    ypos=amin+0.8*(amax-amin)
    ypos1=amin+0.65*(amax-amin)
	xyouts,xpos,ypos,'resistivity:'
	xyouts,xpos,ypos1,' '+string(res,'(f8.5)')

   endif

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'

   endwhile

end

