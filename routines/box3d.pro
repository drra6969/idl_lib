  pro box3d,xmin,xmax,ymin,ymax,zmin,zmax,xang,yang,zang,persp

  box_1x=[xmin,xmax,xmin,xmax,xmin,xmax,xmin,xmax]
  box_1y=[ymin,ymin,ymax,ymax,ymin,ymin,ymax,ymax]
  box_1z=[zmin,zmin,zmin,zmin,zmax,zmax,zmax,zmax]
  box_2x=[xmin,xmin,xmin,xmin,xmax,xmax,xmax,xmax]
  box_2y=[ymin,ymax,ymin,ymax,ymin,ymax,ymin,ymax]
  box_2z=[zmin,zmin,zmax,zmax,zmin,zmin,zmax,zmax]
  box_3x=[xmin,xmin,xmax,xmax,xmin,xmin,xmax,xmax]
  box_3y=[ymin,ymin,ymin,ymin,ymax,ymax,ymax,ymax]
  box_3z=[zmin,zmax,zmin,zmax,zmin,zmax,zmin,zmax]
;
;           ------------------------------     standart
;         /!              3,3            /!    orientation
;        / !                            / !
;       1,1!                           /1,3 
;      /   !                          /   !
;      ------------------------------     !
;     !    !         3,2             !    !
;     !    !                         !    !
;     !    !2,2                      !    !2,3
;     !    !                         !    !
;     !    !                         !    !
;     !    !                         !    !
;     !    !                         !    !
;     !2,0 !                         !2,1 !
;    y!     -------------------------!----
;     !   /              3,1         !   /
;     ! x/1,0                        !  /1,2
;     ! /                            ! /
;     !/                             !/
;      ------------------------------
;           z        3,0
;
; 1. index - direction of axis, 2. index - sequence of plotting
;
  lx=(xmax-xmin) & ly=(ymax-ymin) & lz=(zmax-zmin)
  xh=xmin+0.5*lx & yh=ymin+0.5*ly & zh=zmin+0.5*lz
  abc=[lx,ly,lz]
  scamax=max(abc)
  print,scamax
  if min(abc) le 0 then $
    print,'wrong boundaries!! lx=',lx,'  ly=',ly,'  lz=', lz
  scax=lx & scay=ly & scaz=lz
  ratx=float(lx)/scamax & raty=float(ly)/scamax & ratz=float(lz)/scamax
  if min(abc) le 0.5*scamax then begin 
    scax=scamax*sqrt(ratx)
    scay=scamax*sqrt(raty)
    scaz=scamax*sqrt(ratz)
  endif
  if min(abc) le 0.15*scamax then begin 
    scax=scamax*(ratx)^0.7
    scay=scamax*(raty)^0.7
    scaz=scamax*(ratz)^0.7
  endif
  scx=1.6*scax & scy=1.6*scay & scz=1.6*scaz
  print,scax,scay,scaz
 t3d,/reset
  !x.s=[-(xmin-0.5*(scx-lx)),1.0]/scx
  !y.s=[-(ymin-0.5*(scy-ly)),1.0]/scy
  !z.s=[-(zmin-0.5*(scz-lz)),1.0]/scz
  t3d,translate=[-0.5,-.5,-.5]
  charsize=3
  erase
 t3d,rotate=[-90,180+yang,0]
 t3d,rotate=[xang,0,0],perspective=persp
 t3d,translate=[0.5,.5,.5]
  lst1=intarr(4) 
  for i=1,3 do lst1(i)=0 & lst2=lst1 & lst3=lst1
; here is the box:
  for i=0,3 do begin
    if yang lt 90 then begin
      lst3(3)=1 & lst2(3)=1 & lst1(3)=1 & endif 
    if yang gt 90 then begin
      lst3(1)=1 & lst2(3)=1 & lst1(2)=1 & endif 
    if lst1(i) ne 1 then  plots, box_1x((2*i):(2*i+1)), $
                                 box_1y((2*i):(2*i+1)), $
                                 box_1z((2*i):(2*i+1)),/t3d,/data
    if lst2(i) ne 1 then  plots, box_2x((2*i):(2*i+1)), $ 
                                 box_2y((2*i):(2*i+1)), $
                                 box_2z((2*i):(2*i+1)),/t3d,/data
    if lst3(i) ne 1 then  plots, box_3x((2*i):(2*i+1)), $
                                 box_3y((2*i):(2*i+1)), $
                                 box_3z((2*i):(2*i+1)),/t3d,/data
  endfor

;  some lines for eye support
  ey1x=[xh,xh,xh,xh,xh]
  ey1y=[ymin,ymax,ymax,ymin,ymin]
  ey1z=[zmin,zmin,zmax,zmax,zmin]
  ey2x=[xmin,xmin,xmax,xmax,xmin]
  ey2y=[yh,yh,yh,yh,yh]
  ey2z=[zmin,zmax,zmax,zmin,zmin]
  ey3x=[xmin,xmax,xmax,xmin,xmin]
  ey3y=[ymin,ymin,ymax,ymax,ymin]
  ey3z=[zh,zh,zh,zh,zh]
     plots, ey1x,ey1y,ey1z,line=1,/t3d,/data
;     plots, ey2x,ey2y,ey2z,line=1,/t3d,/data
     plots, ey3x,ey3y,ey3z,line=1,/t3d,/data
;  complete box in dotted
  ey4x=[xmax,xmax]
  ey4y=[ymin,ymax]
  ey4z=[zmax,zmax]
  if yang le 90 then begin
   ey5x=[xmax,xmax,xmin]
   ey5y=[ymax,ymax,ymax]
   ey5z=[zmin,zmax,zmax]
  endif
  if yang gt 90 then begin
   ey5x=[xmax,xmax,xmin]
   ey5y=[ymin,ymin,ymin]
   ey5z=[zmin,zmax,zmax]
  endif
  plots, ey4x,ey4y,ey4z,line=1,/t3d,/data
  plots, ey5x,ey5y,ey5z,line=1,/t3d,/data


 ;  some ticks for better 3D feeling
  lti=0.025*scy
  ti1x=[xmax,xmax,xmax,xmax,xmax,xmax,xh,xh,xmin,xmin]
  ti1y=[ymin,ymin-lti,ymin,ymin-lti,ymin,ymin-lti,ymin,ymin-lti,ymin,ymin-lti]
  ti1z=[zmin,zmin,zh,zh,zmax,zmax,zmax,zmax,zmax,zmax] 
   if yang ge 90 then $
   ti1y=[ymax,ymax+lti,ymax,ymax+lti,ymax,ymax+lti,ymax,ymax+lti,ymax,ymax+lti]
  lti=0.025*scz
  ti2x=[xmin,xmin,xh,xh,xmax,xmax,xmax,xmax,xmax,xmax]
  ti2y=[ymax,ymax,ymax,ymax,ymax,ymax,yh,yh,ymin,ymin]
  ti2z=[zmin-lti,zmin,zmin-lti,zmin,zmin-lti,zmin,zmin-lti,zmin,zmin-lti,zmin] 
   if yang ge 90 then ti2y=[ymin,ymin,ymin,ymin,ymin,ymin,yh,yh,ymax,ymax]
  lti=0.025*scx
  ti3x=[xmin,xmin-lti,xmin,xmin-lti,xmin,xmin-lti,xmin,xmin-lti,xmin,xmin-lti]
  ti3y=[ymin,ymin,yh,yh,ymax,ymax,ymax,ymax,ymax,ymax]
  ti3z=[zmax,zmax,zmax,zmax,zmax,zmax,zh,zh,zmin,zmin]
  if yang ge 90 then ti3y=[ymin,ymin,yh,yh,ymax,ymax,ymin,ymin,ymin,ymin]
  for i=0,4 do begin
     plots, ti1x((2*i):(2*i+1)),ti1y((2*i):(2*i+1)), $
            ti1z((2*i):(2*i+1)),/t3d,/data
     plots, ti2x((2*i):(2*i+1)),ti2y((2*i):(2*i+1)), $
            ti2z((2*i):(2*i+1)),/t3d,/data
     plots, ti3x((2*i):(2*i+1)),ti3y((2*i):(2*i+1)), $
            ti3z((2*i):(2*i+1)),/t3d,/data
  endfor   

;  and now some numbers
  if abs(xmin) lt 4.5 then oxmin=string(xmin,'(f4.1)') $
       else oxmin=string(xmin,'(i3)')
  if abs(xmax) lt 4.5 then oxmax=string(xmax,'(f4.1)') $
       else oxmax=string(xmax,'(i3)')
  if abs(ymin) lt 4.5 then oymin=string(ymin,'(f4.1)') $
       else oymin=string(ymin,'(i4)')
  if abs(ymax) lt 4.5 then oymax=string(ymax,'(f4.1)') $
       else oymax=string(ymax,'(i4)')
  if abs(zmin) lt 4.5 then ozmin=string(zmin,'(f4.1)') $
       else ozmin=string(zmin,'(i4)')
  if abs(zmax) lt 4.5 then ozmax=string(zmax,'(f4.1)') $
       else ozmax=string(zmax,'(i4)')


  if yang lt 30 then begin
    xyouts,xh+.03*scx,ymax+.05*scy,z=zmin-.05*scz, 'X',/t3d,/data, $
           text_axes=1,size=2
    xyouts,xmin+.0*scx,ymax+.05*scy,z=zmin-.05*scz, oxmin,/t3d,/data, $
           text_axes=1,size=2
    xyouts,xmax-.05*scx,ymax+.05*scy,z=zmin-.05*scz, oxmax,/t3d,/data, $
           text_axes=1,size=2
    xyouts,xmax,yh+.03*scy,z=zmin-.05*scz, 'Y',/t3d,/data, $
           text_axes=1,size=2
    xyouts,xmax,ymin-.05*scy,z=zmin-.05*scz, oymin,/t3d,/data, $
           text_axes=1,size=2 
    xyouts,xmax,ymax-.05*scy,z=zmin-.05*scz, oymax,/t3d,/data, $
           text_axes=1,size=2 
    xyouts,xmax,ymin-.06*scy,z=zh+.05*scz, 'Z',/t3d,/data, $
         text_axes=1,size=2
    xyouts,xmax,ymin-.05*scy,z=zmin+.05*scz, ozmin,/t3d,/data, $
         text_axes=1,size=2
    xyouts,xmax,ymin-.05*scy,z=zmax-.05*scz, ozmax,/t3d,/data, $
         text_axes=1,size=2
  endif
  if ((yang le 90) and (yang ge 30)) then begin
    xyouts,xh+.03*scx,ymax+.05*scy,z=zmin-.05*scz, 'X',/t3d,/data, $
           text_axes=2,size=2
    xyouts,xmin+.0*scx,ymax+.015*scy,z=zmin-.05*scz, oxmin,/t3d,/data, $
           text_axes=2,size=2
    xyouts,xmax+.03*scx,ymax+.02*scy,z=zmin-.05*scz, oxmax,/t3d,/data, $
           text_axes=2,size=2
    xyouts,xmax,yh+.03*scy,z=zmin-.06*scz, 'Y',/t3d,/data, $
           text_axes=2,size=2 
    xyouts,xmax,ymin-.04*scy,z=zmin-.06*scz, oymin,/t3d,/data, $
           text_axes=2,size=2 
    xyouts,xmax,ymax-.07*scy,z=zmin-.06*scz, oymax,/t3d,/data, $
           text_axes=2,size=2 
    xyouts,xmax,ymin-.06*scy,z=zh+.05*scz, 'Z',/t3d,/data, $
         text_axes=2,size=2
    xyouts,xmax,ymin-.11*scy,z=zmin+.03*scz, ozmin,/t3d,/data, $
         text_axes=2,size=2
    xyouts,xmax,ymin-.1*scy,z=zmax-.05*scz, ozmax,/t3d,/data, $
         text_axes=2,size=2
  endif
  if ((yang lt 150) and (yang gt 90)) then begin
    xyouts,xh+.03*scx,ymin-.05*scy,z=zmin-.05*scz, 'X',/t3d,/data, $
           text_axes=2,size=2
    xyouts,xmin-.0*scx,ymin-.1*scy,z=zmin-.05*scz, oxmin,/t3d,/data, $
           text_axes=2,size=2
    xyouts,xmax+.0*scx,ymin-.1*scy,z=zmin-.05*scz, oxmax,/t3d,/data, $
           text_axes=2,size=2
    xyouts,xmax,yh+.05*scy,z=zmin-.06*scz, 'Y',/t3d,/data, $
           text_axes=2,size=2 
    xyouts,xmax,ymin-.03*scy,z=zmin-.06*scz, oymin,/t3d,/data, $
           text_axes=2,size=2 
    xyouts,xmax,ymax-.06*scy,z=zmin-.06*scz, oymax,/t3d,/data, $
           text_axes=2,size=2 
    xyouts,xmin,ymin-.06*scy,z=zh+.05*scz, 'Z',/t3d,/data, $
         text_axes=2,size=2
    xyouts,xmin,ymin-.1*scy,z=zmin+.03*scz, ozmin,/t3d,/data, $
         text_axes=2,size=2
    xyouts,xmin,ymin-.1*scy,z=zmax-.05*scz, ozmax,/t3d,/data, $
         text_axes=2,size=2
  endif
  if yang ge 150 then begin
    xyouts,xh+.05*scx,ymin-.05*scy,z=zmin-.05*scz, 'X',/t3d,/data, $
           text_axes=1,size=2
    xyouts,xmin-.02*scx,ymin-.05*scy,z=zmin-.05*scz, oxmin,/t3d,/data, $
           text_axes=1,size=2
    xyouts,xmax-.05*scx,ymin-.05*scy,z=zmin-.05*scz, oxmax,/t3d,/data, $
           text_axes=1,size=2
    xyouts,xmax,yh+.05*scy,z=zmin-.06*scz, 'Y',/t3d,/data, $
           text_axes=1,size=2 
    xyouts,xmax,ymin+.03*scy,z=zmin-.06*scz, oymin,/t3d,/data, $
           text_axes=1,size=2 
    xyouts,xmax,ymax-.06*scy,z=zmin-.06*scz, oymax,/t3d,/data, $
           text_axes=1,size=2 
    xyouts,xmin-.04*scx,ymin-.0*scy,z=zh+.05*scz, 'Z',/t3d,/data, $
         text_axes=1,size=2
    xyouts,xmin-.1*scx,ymin-.0*scy,z=zmin+.02*scz, ozmin,/t3d,/data, $
         text_axes=1,size=2
    xyouts,xmin-.1*scx,ymin-.0*scy,z=zmax-.03*scz, ozmax,/t3d,/data, $
         text_axes=1,size=2
  endif

end

