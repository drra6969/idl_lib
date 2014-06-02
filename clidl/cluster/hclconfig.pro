PRO hclconfig

COMMON sc, naux,time,sccoor,scvel,scdr1,scdr2,scdr3,scdr4,gsegsm,diptild, $
           re, rsc,dxsc,dysc,dzsc,volsc,volrat

; r in re
  sizeaux=size(sccoor) & nn=sizeaux(2)
  rsc = sccoor/re
; max separation in x, y, and z
  dxsc=fltarr(nn) & dysc=dxsc & dzsc=dxsc 
  ddsc=fltarr(6,nn) & distsc=dxsc & volsc=dxsc & vol0=dxsc
  for j=0,nn-1 do begin
    dxsc(j) = max([scdr1(0,j),scdr2(0,j),scdr3(0,j),scdr4(0,j)])$
              - min([scdr1(0,j),scdr2(0,j),scdr3(0,j),scdr4(0,j)])
    dysc(j) = max([scdr1(1,j),scdr2(1,j),scdr3(1,j),scdr4(1,j)])$
              - min([scdr1(1,j),scdr2(1,j),scdr3(1,j),scdr4(1,j)])
    dzsc(j) = max([scdr1(2,j),scdr2(2,j),scdr3(2,j),scdr4(2,j)])$
              - min([scdr1(2,j),scdr2(2,j),scdr3(2,j),scdr4(2,j)])
  endfor

; deviation of ideal tetrahedron by volume
  dd12 = scdr1-scdr2 & dd13 = scdr1-scdr3 & dd14 = scdr1-scdr4 
  dd23 = scdr2-scdr3 & dd24 = scdr2-scdr4 & dd34 = scdr3-scdr4
  ddsc(0,*)=sqrt(dd12(0,*)^2+dd12(1,*)^2+dd12(2,*)^2)
  ddsc(1,*)=sqrt(dd13(0,*)^2+dd13(1,*)^2+dd13(2,*)^2)
  ddsc(2,*)=sqrt(dd14(0,*)^2+dd14(1,*)^2+dd14(2,*)^2)
  ddsc(3,*)=sqrt(dd23(0,*)^2+dd23(1,*)^2+dd23(2,*)^2)
  ddsc(4,*)=sqrt(dd24(0,*)^2+dd24(1,*)^2+dd24(2,*)^2)
  ddsc(5,*)=sqrt(dd34(0,*)^2+dd34(1,*)^2+dd34(2,*)^2)
  distsc(*) = (ddsc(0,*)+ddsc(1,*)+ddsc(2,*)+ddsc(3,*)+ddsc(4,*)+ddsc(5,*))/6.
  volsc(*) =   dd12(0,*) * (dd13(1,*)*dd14(2,*) - dd13(2,*)*dd14(1,*))$
             + dd12(1,*) * (dd13(2,*)*dd14(0,*) - dd13(0,*)*dd14(2,*))$
             + dd12(2,*) * (dd13(0,*)*dd14(1,*) - dd13(1,*)*dd14(0,*))
  volsc(*) = abs(volsc(*))/6.
  vol0     = sqrt(2.)/12.*distsc^3
  volrat = volsc/vol0 

  return
end
