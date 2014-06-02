PRO grid2d, x,y,xmin,xmax,ymin,ymax,nxn,nyn,xn,yn,iox,ioy,dxn,dyn

;--- generation of new grid for velocity vectors---
  iox=fltarr(nxn) & ioy=fltarr(nyn) 
  xn=findgen(nxn) & yn=findgen(nyn) 
  dxn=(xmax-xmin)/float(nxn-1) &  xn=xn*dxn+xmin & xn(nxn-1)=xmax
  dyn=(ymax-ymin)/float(nyn-1) &  yn=yn*dyn+ymin & yn(nyn-1)=ymax
  in=-1 &  k=0
  repeat begin &    in=in+1
    while xn(in) gt x(k+1) do k=k+1
    iox(in) = float(k) + (xn(in)-x(k))/(x(k+1)-x(k)) 
  endrep until in eq nxn-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while yn(in) gt y(k+1) do k=k+1
    ioy(in) = float(k) + (yn(in)-y(k))/(y(k+1)-y(k))        
;    print,'yn(',in,')=',yn(in),'   y(',k,')=',y(k)
  endrep until in eq nyn-1
  
return
end

