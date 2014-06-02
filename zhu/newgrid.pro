PRO newgrid, x,y,z,xmin,xmax,ymin,ymax,zmin,zmax, $
             nxn,nyn,nzn,xn,yn,zn,iox,ioy,ioz,dxn,dyn,dzn

;--- generation of new grid for velocity vectors---
  iox=fltarr(nxn) & ioy=fltarr(nyn) & ioz=fltarr(nzn)
  xn=findgen(nxn) & yn=findgen(nyn) & zn=findgen(nzn)
  dxn=(xmax-xmin)/float(nxn-1) &  xn=xn*dxn+xmin
  dyn=(ymax-ymin)/float(nyn-1) &  yn=yn*dyn+ymin
  dzn=(zmax-zmin)/float(nzn-1) &  zn=zn*dzn+zmin
  in=-1 &  k=0
  repeat begin &    in=in+1
    while xn(in) ge x(k+1) do k=k+1
    iox(in) = float(k) + (xn(in)-x(k))/(x(k+1)-x(k)) 
  endrep until in eq nxn-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while yn(in) ge y(k+1) do k=k+1
    ioy(in) = float(k) + (yn(in)-y(k))/(y(k+1)-y(k))        
  endrep until in eq nyn-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while zn(in) ge z(k+1) do k=k+1
    ioz(in) = float(k) + (zn(in)-z(k))/(z(k+1)-z(k))        
  endrep until in eq nzn-1
  
return
end

