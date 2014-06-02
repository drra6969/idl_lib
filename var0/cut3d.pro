PRO cut3d,tim
COMMON procommon, nsat,startime,itot,pi,ntmax, $
                  rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd, $
                  xsi,ysi,zsi,vxsi,vysi,vzsi, $
                  time,bxs,bys,bzs,vxs,vys,vzs,rhos,ps,bs,ptots, $
                  jxs,jys,jzs,xs,ys,zs,cutalong
COMMON simdat3, nx,ny,nz,x,y,z,dx,dy,dz,bx,by,bz,vx,vy,vz,rho,u,res,p,$
                jx,jy,jz,xmin,xmax,ymin,ymax,zmin,zmax,linealong,icut


   pbd = fltarr(2) & rhobd = pbd & vxbd = pbd & vybd = pbd & vzbd = pbd
   bxbd = pbd & bybd = pbd & bzbd = pbd
   zeit = 0.0 

   tim=0.0 & fnumber=1 & cutalong='x' & bdok='y' & linealong='y'
   icut=1
   nxf = 401   &  nyf = 201 &  nzf = 201

   print, 'Cut along x or y or z:'
   read, cutalong 
   if ((cutalong ne 'y') and (cutalong ne 'z')) then cutalong='x'
;   print,'cutalong=',cutalong

; Next 3 blocks are for average boundary values along cut
   if cutalong eq 'x' then begin
     print, 'boundaries along x ',xmin,'/ ',xmax,'ok? (enter,y, or n)'
     read, bdok
     if bdok eq 'n' then begin
       print,'Enter new xmin, xmax'
       read,xmin,xmax
     endif
     if (xmin lt x(1)) then begin
       print, 'warning! xmin:',xmin,' out of bounds:',x(1),' Reset!'
       xmin=x(1)
     endif  
     if (xmax gt x(nx-2)) then begin
       print, 'warning! xmax:',xmax,' out of bounds:',x(nx-2),' Reset!'
       xmax=x(nx-2)
     endif  
     rhobd(0)=total(rho(1,*,*))/ny/nz & rhobd(1)=total(rho(nx-2,*,*))/ny/nz
     pbd(0)=total(p(1,*,*))/ny/nz     & pbd(1)=total(p(nx-2,*,*))/ny/nz
     bxbd(0)=total(bx(1,*,*))/ny/nz   & bxbd(1)=total(bx(nx-2,*,*))/ny/nz
     bybd(0)=total(by(1,*,*))/ny/nz   & bybd(1)=total(by(nx-2,*,*))/ny/nz
     bzbd(0)=total(bz(1,*,*))/ny/nz   & bzbd(1)=total(bz(nx-2,*,*))/ny/nz
     vxbd(0)=total(vx(1,*,*))/ny/nz   & vxbd(1)=total(vx(nx-2,*,*))/ny/nz
     vybd(0)=total(vy(1,*,*))/ny/nz   & vybd(1)=total(vy(nx-2,*,*))/ny/nz
     vzbd(0)=total(vz(1,*,*))/ny/nz   & vzbd(1)=total(vz(nx-2,*,*))/ny/nz
;     vy=vy-vybd(0) & vybd(0)=0.0 & vybd(1)=vybd(1)-vybd(0)
;     vz=vz-vzbd(0) & vzbd(0)=0.0 & vzbd(1)=vzbd(1)-vzbd(0)
   endif
   if cutalong eq 'y' then begin
     print, 'boundaries along y ',ymin,'/ ',ymax,'ok? (enter,y, or n)'
     read, bdok
     if bdok eq 'n' then begin
       print,'Enter new ymin, ymax'
       read,ymin,ymax
     endif
     if (ymin lt y(1)) then begin
       print, 'warning! ymin:',ymin,' out of bounds:',y(1),' Reset!'
       ymin=y(1)
     endif  
     if (ymax gt y(ny-2)) then begin
       print, 'warning! ymax:',ymax,' out of bounds:',y(ny-2),' Reset!'
       ymax=y(ny-2)
     endif  
     rhobd(0)=total(rho(*,1,*))/nx/nz & rhobd(1)=total(rho(*,ny-2,*))/nx/nz
     pbd(0)=total(p(*,1,*))/nx/nz     & pbd(1)=total(p(*,ny-2,*))/nx/nz
     bxbd(0)=total(bx(*,1,*))/nx/nz   & bxbd(1)=total(bx(*,ny-2,*))/nx/nz
     bybd(0)=total(by(*,1,*))/nx/nz   & bybd(1)=total(by(*,ny-2,*))/nx/nz
     bzbd(0)=total(bz(*,1,*))/nx/nz   & bzbd(1)=total(bz(*,ny-2,*))/nx/nz
     vxbd(0)=total(vx(*,1,*))/nx/nz   & vxbd(1)=total(vx(*,ny-2,*))/nx/nz
     vybd(0)=total(vy(*,1,*))/nx/nz   & vybd(1)=total(vy(*,ny-2,*))/nx/nz
     vzbd(0)=total(vz(*,1,*))/nx/nz   & vzbd(1)=total(vz(*,ny-2,*))/nx/nz
   endif
   if cutalong eq 'z' then begin
     print, 'boundaries along z ',zmin,'/ ',zmax,'ok? (enter,y, or n)'
     read, bdok
     if bdok eq 'n' then begin
       print,'Enter new zmin, zmax'
       read,zmin,zmax
     endif
     if (zmin lt z(1)) then begin
       print, 'warning! zmin:',zmin,' out of bounds:',z(1),' Reset!'
       zmin=z(1)
     endif  
     if (zmax gt z(nz-2)) then begin
       print, 'warning! zmax:',zmax,' out of bounds:',z(nz-2),' Reset!'
       zmax=z(nz-2)
     endif  
     rhobd(0)=total(rho(*,*,1))/nx/ny & rhobd(1)=total(rho(*,*,nz-2))/nx/ny
     pbd(0)=total(p(*,*,1))/nx/ny     & pbd(1)=total(p(*,*,nz-2))/nx/ny
     bxbd(0)=total(bx(*,*,1))/nx/ny   & bxbd(1)=total(bx(*,*,nz-2))/nx/ny
     bybd(0)=total(by(*,*,1))/nx/ny   & bybd(1)=total(by(*,*,nz-2))/nx/ny
     bzbd(0)=total(bz(*,*,1))/nx/ny   & bzbd(1)=total(bz(*,*,nz-2))/nx/ny
     vxbd(0)=total(vx(*,*,1))/nx/ny   & vxbd(1)=total(vx(*,*,nz-2))/nx/ny
     vybd(0)=total(vy(*,*,1))/nx/ny   & vybd(1)=total(vy(*,*,nz-2))/nx/ny
     vzbd(0)=total(vz(*,*,1))/nx/ny   & vzbd(1)=total(vz(*,*,nz-2))/nx/ny
   endif

; GRID FOR INTERPOLATION
   ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
   xf=findgen(nxf) & yf=findgen(nyf) & zf=findgen(nzf)
   dxf=(xmax-xmin)/float(nxf-1) & xf=xf*dxf+xmin
   dyf=(ymax-ymin)/float(nyf-1) & yf=yf*dyf+ymin
   dzf=(zmax-zmin)/float(nzf-1) & zf=zf*dzf+zmin
   in=-1 & k=0
   repeat begin
     in=in+1
     while xf(in) gt x(k+1) do k=k+1
     ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
   endrep until in eq nxf-1
   in=-1 & k=0
   repeat begin
     in=in+1
     while yf(in) gt y(k+1) do k=k+1
     ioyf(in) = float(k) + (yf(in)-y(k))/(y(k+1)-y(k))        
   endrep until in eq nyf-1
   in=-1 & k=0
   repeat begin
     in=in+1
     while zf(in) gt z(k+1) do k=k+1
     iozf(in) = float(k) + (zf(in)-z(k))/(z(k+1)-z(k))        
   endrep until in eq nzf-1

line:
   print, 'Line along x or y or z:(different from cut:',cutalong,')'
   read, linealong 
   if ((linealong eq cutalong) or $
       ((linealong ne 'x')and(linealong ne 'y')and(linealong ne 'z') ) ) $
          then begin
           print, 'linealong=',linealong,' makes no sense!!!'
           print, 'Try again!'
           goto, line
       endif
   print,'cutalong=',cutalong, 'linealong=',linealong

;determine arrays for x,y plane
   if ((cutalong ne 'z') and (linealong ne 'z')) then begin
     for i=0,nz-1 do  print,format='(i3,f10.2)',i,z(i)
     print, 'Enter z index for the cuts from the above range!'
     read, icut
     if (icut<0 or icut>nz-1) then begin
       icut=(nz-1)/2
       print,'z index out of range, reset to: z(', icut,')=',z(icut)
     endif
     bxf=bx(*,*,icut) & bxf=reform(bxf)
     byf=by(*,*,icut) & byf=reform(byf)
     bzf=bz(*,*,icut) & bzf=reform(bzf)
     vxf=vx(*,*,icut) & vxf=reform(vxf)
     vyf=vy(*,*,icut) & vyf=reform(vyf)
     vzf=vz(*,*,icut) & vzf=reform(vzf)
     jxf=jx(*,*,icut) & jxf=reform(jxf)
     jyf=jy(*,*,icut) & jyf=reform(jyf)
     jzf=jz(*,*,icut) & jzf=reform(jzf)
     rhof=rho(*,*,icut) & rhof=reform(rhof)
     pf=p(*,*,icut) & pf=reform(pf)
     if cutalong eq 'x' then  begin 
       nsat=nyf & itot=nxf & startime=xmin
     endif else begin
       nsat=nxf & itot=nyf & startime=ymin
     endelse    
   endif 
   if ((cutalong ne 'y') and (linealong ne 'y')) then begin
     for i=0,ny-1 do  print,format='(i3,f10.2)',i,y(i)
     print, 'Enter y index for the cuts from the above range!'
     read, icut
     if (icut<0 or icut>ny-1) then begin
       icut=(ny-1)/2
       print,'y index out of range, reset to: y(', icut,')=',y(icut)
     endif
     bxf=bx(*,icut,*) & bxf=reform(bxf)
     byf=by(*,icut,*) & byf=reform(byf)
     bzf=bz(*,icut,*) & bzf=reform(bzf)
     vxf=vx(*,icut,*) & vxf=reform(vxf)
     vyf=vy(*,icut,*) & vyf=reform(vyf)
     vzf=vz(*,icut,*) & vzf=reform(vzf)
     jxf=jx(*,icut,*) & jxf=reform(jxf)
     jyf=jy(*,icut,*) & jyf=reform(jyf)
     jzf=jz(*,icut,*) & jzf=reform(jzf)
     rhof=rho(*,icut,*) & rhof=reform(rhof)
     pf=p(*,icut,*) & pf=reform(pf)
     if cutalong eq 'x' then  begin 
       nsat=nzf & itot=nxf & startime=xmin
     endif else begin
       nsat=nxf & itot=nzf & startime=zmin
     endelse 
   endif
   if ((cutalong ne 'x') and (linealong ne 'x')) then begin
     for i=0,nx-1 do  print,format='(i3,f10.2)',i,x(i)
     print, 'Enter x index for the cuts from the above range!'
     read, icut
     if (icut<0 or icut>nx-1) then begin
       icut=(nx-1)/2
       print,'x index out of range, reset to: x(', icut,')=',x(icut)
     endif
     bxf=bx(icut,*,*) & bxf=reform(bxf)
     byf=by(icut,*,*) & byf=reform(byf)
     bzf=bz(icut,*,*) & bzf=reform(bzf)
     vxf=vx(icut,*,*) & vxf=reform(vxf)
     vyf=vy(icut,*,*) & vyf=reform(vyf)
     vzf=vz(icut,*,*) & vzf=reform(vzf)
     jxf=jx(icut,*,*) & jxf=reform(jxf)
     jyf=jy(icut,*,*) & jyf=reform(jyf)
     jzf=jz(icut,*,*) & jzf=reform(jzf)
     rhof=rho(icut,*,*) & rhof=reform(rhof)
     pf=p(icut,*,*) & pf=reform(pf)
     if cutalong eq 'y' then  begin 
       nsat=nzf & itot=nyf & startime=ymin
     endif else begin
       nsat=nyf & itot=nzf & startime=zmin
     endelse 
   endif
 
   xsi=fltarr(nsat) & ysi=xsi & zsi=xsi 
   vxsi=xsi & vysi=xsi & vzsi=xsi
   xs=fltarr(nsat,itot) & ys=xs & zs=xs 
   bxs=xs & bys=xs & bzs=xs & vxs=xs & vys=xs & vzs=xs
   rhos=xs & ps=xs & bs=xs & ptots=xs & jxs=xs & jys=xs & jzs=xs
   time=fltarr(itot) 

;print,'1. bxs',size(bxs), ' bxf',size(bxf),' ioxf',size(ioxf),$
;         ' ioyf:',size(ioyf),' iozf:',size(iozf) 
   if ((cutalong ne 'z') and (linealong ne 'z')) then begin
     bxs=interpolate(bxf,ioxf,ioyf,/grid)
     bys=interpolate(byf,ioxf,ioyf,/grid)
     bzs=interpolate(bzf,ioxf,ioyf,/grid)
     vxs=interpolate(vxf,ioxf,ioyf,/grid)
     vys=interpolate(vyf,ioxf,ioyf,/grid)
     vzs=interpolate(vzf,ioxf,ioyf,/grid)
     jxs=interpolate(jxf,ioxf,ioyf,/grid)
     jys=interpolate(jyf,ioxf,ioyf,/grid)
     jzs=interpolate(jzf,ioxf,ioyf,/grid)
     rhos=interpolate(rhof,ioxf,ioyf,/grid)
     ps=interpolate(pf,ioxf,ioyf,/grid)
   endif
   if ((cutalong ne 'y') and (linealong ne 'y')) then begin
     bxs=interpolate(bxf,ioxf,iozf,/grid)
     bys=interpolate(byf,ioxf,iozf,/grid)
     bzs=interpolate(bzf,ioxf,iozf,/grid)
     vxs=interpolate(vxf,ioxf,iozf,/grid)
     vys=interpolate(vyf,ioxf,iozf,/grid)
     vzs=interpolate(vzf,ioxf,iozf,/grid)
     jxs=interpolate(jxf,ioxf,iozf,/grid)
     jys=interpolate(jyf,ioxf,iozf,/grid)
     jzs=interpolate(jzf,ioxf,iozf,/grid)
     rhos=interpolate(rhof,ioxf,iozf,/grid)
     ps=interpolate(pf,ioxf,iozf,/grid)
   endif
   if ((cutalong ne 'x') and (linealong ne 'x')) then begin
     bxs=interpolate(bxf,ioyf,iozf,/grid)
     bys=interpolate(byf,ioyf,iozf,/grid)
     bzs=interpolate(bzf,ioyf,iozf,/grid)
     vxs=interpolate(vxf,ioyf,iozf,/grid)
     vys=interpolate(vyf,ioyf,iozf,/grid)
     vzs=interpolate(vzf,ioyf,iozf,/grid)
     jxs=interpolate(jxf,ioyf,iozf,/grid)
     jys=interpolate(jyf,ioyf,iozf,/grid)
     jzs=interpolate(jzf,ioyf,iozf,/grid)
     rhos=interpolate(rhof,ioyf,iozf,/grid)
     ps=interpolate(pf,ioyf,iozf,/grid)
   endif
print,'1. bxs',size(bxs), ' bxf',size(bxf),' ioxf',size(ioxf),$
       ' ioyf:',size(ioyf),' iozf:',size(iozf) 

   if cutalong eq 'x' then  begin 
     time=xf 
     bxs=rotate(bxs,4) & bys=rotate(bys,4) & bzs=rotate(bzs,4)
     vxs=rotate(vxs,4) & vys=rotate(vys,4) & vzs=rotate(vzs,4)
     jxs=rotate(jxs,4) & jys=rotate(jys,4) & jzs=rotate(jzs,4)
     rhos=rotate(rhos,4) & ps=rotate(ps,4)
     if linealong eq 'y' then begin
       xsi[0:nsat-1]=xf(0) & ysi=yf & zsi[0:nsat-1]=z(icut)
       for i=0,nsat-1 do xs(i,*)=xf
       for i=0,itot-1 do ys(*,i)=yf
       for i=0,itot-1 do zs(0:nsat-1,0:itot-1)=z(icut)
     endif
     if linealong eq 'z' then begin
       xsi[0:nsat-1]=xf(0) & zsi=zf & ysi[0:nsat-1]=y(icut)
       for i=0,nsat-1 do xs(i,*)=xf
       for i=0,itot-1 do zs(*,i)=zf
       for i=0,itot-1 do ys(0:nsat-1,0:itot-1)=y(icut)
     endif
   endif
   if cutalong eq 'y' then  begin 
     time=yf
     if linealong eq 'x' then  begin 
       ysi[0:nsat-1]=yf(0) & xsi=xf & zsi[0:nsat-1]=z(icut)
       for i=0,nsat-1 do ys(i,*)=yf
       for i=0,itot-1 do xs(*,i)=xf
       for i=0,itot-1 do zs(0:nsat-1,0:itot-1)=z(icut)      
     endif
     if linealong eq 'z' then  begin 
       ysi[0:nsat-1]=yf(0) & zsi=zf & xsi[0:nsat-1]=x(icut)
       for i=0,nsat-1 do ys(i,*)=yf
       for i=0,itot-1 do zs(*,i)=zf
       for i=0,itot-1 do xs(0:nsat-1,0:itot-1)=x(icut)
       bxs=rotate(bxf,4) & bys=rotate(byf,4) & bzs=rotate(bzf,4)
       vxs=rotate(vxf,4) & vys=rotate(vyf,4) & vzs=rotate(vzf,4)
       jxs=rotate(jxf,4) & jys=rotate(jyf,4) & jzs=rotate(jzf,4)
       rhos=rotate(rhof,4) & ps=rotate(pf,4)
     endif
   endif
   if cutalong eq 'z' then  begin 
     time=zf
     if linealong eq 'x' then  begin 
       zsi[0:nsat-1]=zf(0) & xsi=xf & ysi[0:nsat-1]=y(icut)
       for i=0,nsat-1 do zs(i,*)=zf
       for i=0,itot-1 do xs(*,i)=xf
       for i=0,itot-1 do ys(0:nsat-1,0:itot-1)=y(icut)
     endif
     if linealong eq 'y' then begin
       zsi[0:nsat-1]=zf(0) & ysi=yf & xsi[0:nsat-1]=x(icut)
       for i=0,nsat-1 do zs(i,*)=zf
       for i=0,itot-1 do ys(*,i)=yf
       for i=0,itot-1 do xs(0:nsat-1,0:itot-1)=x(icut)
     endif
;     bxs=bxf & bys=byf & bzs=bzf & vxs=vxf & vys=vyf & vzs=vzf
;     rhos=rhof & ps=pf & jxs=jxf & jys=jyf & jzs=jzf
   endif
   vxsi[0:nsat-1]=0.0 & vysi[0:nsat-1]=0.0 & vzsi[0:nsat-1]=0.0

   bs=sqrt(bxs*bxs+bys*bys+bzs*bzs)
   ptots=ps+bs^2
   print,xsi
  
return
end

  
