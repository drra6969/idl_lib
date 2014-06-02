PRO readsat
COMMON procommon, nsat,startime,itot,pi,ntmax, $
                  rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd, $
                  xsi,ysi,zsi,vxsi,vysi,vzsi, $
                  time,bxs,bys,bzs,vxs,vys,vzs,rhos,ps,bs,ptots, $
                  jxs,jys,jzs,xs,ys,zs,cutalong

; READ INPUT DATA FOR NSAT SATELLITES
;------------------------------------
  pbd = fltarr(2) & rhobd = pbd & vxbd = pbd & vybd = pbd & vzbd = pbd
  bxbd = pbd & bybd = pbd & bzbd = pbd
  zeit = 0.0 
  
  openr, 8, 'satbin',/F77_UNFORMATTED
  readu, 8,  nsat,startime
  print, 'Number of satellites:',nsat,'     Start time:',startime

  xsi=fltarr(nsat) & ysi=xsi & zsi=xsi
  xi=xsi & yi=xsi & zi=xsi
  vxsi=xsi & vysi=xsi & vzsi=xsi
  vxi=xsi & vyi=xsi & vzi=xsi & bxi=xsi & byi=xsi & bzi=xsi
  rhoi=xsi &  pri=xsi &  bi=xsi 
  xs=fltarr(nsat,ntmax) & ys=xs & zs=xs 
  bxs=xs & bys=xs & bzs=xs & vxs=xs & vys=xs & vzs=xs
  rhos=xs & ps=xs & bs=xs & ptots=xs

  readu, 8, nnorm,bnorm,vnorm,pnorm,lnorm,tnorm
  readu, 8, rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd
  readu, 8, xsi,ysi,zsi,vxsi,vysi,vzsi
  i=0
  while not eof(8) do begin
     readu, 8, zeit,bxi,byi,bzi,vxi,vyi,vzi,rhoi,pri,bi,xi,yi,zi
     time(i)=zeit   & bxs(*,i)=bxi & bys(*,i)=byi & bzs(*,i)=bzi
     vxs(*,i)=vxi   & vys(*,i)=vyi & vzs(*,i)=vzi
     rhos(*,i)=rhoi & ps(*,i)=pri   & bs(*,i)=bi & ptots(*,i)= pri+bi^2
     xs(*,i)=xi   & ys(*,i)=yi &  zs(*,i)=zi
     itot=i
     if i lt ntmax-1 then i=i+1 else stop, ' to many records '
  endwhile
  time=time(0:itot) & bxs=bxs(*,0:itot) & bys=bys(*,0:itot) & bzs=bzs(*,0:itot)
  vxs=vxs(*,0:itot) & vys=vys(*,0:itot) & vzs=vzs(*,0:itot)
  rhos=rhos(*,0:itot) & ps=ps(*,0:itot) & bs=bs(*,0:itot)
  ptots=ptots(*,0:itot)
  xs=xs(*,0:itot) & ys=ys(*,0:itot) & zs=zs(*,0:itot)

  itot=itot+1
  print, 'Size of datasets:',itot
  close, 8
  
return
end

  
