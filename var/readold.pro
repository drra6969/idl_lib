PRO readold
COMMON procommon, nsat,startime,itot,pi,$
                  nnorm,bnorm,vnorm,pnorm,lnorm,tnorm,$
                  rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd,$
                  xs0,ys0,zs0,vxs0,vys0,vzs0,$
                  time,bxs,bys,bzs,vxs,vys,vzs,rhos,ps,bs,ptots,$
                  xs,ys,zs

; READ INPUT DATA FOR NSAT SATELLITES
;------------------------------------
  pbd = findgen(2) 
  rhobd = pbd & vxbd = pbd & vybd = pbd & vzbd = pbd
  bxbd = pbd & bybd = pbd & bzbd = pbd
  zeit = 0.0  
  
  nsat=5 
  philm=0.26
  dumm='' & dumm7=strarr(7)
  xs0=fltarr(nsat) & ys0=xs0 & zs0=xs0
  vxs0=xs0 & vys0=xs0 & vzs0=xs0
  xsi=0.0 & vli=xsi & vmi=xsi & vni=xsi & bli=xsi & bmi=xsi & bni=xsi
  rhoi=xsi &  pri=xsi &  bi=xsi 
  xs=fltarr(nsat,nt) & ys=xs & zs=xs 
  bxs=xs & bys=xs & bzs=xs & vxs=xs & vys=xs & vzs=xs
  rhos=xs & ps=xs & bs=xs & ptots=xs
  
  print, 'What filenumber?'
  read, 8
  openr,8,'sat'+string(num,'(i1)')
  readf,8,format='(a55,f9.4)',dumm,philm
  readf,8,format='(a80)',dumm
  readf,8,format='(a80)',dumm
  cophi=cos(philm) & siphi=sin(philm)

; Read off satellite position data
  for i=0,4 do begin
    readf,8, format='(a7,f7.2,a3,f7.2)',dumm,xsi,dumm,ysi
    xs0(i)=xsi & ys0(i)=ysi & zs0(i)=0.0
  endfor
  readf,8,dumm7

; Read off all satellite data
  i=0
  while not eof(num) do begin
    for k=0,4 do begin
      readf,8, zeit,bli,bmi,bni,vli,vmi,vni,rhoi,pri,bi,ptoti 
      time(i) = zeit 
      bx(i,k) = bni
      by(i,k) = bli*cophi-bmi*siphi &  bz(i,k) = bli*siphi+bmi*cophi
      vx(i,k) = vni
      vy(i,k) = vli*cophi-vmi*siphi &  vz(i,k) = vli*siphi+vmi*cophi
      rhos(i,k)=rhoi & ps(i,k)=pri & bs(i,k)=bi & ptots(i,k)=ptoti
    endfor
    itot=i
    if i lt 499 then i=i+1 else stop, 'Too many records.'
  endwhile
  for k=0,4 do begin
    xs(*,k)=time(*)*xs0(k) 
    ys(*,k)=time(*)*ys0(k)  
    zs(*,k)=time(*)*zs0(k)  
  time=time(0:itot) & bxs=bxs(0:itot,*) & bys=bys(0:itot,*) & bzs=bzs(0:itot,*)
  vxs=vxs(0:itot,*) & vys=vys(0:itot,*) & vzs=vzs(0:itot,*)
  rhos=rhos(0:itot,*) & ps=ps(0:itot,*) & bs=bs(0:itot,*)
  rhos=rhos(0:itot,*) & ps=ps(0:itot,*) & bs=bs(0:itot,*) 
  ptots=ptots(0:itot,*)
  xs=xs(0:itot,*) & ys=ys(0:itot,*) & zs=zs(0:itot,*)
  
  nt=itot+1
  close, 8
  
return
end

  
