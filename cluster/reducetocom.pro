PRO reducetocom, naux,time,sccoor,scvel,scdr1,scdr2,scdr3,scdr4,gsegsm,diptild,$
                 nfgm,timem,statsm,b,varbt,varbb,$
                 ncis,timep,statsp,np,no,nhe,nhia,vp,vhia,tp,thia, nn
                 
;common times in aux, B, and P
  im=0 & ip=0 & ia=0
  inda=intarr(naux) & indm=intarr(nfgm) & indp=intarr(ncis)
  while (ia lt naux-1) and (ip lt ncis-1) and (im lt nfgm-1) do begin
  ttmax=max([time(ia),timem(im),timep(ip)])
      while ( (timep(ip) lt ttmax) and (ip lt ncis-1) ) do ip=ip+1
      while ( (timem(im) lt ttmax) and (im lt nfgm-1) ) do im=im+1
      while ( (time(ia) lt ttmax) and (ia lt naux-1) )  do ia=ia+1
      if (timep(ip) eq ttmax) and (time(ia) eq ttmax) then begin
        inda(ia)=1 & indm(im)=1 & indp(ip)=1
        ip=ip+1 & im=im+1 & ia=ia+1
      endif
  endwhile
  iasub=where(inda eq 1) & ipsub=where(indp eq 1) & imsub=where(indm eq 1)

  time=time(iasub) & sccoor=sccoor(*,iasub) & scvel=scvel(*,iasub)
  scdr1=scdr1(*,iasub) & scdr2=scdr2(*,iasub) & scdr3=scdr3(*,iasub) 
  scdr4=scdr4(*,iasub)
  gsegsm=gsegsm(iasub) & diptild=diptild(iasub)

  statsm=statsm(*,imsub) & b=b(*,imsub) & varbt=varbt(imsub) & varbb=varbb(imsub) 

  statsp=statsp(*,ipsub)
  np=np(ipsub)   & no=no(ipsub) & nhe=nhe(ipsub) & nhia=nhia(ipsub)
  vp=vp(*,ipsub) & vhia=vhia(*,ipsub)
  tp=tp(ipsub)   & thia=thia(ipsub)

  sizenew=size(time)
  nn=sizenew(1) & print,nn

  return
end
