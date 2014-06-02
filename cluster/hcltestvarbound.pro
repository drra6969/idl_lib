Pro hcltestvar,timep1,timep3,timep4,ncis1,ncis3,ncis4,tmin,tmax,isc,smo,iterror

;determine varaible for dHT and Walen tests

COMMON bfield, nfgm1,timem1,statsm1,b1,varbt1,varbb1, $
               nfgm2,timem2,statsm2,b2,varbt2,varbb2, $
               nfgm3,timem3,statsm3,b3,varbt3,varbb3, $
               nfgm4,timem4,statsm4,b4,varbt4,varbb4
COMMON bulk, n1,v1,t1,p1,n3,v3,t3,p3,n4,v4,t4,p4
COMMON test, timeqs,nnn,neqs,veqs,bave

  if (isc eq 1) then begin
       neqs=n1 & veqs=v1 & bave=b1 
       numb=nfgm1 & numv=ncis1 & timeb=timem1 & timev=timep1
  endif
  if (isc eq 3) then begin
       neqs=n3 & veqs=v3 & bave=b3 
       numb=nfgm3 & numv=ncis3 & timeb=timem3 & timev=timep3
  endif
  if (isc eq 4) then begin
       neqs=n4 & veqs=v4 & bave=b4 
       numb=nfgm4 & numv=ncis4 & timeb=timem4 & timev=timep4
  endif
  
  hindbd,tmin,tmax,numb,timeb,itbs,itbe
  hindbd,tmin,tmax,numv,timev,itvs,itve
  
  if ( (itbe-itbs lt 3) or (itve-itvs) lt 3) then begin 
    print, 'Not enough data to test in the interval!!!'
    iterror='y'
    return
  endif 
  numb=itbe-itbs+1 & numv=itve-itvs+1
  timeb=timeb(itbs:itbe) & timev=timev(itvs:itve)
  bave=bave(*,itbs:itbe) &  neqs=neqs(itvs:itve) & veqs=veqs(*,itvs:itve) 
  
  i=0 & j=0 & k=0 & timeqs=timeb
  while ( (timeb(i) lt timev(j)) and (i lt numb-1) ) do i=i+1
  while (i lt numb-1) do begin
    while ( (timev(j) lt timeb(i)) and (j lt numv-1) )  do j=j+1
    if timev(j) eq timeb(i) then begin
       timeqs(k)=timev(j)
       neqs(k)=neqs(j) & veqs(*,k)=veqs(*,j) & bave(*,k)=bave(*,i) 
       k=k+1
     endif
     i=i+1
  endwhile
  nnn=k
  if nnn lt 3 then begin     
    print, 'Not enough data to test in the interval!!!'
    print,'n',n
    iterror='y'
    return
  endif 
  timeqs=timeqs(0:nnn-1) 
  neqs=neqs(0:nnn-1) & veqs=veqs(*,0:nnn-1) & bave=bave(*,0:nnn-1) 
  
  print,'Data Interval with n=',nnn
  print,'  in tmin=',tmin,'  and tmax=',tmax
  iterror='n'

 return
end