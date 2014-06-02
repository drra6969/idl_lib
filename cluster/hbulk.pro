Pro hbulk

COMMON pfield,ncis1,timep1,statsp1,np1,no1,nhe1,nhia1,vp1,vhia1,tp1,thia1, $
              ncis3,timep3,statsp3,np3,no3,nhe3,nhia3,vp3,vhia3,tp3,thia3, $
              ncis4,timep4,statsp4,np4,no4,nhe4,nhia4,vp4,vhia4,tp4,thia4 
COMMON bulk, n1,v1,t1,p1,n3,v3,t3,p3,n4,v4,t4,p4 

  kb=1.38e-23 & nt0=kb*1.e12
  n1=nhia1 & v1=vhia1 & t1=thia1
;  n3=np3 & v3=vp3 & t3=tp3
  n3=nhia3 & v3=vhia3 & t3=thia3
  n4=nhia4 & v4=vhia4 & t4=thia4

  ind=where((nhia1 eq 0.), count)
  if count gt 0 then begin
    n1(ind)=np1(ind) & v1(*,ind)=vp1(*,ind) & t1(ind)=tp1(ind) & p1=nt0*n1*t1
  endif
;  ind=where(np3 eq 0., count)
;  n3(ind)=nhia3(ind) & v3(*,ind)=vhia3(*,ind) & t3(ind)=thia3(ind) & p3=nt0*n3*t3
  ind=where(nhia3 eq 0., count)
  if count gt 0 then begin
    n3(ind)=np3(ind) & v3(*,ind)=vp3(*,ind) & t3(ind)=tp3(ind) & p3=nt0*n3*t3
  endif
  ind=where(nhia4 eq 0., count)
  if count gt 0 then begin
    n4(ind)=np4(ind) & v4(*,ind)=vp4(*,ind) & t4(ind)=tp4(ind) & p4=nt0*n4*t4
  endif
  
 return
end