PRO hnobadp

COMMON pfield,ncis1,timep1,statsp1,np1,no1,nhe1,nhia1,vp1,vhia1,tp1,thia1, $
              ncis3,timep3,statsp3,np3,no3,nhe3,nhia3,vp3,vhia3,tp3,thia3, $
              ncis4,timep4,statsp4,np4,no4,nhe4,nhia4,vp4,vhia4,tp4,thia4 


  ind=where(((abs(np1) lt 200.) or (abs(nhia1) lt 200.)) $
          and ((abs(tp1) lt 10000.) or (abs(thia1) lt 10000.)) $
          and ( ((abs(vp1(0,*)) lt 2000.) and (abs(vp1(1,*)) lt 2000.) $
                                       and (abs(vp1(2,*)) lt 2000.)) $
             or ((abs(vhia1(0,*)) lt 2000.) and (abs(vhia1(1,*)) lt 2000.) $
                                         and (abs(vhia1(2,*)) lt 2000.)) ) $
          and (statsp1(0,*) ge 1), count)
  timep1=timep1(ind) & statsp1=statsp1(*,ind)
  np1=np1(ind)   & no1=no1(ind) & nhe1=nhe1(ind) & nhia1=nhia1(ind)
  vp1=vp1(*,ind) & vhia1=vhia1(*,ind)
  tp1=tp1(ind)   & thia1=thia1(ind)
  ddim=size(timep1) & ncis1=ddim(1) & print,'ncis1:',ncis1
  i0=where(abs(np1) gt 200.,ct)   & if ct gt 0 then np1(i0)=0.
  i0=where(abs(nhia1) gt 200.,ct) & if ct gt 0 then nhia1(i0)=0.
  i0=where(abs(tp1) gt 10000.,ct)   & if ct gt 0 then tp1(i0)=0.
  i0=where(abs(thia1) gt 10000.,ct) & if ct gt 0 then thia1(i0)=0.
  i0=where(abs(vp1) gt 2000.,ct)   & if ct gt 0 then vp1(i0)=0.
  i0=where(abs(vhia1) gt 2000.,ct) & if ct gt 0 then vhia1(i0)=0.
  

  ind=where(((abs(np3) lt 200.) or (abs(nhia3) lt 200.)) $
          and ((abs(tp3) lt 10000.) or (abs(thia3) lt 10000.)) $
          and ( ((abs(vp3(0,*)) lt 2000.) and (abs(vp3(1,*)) lt 2000.) $
                                       and (abs(vp3(2,*)) lt 2000.)) $
             or ((abs(vhia3(0,*)) lt 2000.) and (abs(vhia3(1,*)) lt 2000.) $
                                         and (abs(vhia3(2,*)) lt 2000.)) ) $
          and (statsp3(0,*) ge 1), count)
  timep3=timep3(ind) & statsp3=statsp3(*,ind)
  np3=np3(ind)   & no3=no3(ind) & nhe3=nhe3(ind) & nhia3=nhia3(ind)
  vp3=vp3(*,ind) & vhia3=vhia3(*,ind)
  tp3=tp3(ind)   & thia3=thia3(ind)
  ddim=size(timep3) & ncis3=ddim(1) & print,'ncis4:',ncis3
  i0=where(abs(np3) gt 200.,ct)   & if ct gt 0 then np3(i0)=0.
  i0=where(abs(nhia3) gt 200.,ct) & if ct gt 0 then nhia3(i0)=0.
  i0=where(abs(tp3) gt 10000.,ct)   & if ct gt 0 then tp3(i0)=0.
  i0=where(abs(thia3) gt 10000.,ct) & if ct gt 0 then thia3(i0)=0.
  i0=where(abs(vp3) gt 2000.,ct)   & if ct gt 0 then vp3(i0)=0.
  i0=where(abs(vhia3) gt 2000.,ct) & if ct gt 0 then vhia3(i0)=0.

  ind=where(((abs(np4) lt 200.) or (abs(nhia4) lt 200.)) $
          and ( ((abs(vp4(0,*)) lt 2000.) and (abs(vp4(1,*)) lt 2000.) $
                                       and (abs(vp4(2,*)) lt 2000.)) $
             or ((abs(vhia4(0,*)) lt 2000.) and (abs(vhia4(1,*)) lt 2000.) $
                                         and (abs(vhia4(2,*)) lt 2000.)) ) $
          and (statsp4(0,*) ge 1), count)
  timep4=timep4(ind) & statsp4=statsp4(*,ind)
  np4=np4(ind)   & no4=no4(ind) & nhe4=nhe4(ind) & nhia4=nhia4(ind)
  vp4=vp4(*,ind) & vhia4=vhia4(*,ind)
  tp4=tp4(ind)   & thia4=thia4(ind)
  ddim=size(timep4) & ncis4=ddim(1) & print,'ncis4:',ncis4
  i0=where(abs(np4) gt 200.,ct)   & if ct gt 0 then np4(i0)=0.
  i0=where(abs(nhia4) gt 200.,ct) & if ct gt 0 then nhia4(i0)=0.
  i0=where(abs(tp4) gt 10000.,ct)   & if ct gt 0 then tp4(i0)=0.
  i0=where(abs(thia4) gt 10000.,ct) & if ct gt 0 then thia4(i0)=0.
  i0=where(abs(vp4) gt 2000.,ct)   & if ct gt 0 then vp4(i0)=0.
  i0=where(abs(vhia4) gt 2000.,ct) & if ct gt 0 then vhia4(i0)=0.
  
  return
end
