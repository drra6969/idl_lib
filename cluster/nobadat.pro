PRO nobadat, time,sccoor,scvel,scdr1,scdr2,scdr3,scdr4,gsegsm,diptild,$
             timem,statsm,b,varbt,varbb,$
             timep,statsp,np,no,nhe,nhia,vp,vhia,tp,thia, nn
             

  ind=where(((abs(np) lt 200.) or (abs(nhia) lt 200.)) $
          and ((abs(tp) lt 10000.) or (abs(thia) lt 10000.)) $
          and ( ((abs(vp(0,*)) lt 2000.) and (abs(vp(1,*)) lt 2000.) $
                                       and (abs(vp(2,*)) lt 2000.)) $
             or ((abs(vhia(0,*)) lt 2000.) and (abs(vhia(1,*)) lt 2000.) $
                                         and (abs(vhia(2,*)) lt 2000.)) ) $
             and (abs(b(0,*)) lt 150.) and (abs(b(1,*)) lt 150.) $
             and (abs(b(2,*)) lt 150.) $
             and (statsm(0,*) eq 2) and (statsp(0,*) eq 2), count)

  time=time(ind) & sccoor=sccoor(*,ind) & scvel=scvel(*,ind)
  scdr1=scdr1(*,ind) & scdr2=scdr2(*,ind) & scdr3=scdr3(*,ind) 
  scdr4=scdr4(*,ind)
  gsegsm=gsegsm(ind) & diptild=diptild(ind)

  statsm=statsm(*,ind) & b=b(*,ind) & varbt=varbt(ind) & varbb=varbb(ind) 

  statsp=statsp(*,ind)
  np=np(ind)   & no=no(ind) & nhe=nhe(ind) & nhia=nhia(ind)
  vp=vp(*,ind) & vhia=vhia(*,ind)
  tp=tp(ind)   & thia=thia(ind)
  sizenew=size(time)
  nn=sizenew(1) & print,nn
  
  i0=where(abs(np) gt 200.,ct)   & if ct gt 0 then np(i0)=0.
  i0=where(abs(nhia) gt 200.,ct) & if ct gt 0 then nhia(i0)=0.
  i0=where(abs(tp) gt 10000.,ct)   & if ct gt 0 then tp(i0)=0.
  i0=where(abs(thia) gt 10000.,ct) & if ct gt 0 then thia(i0)=0.
  i0=where(abs(vp) gt 2000.,ct)   & if ct gt 0 then vp(i0)=0.
  i0=where(abs(vhia) gt 2000.,ct) & if ct gt 0 then vhia(i0)=0.

;  format = '(3f8.1,a2,3f8.1,a4,f6.3,a2,f6.3,4i3,a2,3f8.2)'
;  for j=0,nn-1 do begin
;    print,vp(*,j),'  ',vhia(*,j),'    ',np(j),'  ',nhia(j),statsp(*,j),$
;          '  ',b(*,j),form=format
;  endfor

  return
end
