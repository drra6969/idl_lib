PRO hnobadb

COMMON bfield, nfgm1,timem1,statsm1,b1,varbt1,varbb1, $
               nfgm2,timem2,statsm2,b2,varbt2,varbb2, $
               nfgm3,timem3,statsm3,b3,varbt3,varbb3, $
               nfgm4,timem4,statsm4,b4,varbt4,varbb4

  blim=150.           
  ind=where((statsm1(0,*) ge 1) and abs(b1(0,*) lt blim) and $
             abs(b1(1,*) lt blim) and abs(b1(2,*) lt blim) )
  timem1=timem1(ind) & b1=b1(*,ind) & statsm1=statsm1(*,ind)
                       varbt1=varbt1(ind) & varbb1=varbb1(ind)
  ind=where((statsm2(0,*) ge 1) and abs(b2(0,*) lt blim) and $
             abs(b2(1,*) lt blim) and abs(b2(2,*) lt blim) )
  timem2=timem2(ind) & b2=b2(*,ind) & statsm2=statsm2(*,ind)
                       varbt2=varbt2(ind) & varbb2=varbb2(ind)
  ind=where((statsm3(0,*) ge 1) and abs(b3(0,*) lt blim) and $
             abs(b3(1,*) lt blim) and abs(b3(2,*) lt blim) )
  timem3=timem3(ind) & b3=b3(*,ind) & statsm3=statsm3(*,ind)
                       varbt3=varbt3(ind) & varbb3=varbb3(ind)
  ind=where((statsm4(0,*) ge 1) and abs(b4(0,*) lt blim) and $
             abs(b4(1,*) lt blim) and abs(b4(2,*) lt blim) )
  timem4=timem4(ind) & b4=b4(*,ind) & statsm4=statsm4(*,ind)
                       varbt4=varbt4(ind) & varbb4=varbb4(ind)
  
  ddim=size(timem1) & nfgm1=ddim(1) & print,'nfgm1:',nfgm1
  ddim=size(timem2) & nfgm2=ddim(1) & print,'nfgm2:',nfgm2
  ddim=size(timem3) & nfgm3=ddim(1) & print,'nfgm3:',nfgm3
  ddim=size(timem4) & nfgm4=ddim(1) & print,'nfgm4:',nfgm4
  
  return
end
