Pro hcurrent

COMMON bfield, nfgm1,timem1,statsm1,b1,varbt1,varbb1, $
               nfgm2,timem2,statsm2,b2,varbt2,varbb2, $
               nfgm3,timem3,statsm3,b3,varbt3,varbb3, $
               nfgm4,timem4,statsm4,b4,varbt4,varbb4
COMMON sc, naux,time,sccoor,scvel,scdr1,scdr2,scdr3,scdr4,gsegsm,diptild, $
           re, rsc,dxsc,dysc,dzsc,volsc,volrat
COMMON current, njc,timec,jc,divb           

; BASE FOR CURRENT DENSITY IS SC3 
;  Tasks: - determine that b_i are present in each vicinty of b3
;         - interpolate b_i to same time
;         - determine coefficients for linear interpolation of b between SC
;            centered at SC3
;         - determine current density from expansion coefficients
;  e.g.: Bx1 = Bx0 + axx*x1 + axy*y1 + axz*z1
;              with index 1 ~ sc1 location and Bx0 is BX(SC3)
;       => [Bx1,Bx2,Bx3]-[Bx0,Bx0,Bx0]=RR#alpx
;           with alpx=[axx,axy,axz] 
;           and  RR=[[x1,y1,z1],[x2,y2,z2],[x3,y3,z3]]
;       Solution: alpx=InvRR#([Bx1,Bx2,Bx3]-[Bx0,Bx0,Bx0])

  timec=timem3 & divb=timec & jc=fltarr(3,nfgm3)

; Tolerance for interpolation of b_i (in ms) and min tetrahedron volume 
  ttol=9000. & critvol=0.1
  mu0=1./4/!pi*1.e7
  
  imax1=nfgm1-1 & imax2=nfgm2-1 & imax3=nfgm3-1 & imax4=nfgm4-1
  imaxsc=naux-1
  print, 'Array dimensions (b and aux)',imax1, imax2, imax3, imax4, imaxsc

; Min and max indices for loop through b3 data
  j=0  & i1=0 & i2=0 & i3=0 & i4=0 & isc=0
  if volrat(isc) gt critvol then  $
     locmat, isc,scdr1,scdr2,scdr3,scdr4,volrat,critvol,invr
  while ( (timem3(imax3) gt timem1(imax1)) or $
          (timem3(imax3) gt timem2(imax2)) or $
          (timem3(imax3) gt timem4(imax4)) )  do imax3=imax3-1  
  while ( ( (timem3(i3) lt timem1(i1)) or (timem3(i3) lt timem2(i2)) $
                                       or (timem3(i3) lt timem4(i4)) ) $
          and (i3 lt (imax3-1)) ) do i3=i3+1
  i3=i3-1

; Step through b3 data subject to above conditions and determine J
   while i3 lt imax3-1 do begin
      i3=i3+1
      while ( (timem1(i1) lt timem3(i3)) and (i1 lt imax1-1) ) do i1=i1+1
      while ( (timem2(i2) lt timem3(i3)) and (i2 lt imax2-1) ) do i2=i2+1
      while ( (timem4(i4) lt timem3(i3)) and (i4 lt imax4-1) ) do i4=i4+1
      while ( (time(isc) lt (timem3(i3)-30000.)) and (isc lt imaxsc-1) ) $
        do begin 
          isc=isc+1
          if volrat(isc) gt critvol then $
                     locmat, isc,scdr1,scdr2,scdr3,scdr4,volrat,critvol,invr
      endwhile
;      
      if ( (timem1(i1) gt timem3(i3)) and (timem2(i2) gt timem3(i3)) $
            and (timem4(i4) gt timem3(i3)) and (volrat(isc) gt critvol) $
            and (timem1(i1)-timem1(i1-1) lt ttol) $
            and (timem2(i2)-timem2(i2-1) lt ttol) $
            and (timem4(i4)-timem4(i4-1) lt ttol) ) then begin
        bc1=b1(*,i1)+(b1(*,i1)-b1(*,i1-1)) $
              *(timem3(i3)-timem1(i1-1))/(timem1(i1)-timem1(i1-1))
        bc2=b2(*,i2)+(b2(*,i2)-b2(*,i2-1)) $
              *(timem3(i3)-timem2(i2-1))/(timem2(i2)-timem2(i2-1))
        bc3=b4(*,i4)+(b4(*,i4)-b4(*,i4-1)) $
              *(timem3(i3)-timem4(i4-1))/(timem4(i4)-timem4(i4-1))
        bx1=[bc1(0),bc2(0),bc3(0)] & by1=[bc1(1),bc2(1),bc3(1)]
        bz1=[bc1(2),bc2(2),bc3(2)] 
        bbx=bx1-b3(0,i3) & bby=by1-b3(1,i3) & bbz=bz1-b3(2,i3)
        alpx=invr#bbx & alpy=invr#bby &  alpz=invr#bbz 
        divb(j)=alpx(0)+alpy(1)+alpz(2) & timec(j)=timem3(i3)
        jc(0,j) = alpz(1)-alpy(2) & jc(1,j) = alpx(2)-alpz(0)
        jc(2,j) = alpy(0)-alpx(1)
        j=j+1        
      endif            
   endwhile
   njc=j & j=j-1
   timec=timec(0:j) & divb=divb(0:j) & jc=jc(*,0:j)
;   Normalisation: L=10^3m, B=10^(-9)T => B/L=10^(-12)
   j0=mu0*1.e-12
   jc=j0*jc & divb=1.e-12*divb
   
 return
end
   
   
