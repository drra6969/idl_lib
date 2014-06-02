PRO PRINTEQS, nt, tmin,tmax,evv1,evv2,evv3,ewv,vav,evb1,evb2,evb3,ewb,bav,$
             eve1,eve2,eve3,ewe,eav,eveh1,eveh2,eveh3,eweh,ehav,vht,$
             ccoef0,cstd,creg,strnn
; plots of b, v, and e in coordinates of e1, e2, and e3

  !P.MULTI=[0,0,0,0,0]
  !P.CHARSIZE=1.0
  !P.FONT=0
  !P.CHARTHICK=1.
  
  titl='!17 Variance Analysis of B, V, E, and Eht !3'

  erase

  xyouts, .4, 0.98, titl, /norm, alignment=0.5
  xyouts, .4, .93, '!17Variance Interval '+$
     cvhms(tmin)+'-'+cvhms(tmax)+'!3', $
      /norm, alignment=0.5
  xyouts, .75, .98, strnn, /norm
  xyouts, .75, .94, 'No. of Data points:'+string(nt,'(i4)'), /norm

  d1=0.03 & d2=0.06 & d3=0.09 & d4=0.12 & d5=0.15 & d6=0.18 
  d7=0.21 & d8=0.24 & d9=0.27 & d10=0.30
  
  p1=0.89
  xyouts, .05, p1, 'Magnetic Field:', /norm
  xyouts, .07, p1-d1, 'Average:', /norm
  xyouts, .17, p1-d1, string(bav(0),'(f7.2)'), /norm
  xyouts, .27, p1-d1, string(bav(1),'(f7.2)'), /norm
  xyouts, .37, p1-d1, string(bav(2),'(f7.2)'), /norm
  xyouts, .07, p1-d2, 'EWs:', /norm
  xyouts, .17, p1-d2, string(ewb(0),'(f8.3)'), /norm
  xyouts, .27, p1-d2, string(ewb(1),'(f8.3)'), /norm
  xyouts, .37, p1-d2, string(ewb(2),'(f8.3)'), /norm
  xyouts, .07, p1-d3, 'Eigenvectors:', /norm
  xyouts, .07, p1-d4, 'i!dB!n:', /norm
  xyouts, .17, p1-d4, string(evb1(0),'(f7.4)'), /norm
  xyouts, .27, p1-d4, string(evb1(1),'(f7.4)'), /norm
  xyouts, .37, p1-d4, string(evb1(2),'(f7.4)'), /norm
  xyouts, .07, p1-d5, 'j!dB!n:', /norm
  xyouts, .17, p1-d5, string(evb2(0),'(f7.4)'), /norm
  xyouts, .27, p1-d5, string(evb2(1),'(f7.4)'), /norm
  xyouts, .37, p1-d5, string(evb2(2),'(f7.4)'), /norm
  xyouts, .07, p1-d6, 'k!dB!n:', /norm
  xyouts, .17, p1-d6, string(evb3(0),'(f7.4)'), /norm
  xyouts, .27, p1-d6, string(evb3(1),'(f7.4)'), /norm
  xyouts, .37, p1-d6, string(evb3(2),'(f7.4)'), /norm
  xyouts, .07, p1-d7, 'Angles with:', /norm
  xyouts, .21, p1-d7, 'X', /norm
  xyouts, .31, p1-d7, 'Y', /norm
  xyouts, .41, p1-d7, 'Z', /norm
  iphix=180./!pi*acos(abs(evb1(0))) & jphix=180./!pi*acos(abs(evb2(0)))
  kphix=180./!pi*acos(abs(evb3(0))) 
  iphiy=180./!pi*acos(abs(evb1(1))) & jphiy=180./!pi*acos(abs(evb2(1)))
  kphiy=180./!pi*acos(abs(evb3(1))) 
  iphiz=180./!pi*acos(abs(evb1(2))) & jphiz=180./!pi*acos(abs(evb2(2)))
  kphiz=180./!pi*acos(abs(evb3(2))) 
  xyouts, .07, p1-d8, 'i:', /norm
  xyouts, .20, p1-d8,string(iphix,'(f4.1)'), /norm
  xyouts, .30, p1-d8,string(iphiy,'(f4.1)'), /norm
  xyouts, .40, p1-d8,string(iphiz,'(f4.1)'), /norm
  xyouts, .07, p1-d9, 'j:', /norm
  xyouts, .20, p1-d9,string(jphix,'(f4.1)'), /norm
  xyouts, .30, p1-d9,string(jphiy,'(f4.1)'), /norm
  xyouts, .40, p1-d9,string(jphiz,'(f4.1)'), /norm
  xyouts, .07, p1-d10, 'k:', /norm
  xyouts, .20, p1-d10,string(kphix,'(f4.1)'), /norm
  xyouts, .30, p1-d10,string(kphiy,'(f4.1)'), /norm
  xyouts, .40, p1-d10,string(kphiz,'(f4.1)'), /norm

  xyouts, .55, p1, 'Electric Field:', /norm
  xyouts, .57, p1-d1, 'Average:', /norm
  xyouts, .67, p1-d1, string(eav(0),'(f7.2)'), /norm
  xyouts, .77, p1-d1, string(eav(1),'(f7.2)'), /norm
  xyouts, .87, p1-d1, string(eav(2),'(f7.2)'), /norm
  xyouts, .57, p1-d2, 'EWs:', /norm
  xyouts, .67, p1-d2, string(ewe(0),'(f8.3)'), /norm
  xyouts, .77, p1-d2, string(ewe(1),'(f8.3)'), /norm
  xyouts, .87, p1-d2, string(ewe(2),'(f8.3)'), /norm
  xyouts, .57, p1-d3, 'Eigenvectors:', /norm
  xyouts, .57, p1-d4, 'i!dE!n:', /norm
  xyouts, .67, p1-d4, string(eve1(0),'(f7.4)'), /norm
  xyouts, .77, p1-d4, string(eve1(1),'(f7.4)'), /norm
  xyouts, .87, p1-d4, string(eve1(2),'(f7.4)'), /norm
  xyouts, .57, p1-d5, 'j!dE!n:', /norm
  xyouts, .67, p1-d5, string(eve2(0),'(f7.4)'), /norm
  xyouts, .77, p1-d5, string(eve2(1),'(f7.4)'), /norm
  xyouts, .87, p1-d5, string(eve2(2),'(f7.4)'), /norm
  xyouts, .57, p1-d6, 'k!dE!n:', /norm
  xyouts, .67, p1-d6, string(eve3(0),'(f7.4)'), /norm
  xyouts, .77, p1-d6, string(eve3(1),'(f7.4)'), /norm
  xyouts, .87, p1-d6, string(eve3(2),'(f7.4)'), /norm
  xyouts, .57, p1-d7, 'Angles with:', /norm
  xyouts, .71, p1-d7, 'X', /norm
  xyouts, .81, p1-d7, 'Y', /norm
  xyouts, .91, p1-d7, 'Z', /norm
  iphix=180./!pi*acos(abs(eve1(0))) & jphix=180./!pi*acos(abs(eve2(0)))
  kphix=180./!pi*acos(abs(eve3(0))) 
  iphiy=180./!pi*acos(abs(eve1(1))) & jphiy=180./!pi*acos(abs(eve2(1)))
  kphiy=180./!pi*acos(abs(eve3(1))) 
  iphiz=180./!pi*acos(abs(eve1(2))) & jphiz=180./!pi*acos(abs(eve2(2)))
  kphiz=180./!pi*acos(abs(eve3(2))) 
  xyouts, .57, p1-d8, 'i:', /norm
  xyouts, .70, p1-d8,string(iphix,'(f4.1)'), /norm
  xyouts, .80, p1-d8,string(iphiy,'(f4.1)'), /norm
  xyouts, .90, p1-d8,string(iphiz,'(f4.1)'), /norm
  xyouts, .57, p1-d9, 'j:', /norm
  xyouts, .70, p1-d9,string(jphix,'(f4.1)'), /norm
  xyouts, .80, p1-d9,string(jphiy,'(f4.1)'), /norm
  xyouts, .90, p1-d9,string(jphiz,'(f4.1)'), /norm
  xyouts, .57, p1-d10, 'j:', /norm
  xyouts, .70, p1-d10,string(kphix,'(f4.1)'), /norm
  xyouts, .80, p1-d10,string(kphiy,'(f4.1)'), /norm
  xyouts, .90, p1-d10,string(kphiz,'(f4.1)'), /norm


  p1=0.55
  xyouts, .05, p1, 'Velocity:', /norm
  xyouts, .07, p1-d1, 'Average:', /norm
  xyouts, .17, p1-d1, string(vav(0),'(f7.2)'), /norm
  xyouts, .27, p1-d1, string(vav(1),'(f7.2)'), /norm
  xyouts, .37, p1-d1, string(vav(2),'(f7.2)'), /norm
  xyouts, .07, p1-d2, 'EWs:', /norm
  xyouts, .17, p1-d2, string(ewv(0),'(f8.3)'), /norm
  xyouts, .27, p1-d2, string(ewv(1),'(f8.3)'), /norm
  xyouts, .37, p1-d2, string(ewv(2),'(f8.3)'), /norm
  xyouts, .07, p1-d3, 'Eigenvectors:', /norm
  xyouts, .07, p1-d4, 'i!dV!n:', /norm
  xyouts, .17, p1-d4, string(evv1(0),'(f7.4)'), /norm
  xyouts, .27, p1-d4, string(evv1(1),'(f7.4)'), /norm
  xyouts, .37, p1-d4, string(evv1(2),'(f7.4)'), /norm
  xyouts, .07, p1-d5, 'j!dV!n:', /norm
  xyouts, .17, p1-d5, string(evv2(0),'(f7.4)'), /norm
  xyouts, .27, p1-d5, string(evv2(1),'(f7.4)'), /norm
  xyouts, .37, p1-d5, string(evv2(2),'(f7.4)'), /norm
  xyouts, .07, p1-d6, 'k!dV!n:', /norm
  xyouts, .17, p1-d6, string(evv3(0),'(f7.4)'), /norm
  xyouts, .27, p1-d6, string(evv3(1),'(f7.4)'), /norm
  xyouts, .37, p1-d6, string(evv3(2),'(f7.4)'), /norm
  xyouts, .07, p1-d7, 'Angles with:', /norm
  xyouts, .21, p1-d7, 'X', /norm
  xyouts, .31, p1-d7, 'Y', /norm
  xyouts, .41, p1-d7, 'Z', /norm
  iphix=180./!pi*acos(abs(evv1(0))) & jphix=180./!pi*acos(abs(evv2(0)))
  kphix=180./!pi*acos(abs(evv3(0))) 
  iphiy=180./!pi*acos(abs(evv1(1))) & jphiy=180./!pi*acos(abs(evv2(1)))
  kphiy=180./!pi*acos(abs(evv3(1))) 
  iphiz=180./!pi*acos(abs(evv1(2))) & jphiz=180./!pi*acos(abs(evv2(2)))
  kphiz=180./!pi*acos(abs(evv3(2))) 
  xyouts, .07, p1-d8, 'i:', /norm
  xyouts, .20, p1-d8,string(iphix,'(f4.1)'), /norm
  xyouts, .30, p1-d8,string(iphiy,'(f4.1)'), /norm
  xyouts, .40, p1-d8,string(iphiz,'(f4.1)'), /norm
  xyouts, .07, p1-d9, 'j:', /norm
  xyouts, .20, p1-d9,string(jphix,'(f4.1)'), /norm
  xyouts, .30, p1-d9,string(jphiy,'(f4.1)'), /norm
  xyouts, .40, p1-d9,string(jphiz,'(f4.1)'), /norm
  xyouts, .07, p1-d10, 'k:', /norm
  xyouts, .20, p1-d10,string(kphix,'(f4.1)'), /norm
  xyouts, .30, p1-d10,string(kphiy,'(f4.1)'), /norm
  xyouts, .40, p1-d10,string(kphiz,'(f4.1)'), /norm


  xyouts, .55, p1, 'DeHoffmann Teller Electric Field:', /norm
  xyouts, .57, p1-d1, 'Average:', /norm
  xyouts, .67, p1-d1, string(ehav(0),'(f7.2)'), /norm
  xyouts, .77, p1-d1, string(ehav(1),'(f7.2)'), /norm
  xyouts, .87, p1-d1, string(ehav(2),'(f7.2)'), /norm
  xyouts, .57, p1-d2, 'EWs:', /norm
  xyouts, .67, p1-d2, string(eweh(0),'(f8.3)'), /norm
  xyouts, .77, p1-d2, string(eweh(1),'(f8.3)'), /norm
  xyouts, .87, p1-d2, string(eweh(2),'(f8.3)'), /norm
  xyouts, .57, p1-d3, 'Eigenvectors:', /norm
  xyouts, .57, p1-d4, 'i!dEht!n:', /norm
  xyouts, .67, p1-d4, string(eveh1(0),'(f7.4)'), /norm
  xyouts, .77, p1-d4, string(eveh1(1),'(f7.4)'), /norm
  xyouts, .87, p1-d4, string(eveh1(2),'(f7.4)'), /norm
  xyouts, .57, p1-d5, 'j!dEht!n:', /norm
  xyouts, .67, p1-d5, string(eveh2(0),'(f7.4)'), /norm
  xyouts, .77, p1-d5, string(eveh2(1),'(f7.4)'), /norm
  xyouts, .87, p1-d5, string(eveh2(2),'(f7.4)'), /norm
  xyouts, .57, p1-d6, 'k!dEht!n:', /norm
  xyouts, .67, p1-d6, string(eveh3(0),'(f7.4)'), /norm
  xyouts, .77, p1-d6, string(eveh3(1),'(f7.4)'), /norm
  xyouts, .87, p1-d6, string(eveh3(2),'(f7.4)'), /norm
  xyouts, .57, p1-d7, 'Angles with:', /norm
  xyouts, .71, p1-d7, 'X', /norm
  xyouts, .81, p1-d7, 'Y', /norm
  xyouts, .91, p1-d7, 'Z', /norm
  iphix=180./!pi*acos(abs(eveh1(0))) & jphix=180./!pi*acos(abs(eveh2(0)))
  kphix=180./!pi*acos(abs(eveh3(0))) 
  iphiy=180./!pi*acos(abs(eveh1(1))) & jphiy=180./!pi*acos(abs(eveh2(1)))
  kphiy=180./!pi*acos(abs(eveh3(1))) 
  iphiz=180./!pi*acos(abs(eveh1(2))) & jphiz=180./!pi*acos(abs(eveh2(2)))
  kphiz=180./!pi*acos(abs(eveh3(2))) 
  xyouts, .57, p1-d8, 'i:', /norm
  xyouts, .70, p1-d8,string(iphix,'(f4.1)'), /norm
  xyouts, .80, p1-d8,string(iphiy,'(f4.1)'), /norm
  xyouts, .90, p1-d8,string(iphiz,'(f4.1)'), /norm
  xyouts, .57, p1-d9, 'j:', /norm
  xyouts, .70, p1-d9,string(jphix,'(f4.1)'), /norm
  xyouts, .80, p1-d9,string(jphiy,'(f4.1)'), /norm
  xyouts, .90, p1-d9,string(jphiz,'(f4.1)'), /norm
  xyouts, .57, p1-d10, 'k:', /norm
  xyouts, .70, p1-d10,string(kphix,'(f4.1)'), /norm
  xyouts, .80, p1-d10,string(kphiy,'(f4.1)'), /norm
  xyouts, .90, p1-d10,string(kphiz,'(f4.1)'), /norm

  xyouts, .05, .21, 'DeHoffmann Teller Velocity:', /norm
  xyouts, .17, .18, string(vht(0),'(f7.2)'), /norm
  xyouts, .27, .18, string(vht(1),'(f7.2)'), /norm
  xyouts, .37, .18, string(vht(2),'(f7.2)'), /norm
  
  xyouts, .05, .14, 'Walen Relation, Correlation: '$
                     +string(creg(0),'(f6.3)'), /norm
  xyouts, .07, .11, 'Slope: ', /norm
  xyouts, .20, .11, string(ccoef0(1,0),'(f7.3)')$
                      +' +-'+string(cstd(1,0),'(f7.3)'), /norm
  xyouts, .05, .07, 'EHT vs E Field, Correlation: '$
                    +string(creg(1),'(f6.3)'), /norm
  xyouts, .07, .04, 'Slope: ',/norm
  xyouts, .20, .04, string(ccoef0(1,1),'(f7.3)')$
                      +' +-'+string(cstd(1,1),'(f7.3)'), /norm

  
   !P.FONT=-1
 
  

return
end
