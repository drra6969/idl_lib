Pro hprintint, nfgm1,timem1,nintb1,it1b1,it2b1,nfgm2,timem2,nintb2,it1b2,it2b2, $
               nfgm3,timem3,nintb3,it1b3,it2b3,nfgm4,timem4,nintb4,it1b4,it2b4, $
               ncis1,timep1,nintp1,it1p1,it2p1,ncis3,timep3,nintp3,it1p3,it2p3, $
               ncis4,timep4,nintp4,it1p4,it2p4
               
  print, 'Dataset SC1:'
  print, 'Total Time range (B):',timem1(0)/60.,timem1(nfgm1-1)/60.
  print, 'Continuous Time Intervals:'
  for ii=0,nintb1-1 do print,'Interval:',$
                           ii, timem1(it1b1(ii))/60.,timem1(it2b1(ii))/60.
  print, 'Total Time range (P):',timep1(0)/60.,timep1(ncis1-1)/60.
  print, 'Continuous Time Intervals:'
  for ii=0,nintp1-1 do print,'Interval:',$
                           ii, timep1(it1p1(ii))/60.,timep1(it2p1(ii))/60.

  print, 'Dataset SC2:'
  print, 'Total Time range (B):',timem2(0)/60.,timem2(nfgm2-1)/60.
  print, 'Continuous Time Intervals:'
  for ii=0,nintb2-1 do print,'Interval:',$
                           ii, timem2(it1b2(ii))/60.,timem2(it2b2(ii))/60.

  print, 'Dataset SC3:'
  print, 'Total Time range (B):',timem3(0)/60.,timem3(nfgm3-1)/60.
  print, 'Continuous Time Intervals:'
  for ii=0,nintb3-1 do print,'Interval:',$
                           ii, timem3(it1b3(ii))/60.,timem3(it2b3(ii))/60.
  print, 'Total Time range (P):',timep3(0)/60.,timep3(ncis3-1)/60.
  print, 'Continuous Time Intervals:'
  for ii=0,nintp3-1 do print,'Interval:',$
                           ii, timep3(it1p3(ii))/60.,timep3(it2p3(ii))/60.

  print, 'Dataset SC4:'
  print, 'Total Time range (B):',timem4(0)/60.,timem4(nfgm4-1)/60.
  print, 'Continuous Time Intervals:'
  for ii=0,nintb4-1 do print,'Interval:',$
                           ii, timem4(it1b4(ii))/60.,timem4(it2b4(ii))/60.
  print, 'Total Time range (P):',timep4(0)/60.,timep4(ncis4-1)/60.
  print, 'Continuous Time Intervals:'
  for ii=0,nintp4-1 do print,'Interval:',$
                           ii, timep4(it1p4(ii))/60.,timep4(it2p4(ii))/60.

 return
end