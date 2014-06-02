pro plotit3

; make plots for the 97 AGU poster

   restore, file='/tan/u1/zhu/temp/realnam2.dat'
   nplots=19
   time=findgen(nplots)*0.5*0.58
;   altitude=plcoo*500.+150.
   altitude=plcoo
   ii=where(altitude le 600)
   curr_units = 1000./4./3.1415
   e_units = 1.6*1e-4*1e6
   res_units = 10./4./3.1415/1.6
   t_units = 100./8/3.1415/1.38

   if !d.flags/256 then window, /free else device, bits=8, /color
   load_mycolor
   f01 = f01*curr_units
   f1max = max(f01(ii,*))
   image, f1max-transpose(f01(ii,*)), time, altitude(ii), xsty=1, yst=1,  $
        xtit='Time (s)', ytit='Altitude (km)',  $
        unit='Current Density (!4l!3A/m!e2!n)',  $
        title='Downward Parallel Current', zmax=max(abs(f01(ii,*)))*1.01

   if !d.flags/256 then window, /free else device, bits=8, /color
   load_mycolor
   f02 = f02*e_units
   f2max = max(f02(ii,*))
   image, f2max-transpose(f02(ii,*)), time, altitude(ii), xsty=1, yst=1,  $
        xtit='Time (s)', ytit='Altitude (km)',  $
        unit='Electric Field (!4l!3V/m)',  $
        title='Parallel Downward Electric Field', zmax=max(abs(f02(ii,*)))*1.01
   

end
