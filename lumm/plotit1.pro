pro plotit1

; make plots for the 97 AGU poster

   restore, file='/tan/u1/zhu/temp/datacray/currentioniz1/realnam1.dat'
   time=findgen(28)*0.5*0.58
   altitude=plcoo*500.+150.
   ii=where(altitude le 600)
   curr_units = 1000./4./3.1415
   e_units = 1.6*1e-4*1e6
   res_units = 10./4./3.1415/1.6
   t_units = 100./8/3.1415/1.38

  f7 = f7*res_units

   if !d.flags/256 then window, /free else device, bits=8, /color
   load_mycolor
   gamma_ct, 0.36
   f7 = 1./f7
   image, transpose(f7(ii,*)), time, altitude(ii), xsty=1, yst=1,  $
        xtit='Time (s)', ytit='Altitude (km)', unit='Conductivity (mho/m)',  $
        title='Parallel Conductivity', zmax=max(abs(f7(ii,*)))*1.01

   if !d.flags/256 then window, /free else device, bits=8, /color
   load_mycolor
   gamma_ct, 0.36
   f13 = f13*t_units
   image, transpose(f13(ii,*)), time, altitude(ii), xsty=1, yst=1,  $
        xtit='Time (s)', ytit='Altitude (km)', $
        unit='Temperature (1000K)',  $
        title='Temperature', zmax=max(abs(f13(ii,*)))*1.01

end
