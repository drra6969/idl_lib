pro plotit2

; make plots for the 97 AGU poster

   restore, file='/why1/io2dcol/alfven.dat'
   nplots=20
   time=findgen(nplots)*0.5*0.58
;   altitude=plcoo*500.+150.
   altitude=plcoo
   ii=where(altitude le 600)
   curr_units = 1000./4./3.1415
   e_units = 1.6*1e-4*1e6
   res_units = e_units/curr_units
   t_units = 100./8/3.1415/1.38

;   if !d.flags/256 then window, /free else device, bits=8, /color
;   load_mycolor
;   gamma_ct, 0.36
;   f7 = f7*res_units
;   image, transpose(f7(ii,*)), time, altitude(ii), xsty=1, yst=1,  $
;        xtit='Time (s)', ytit='Altitude (km)', unit='Resistivity (Ohm m)',  $
;        title='Resistivity', zmax=max(abs(f7(ii,*)))*1.01

   if !d.flags/256 then window, /free else device, bits=8, /color
   load_mycolor
   gamma_ct, 0.36
   f13a = f13a*t_units
   image, transpose(f13a(ii,*)), time, altitude(ii), xsty=1, yst=1,  $
        xtit='Time (s)', ytit='Altitude (km)', unit='Temperature (1000 K)',  $
        title='Electron Temperature', zmax=max(abs(f13a(ii,*)))*1.01

   if !d.flags/256 then window, /free else device, bits=8, /color
   load_mycolor
   gamma_ct, 0.36
   f13 = f13*t_units
   image, transpose(f13(ii,*)), time, altitude(ii), xsty=1, yst=1,  $
        xtit='Time (s)', ytit='Altitude (km)', $
        unit='Temperature (1000 K)',  $
        title='Ion Temperature', zmax=max(abs(f13(ii,*)))*1.01

end
