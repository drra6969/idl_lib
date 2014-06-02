pro plotit1

; make plots for the 97 AGU poster

   restore, file='/why1/zhu/io2d/gradpe/data/alfven/smin.dat'
   nplots=50
   time=findgen(nplots)*ta_time*0.5
;   altitude=plcoo*500.+150.
   altitude=plcoo
   ii=where(altitude le 600)

;   if !d.flags/256 then window, /free else device, bits=8, /color
;   load_mycolor
;   gamma_ct, 0.36
;   f7 = f7*res_units
;   image, transpose(f7(ii,*)), time, altitude(ii), xsty=1, yst=1,  $
;        xtit='Time (s)', ytit='Altitude (km)', unit='Resistivity (Ohm m)',  $
;        title='Resistivity', zmax=max(abs(f7(ii,*)))*1.01

;   if !d.flags/256 then window, /free else device, bits=8, /color
;   load_mycolor
;   gamma_ct, 0.36
;   f13a = f13a*t_units
;   image, transpose(f13a(ii,*)), time, altitude(ii), xsty=1, yst=1,  $
;        xtit='Time (s)', ytit='Altitude (km)', unit='Temperature (1000 K)',  $
;        title='Electron Temperature', zmax=max(abs(f13a(ii,*)))*1.01

   if !d.flags/256 then window, /free else device, bits=8, /color
   load_mycolor
   gamma_ct, 0.36
   f13 = f13*t_units
   image, transpose(f13(ii,*)), time, altitude(ii), xsty=1, yst=1,  $
        xtit='Time (s)', ytit='Altitude (km)', $
        unit='Temperature (1000 K)',  $
        title='Solarmin Ion Temperature', zmax=max(abs(f13(ii,*)))*1.01

;   if !d.flags/256 then window, /free else device, bits=8, /color
;   load_mycolor
;   gamma_ct, 0.36
;   f11 = f11*0.5
;   image, transpose(f11(ii,*)), time, altitude(ii), xsty=1, yst=1,  $
;        xtit='Time (s)', ytit='Altitude (km)', $
;        unit=' Density (x10^5)',  $
;        title='Plasma Density at X=0 (Km)', zmax=max(abs(f11(ii,*)))*1.01

end
