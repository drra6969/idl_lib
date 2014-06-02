pro plotit2

; make plots for the 97 AGU poster

   restore, file='/why1/zhu/io2d/betty/temp/curr_down.dat'
   nplots=40
   time=findgen(nplots)*ta_time*0.5
;   altitude=plcoo*500.+150.
   altitude=plcoo
   ii=where(altitude le 500)

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
        title='Electron Temperature at x='+string(x0,'(f4.1)')+' (km)', $
        zmax=3.5
;        zmax=max(abs(f13a(ii,*)))*1.01

;   if !d.flags/256 then window, /free else device, bits=8, /color
;   load_mycolor
;   gamma_ct, 0.36
;   f13 = f13*t_units
;   name2='Solarmax Ion Temperature at (x='+string(x0,'(f4.1)')+')'
;   image, transpose(f13(ii,*)), time, altitude(ii), xsty=1, yst=1,  $
;        xtit='Time (s)', ytit='Altitude (km)', $
;        unit='Temperature (1000 K)',  $
;        title=name2, zmax=max(abs(f13(ii,*)))*1.01

;   if !d.flags/256 then window, /free else device, bits=8, /color
;   load_mycolor
;   gamma_ct, 0.36
;   f11 = f11*0.5
;   image, transpose(f11(ii,*)), time, altitude(ii), xsty=1, yst=1,  $
;        xtit='Time (s)', ytit='Altitude (km)', $
;        unit=' Density (x10^5)',  $
;        title='Electron Density at x='+string(x0,'(f4.1)')+' (km)', $
;        zmax=max(abs(f11(ii,*)))*1.01

end
