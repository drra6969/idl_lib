
window, xsize=853, ysize=710
  !P.CHARSIZE=3.0
  !P.CHARTHICK=2.5
  xyouts,0.15*xsize,0.95*ysize,'Evolution of Magnetic Flux Tubes',$
      charsize=2.5,charthick=3,/device
  xyouts,0.7*xsize,0.89*ysize,'time:',$
      charsize=2.0,charthick=2.5,/device
     imgif = tvrd()
     gifname='title1'
     WRITE_GIF, gifname, imgif

end


