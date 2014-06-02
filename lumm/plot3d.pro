pro plot3d, px, py, pz, projection=projection, skirt=skirt, $
            b_lines=b_lines, _extra=_extra

;+
; NAME:
;    plot3d
;
; Purpose:
;    plot a line in a 3-D space:
;    plot3d, x, [y, z,], [/projection], [skirt=n]
;
;    if only array x is given, it must be 2-dimensional: x(3,*),
;    where x(0,*) gives the x-corrdinates, x(1,*) -> y, x(2,*) -> z
;         /projection causes projections on the sides of the 
;                     plot volume to be shown
;         skirt draws lines connecting the data line with these
;                     projections at n points, a good value 
;                     would be skirt=30
;         b_lines sets the number of evenly spaced lines drawn on 
;                     the back panels. When omitted, lines will be drawn 
;                     at tickmarks. 
;         also accepts the same keywords as routine surface (e.g. ax,
;                     ay, and az to specify the 3-D viewpoint)
;
; Note: 
;    With IDL version 4.0, the IDL supplied routine PLOT_3DBOX might
;    be a better choice....
;-

if n_params() ne 3 then begin
  x=reform(px(0,*))
  y=reform(px(1,*))
  z=reform(px(2,*))
endif else begin
  x=px
  y=py
  z=pz
endelse

n=n_elements(z)
; set up a temporary 2-D variable
pp=fltarr(n,n)
pp(1,*)=z
surface, pp, x, y, /nodata, /save, ztick_get=ztick, _extra=_extra

max_x=!x.crange(1)
min_x=!x.crange(0)
max_y=!y.crange(1)
min_y=!y.crange(0)
max_z=!z.crange(1)
min_z=!z.crange(0)

; draw a box with the back panels marked by dotted lines
plots, [max_x,max_x], [max_y,max_y], [min_z,max_z], /t3d, line=1
plots, [max_x,max_x,min_x], [min_y,min_y,min_y], [min_z,max_z,max_z], $
   /t3d, line=1
plots, [min_x,min_x,min_x], [min_y,min_y,max_y], [min_z,max_z,max_z], $
   /t3d, line=1
xx=[min_x, max_x, max_x]
yy=[max_y, max_y, min_y]
zz=[min_z, min_z, min_z]
if keyword_set(b_lines) then $
  for i=0,b_lines do plots, xx, yy, $
      zz+i*(max_z-min_z)/float(b_lines), /t3d, line=1 $
else begin
    for i=0,1 do plots, xx, yy, zz+i*(max_z-min_z), /t3d, line=1
    for i=0,n_elements(ztick)-1 do plots, xx, yy, ztick(i), /t3d, line=1
endelse

; plot the data
plots, x, y, z, /t3d

if keyword_set(projection) then begin
  plots, x, y, fltarr(n)+min_z, /t3d, line=2
  plots, x, fltarr(n)+max_y, z, /t3d, line=2
  plots, fltarr(n)+max_x, y, z, /t3d, line=2
endif

if keyword_set(skirt) then begin
  for i=0,n-1,n/skirt do $
     plots, [x(i),x(i)], [y(i),y(i)], [z(i),min_z], /t3d
  for i=0,n-1,n/skirt do $
     plots, [x(i),x(i)], [y(i),max_y], [z(i),z(i)], /t3d
endif

return
end
