pro image, a, x, y, oo=oo, io=io, oi=oi, xstyle=xstyle, ystyle=ystyle, $
	title=title, xtitle=xtitle, ytitle=ytitle, units=units
;+
; NAME:
;       image (old version)
;
; PURPOSE:
;       Make a image of an irregular spaced data array
;
;       image, a, x, y, oo=oo, io=io, oi=oi, xstyle=xstyle, ystyle=ystyle, $
;	       title=title, xtitle=xtitle, ytitle=ytitle, units=units
;
;       plot the data in array a as an image type display with
;       x,y giving the positions of the array elements in a.  
;       A colorbar is plotted on the right side, showing the
;       range of data in array a.  The keyword "units" is the
;       text to be plotted next to the colorbar.  Keywords
;       title, xtitle, ytitle, xstyle, and ystyle go with the image.
;       Keywords oo, oi, io specify the log-log, log-lin, lin-log
;       combinations for x-y axis. 
;-
; first check whether the iput arrays are compatible
is=size(a)
if is(0) ne 2 then begin
    print, 'first argument must be 2-dim array'
    return
endif
i=is(1)
j=is(2)
is=size(x)
if is(1) ne i then begin
    print, 'X has wrong dimension'
    return
endif
is=size(y)
if is(1) ne j then begin
    print, 'Y has wrong dimension'
    return
endif

; scale the data to the number of available colors
c=float(a-min(a))/float(max(a)-min(a)) * !d.n_colors

; set up xx, yy arrays with the corner coordinates of boxes
; such that the boxes are centered on the data points
xx=fltarr(i+1)
yy=fltarr(j+1)
xx(0)=x(0)-(x(1)-x(0))/2.
xx(i)=x(i-1)+(x(i-1)-x(i-2))/2.
xx(1:i-1)=(x(0:i-2)+x(1:i-1))/2.
yy(0)=y(0)-(y(1)-y(0))/2.
yy(j)=y(j-1)+(y(j-1)-y(j-2))/2.
yy(1:j-1)=(y(0:j-2)+y(1:j-1))/2.

; set aside some space for a colorbar
!p.region=[0.,0.,.8,1.]

; find out which keywords are set and take appropriate action
if not keyword_set(xstyle) then xstyle=1
if not keyword_set(ystyle) then ystyle=1
if not keyword_set(title) then title=' '
if not keyword_set(xtitle) then xtitle=' '
if not keyword_set(ytitle) then ytitle=' '

; set up plotting area and draw axis
if keyword_set(oo) then begin
    if xx(0) le 0 then xx(0)=x(0)/2.
    if yy(0) le 0 then yy(0)=y(0)/2.
    plot_oo, [xx(0), xx(i)], [yy(0), yy(j)], /nodata, $
             xsty=xstyle, ysty=ystyle  
endif else begin
    if keyword_set(io) then begin
        if yy(0) le 0 then yy(0)=y(0)/2.
        plot_io, [xx(0), xx(i)], [yy(0), yy(j)], /nodata, $
                 xsty=xstyle, ysty=ystyle
    endif else begin
        if keyword_set(oi) then begin
            if xx(0) le 0 then xx(0)=x(0)/2.
            plot_oi, [xx(0), xx(i)], [yy(0), yy(j)], /nodata, $
                     xsty=xstyle, ysty=ystyle
        endif else $
          plot, [xx(0), xx(i)], [yy(0), yy(j)], /nodata, $
                xsty=xstyle, ysty=ystyle
    endelse
endelse

; fill in the "image" area with color-coded rectangles....
for n=0,i-1 do begin
    for m=0,j-1 do begin
        polyfill, [xx(n),xx(n+1),xx(n+1),xx(n),xx(n)], $
	      [yy(m),yy(m),yy(m+1),yy(m+1),yy(m)], color=c(n,m)
    endfor
endfor

; now plot the axis again....  it looks better that way, though it seems
; to be a waste of programming!
if keyword_set(oo) then $
  plot_oo, [xx(0), xx(i)], [yy(0), yy(j)], /nodata, /noerase, title=title, $
	   xsty=xstyle, ysty=ystyle, ytitle=ytitle, xtitle=xtitle $
else if keyword_set(io) then $
  plot_io, [xx(0), xx(i)], [yy(0), yy(j)], /nodata, /noerase, title=title,  $
	   xsty=xstyle, ysty=ystyle, ytitle=ytitle, xtitle=xtitle  $
else if keyword_set(oi) then $
  plot_oi, [xx(0), xx(i)], [yy(0), yy(j)], /nodata, /noerase, title=title,  $
	   xsty=xstyle, ysty=ystyle, ytitle=ytitle, xtitle=xtitle $
else plot, [xx(0), xx(i)], [yy(0), yy(j)], /nodata, /noerase, title=title,  $
           xsty=xstyle, ysty=ystyle, ytitle=ytitle, xtitle=xtitle


; now comes the color bar
!p.region=[.8,.1,1.,.9]

; findout if "units"-text is to be plotted
if not keyword_set(units) then units=' '

; area of color bar
xc=[0.,1.]
yc=[min(a), max(a)]

; plot the axis to set up the coordinate system
plot, xc, yc, /noerase, /nodata, yticklen=1., xticks=1, $
      xsty=1, ysty=1, xtickname=[' ',' '], xminor=1, $
      xtitle=' ', ytitle=units, title=' ', xtickv=[0,1], $
      xrange=[0,1]

; c is colorindex, yy is position where this color goes
c=findgen(!d.n_colors)
yy=findgen(!d.n_colors+1)/!d.n_colors*(max(a)-min(a))+min(a)
for n=0,!d.n_colors-1 do $
  polyfill, [0., 1., 1., 0., 0.], $
	    [yy(n),yy(n),yy(n+1),yy(n+1),yy(n)], color=c(n)

; plot the axis again to make it look better
plot, xc, yc, /noerase, /nodata, yticklen=1., xticks=1, $
      xsty=1, ysty=1, xtickname=[' ',' '], xminor=1, $
      xtitle=' ', ytitle=units, title=' ', xtickv=[0,1], $
      xrange=[0,1]

; reset defaults settings
!p.region=0

return
end
