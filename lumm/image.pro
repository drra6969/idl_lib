pro image, a, x, y, units=units, zlog=zlog, zmin=zmin, zmax=zmax, $
           space=space, nocolorbar=nocolorbar, reverse=reverse, $
           _extra=_extra
;+
; NAME:
;       image
;
; PURPOSE:
;       Make an image of an unevenly (but regular) spaced data array
;
;       Plot the data in array 'data' as an image type display with
;       x,y giving the positions of the array elements in 'data'.  
;       A colorbar is plotted on the right side, showing the
;       range of data in array 'data'.  The keyword "units" is the
;       text to be plotted next to the colorbar.  Other 
;       graphics keywords are also accepted (not all of them
;       gracefully, though).
;
; CALLING SEQUENCE:
;       image, data, x, y, units=units, zlog=zlog, ...
;
; INPUTS:
;       data: 2-D data array, data(x,y)
;       x:    1-D array of x-values
;       y:    1-D array of y-values
;
; KEYWORD PARAMETERS:
;       units=units: specify text to be shown next to the color bar
;       zlog: set zlog to make a log plot of the array (log axes should be 
;             specified by /xlog and/or /ylog)
;       zmin, zmax: specify to force minimum and maximum value for the
;             colorscale.  When not specified, the scaling is selected
;             to range from min(data) to max(data).
;       nocolorbar: omit the colorbar
;       reverse: set this keyword to reverse the grayscale (or
;             colorscale).  Normally, maxima are white (with b/w
;             grayscale), minima are black.  Setting /reverse turns
;             this the other way around.
;       other graphics keywords: [xy]style, [xy]log, [xy]title, title. 
;             Do not use keywords [xy]range, as they produce strange
;             results until RSI fixes up their polyfill routine.
;             Instead, limit the arrays to the desired range, and set
;             [xy]style=1. See example.
;       space: extra space between colorbar and main plot (normal
;             units). May be positive or negative....
;
; RESTRICTIONS:
;         This program works best with small data arrays.  Large
;         arrays (more than 200x200 or so) are much better plotted
;         with one of the tv or tvscl routines.  This routine is not
;         very fast when it has to deal with large arrays.
;
;         The x and y axes are slightly bigger than the min and max of
;         x and y respectively.  This gives problems if used in
;         conjunction with a call to time_axis.  To avoid any such
;         problems make the call to time_axis as follows:
;         time_axis, [x(0)-(x(1)-x(0))/2., x, x(i-1)+(x(i-1)-x(i-2))/2.]
;         where i=n_elemsnts(x), and then call image.
;
;         The label on the colorbar (specified by keyword unit) may
;         occasionally overlap into the "image" plot itself.  If this
;         happens set the keyword space to a value larger than 0.
;         This causes the spacing between the colorbar and the main
;         plot to increase (the units for space are normal, values for
;         space between 0.01 and 0.05 are usually sufficient).
;
;         Using !p.multi to make several plots works fine as long as
;         only 1 column is plotted.  The colorbar messes things up for
;         multiple column plots (i.e. make sure !p.multi(1)=1.
;
; EXAMPLE:
;        image, dist(20,30), findgen(20), (findgen(30)+1)^2, $
;               units='Test', ystyle=1, /ylog
;
;        plot limited in x-values:
;        IDL> data=dist(40,30) & x=(findgen(40)+1)^2
;        IDL> i=where(15 le x and x le 185)
;        IDL> image, data(i,*), x(i), findgen(30), xsty=1
;
; MODIFICATION HISTORY:
;
;       Fri Oct 20 15:51:11 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;       Fri Sep 15 17:38:51 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;               
;-

on_error, 2
if n_elements(_extra) gt 0 then tags=string(tag_names(_extra), /print) $
                           else tags=''

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
if n_elements(zmin) eq 0 then zmin=min(a)
if n_elements(zmax) eq 0 then zmax=max(a)
; for b/w screens (!d.n_color=2) use line-spacing instead of color
if !d.n_colors gt 2 then ncolors=!d.n_colors else ncolors=!d.y_px_cm/2+1
if keyword_set(zlog) then begin
    if min(a, amin) le 0 then begin
        print, 'For log plots, the data must be positive!'
        print, 'data array(',amin,') = ',min(a)
        return
    endif
    c=float(alog10(a)-alog10(zmin))/float(alog10(zmax)-alog10(zmin)) $
      * (ncolors-1)
endif else c=float(a-zmin)/float(zmax-zmin) * (ncolors-1)
if keyword_set(reverse) then c=abs(c-(ncolors-1))

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

; set aside some space for a colorbar. Save the system variables that
; are changed to reset them before returning.  If 3 or more plots are
; made with !p.multi, the !d.x_ch_size is apparently modified without
; telling anyone on the outside world - thus the scale factor. Maybe
; there is a smarter way of doing this....
if not keyword_set(space) then space=0
save_xmargin=!x.margin
save_pmulti=!p.multi(0)
if !p.multi(2) ge 3 then scale=2 else scale=1
!x.margin=[10,(0.16+space)*!d.x_size/!d.x_ch_size]*scale

; set up plotting area and draw axis
if (strpos(tags,'XLOG') ge 0) and xx(0) le 0 then xx(0)=x(0)/2.
if (strpos(tags,'YLOG') ge 0) and yy(0) le 0 then yy(0)=y(0)/2.
plot, [xx(0), xx(i)], [yy(0), yy(j)], /nodata, _extra=_extra

; undo the frame advance for the case of !p.multi non zero
next_pmulti=!p.multi(0)
!p.multi(0)=save_pmulti

; fill in the "image" area with color-coded rectangles....
; for b/w screens (!d.n_color=2) use line-spacing instead of color
for n=0,i-1 do begin
    for m=0,j-1 do begin
        if !d.n_colors le 2 then key={line_fill:1, spacing:1./(c(n,m)+.01)} $
                           else key={color:c(n,m)}
        polyfill, [xx(n),xx(n+1),xx(n+1),xx(n),xx(n)], $
	      [yy(m),yy(m),yy(m+1),yy(m+1),yy(m)], _extra=key
    endfor
endfor

; now plot the axis again....  it looks better that way, though it seems
; to be a waste of programming!
plot, [xx(0), xx(i)], [yy(0), yy(j)], /nodata, /noerase, _extra=_extra

; now comes the color bar 
if keyword_set(nocolorbar) then begin       ; reset defaults settings
    !p.multi(0)=next_pmulti
    !x.margin=save_xmargin
    return
endif
!x.margin=[0.93*!d.x_size/!d.x_ch_size, 3]*scale

; findout if "units"-text is to be plotted
if not keyword_set(units) then units=''

; area of color bar
xc=[0.,1.]
yc=[zmin, zmax]

; plot the axis to set up the coordinate system
if keyword_set(zlog) then ylog=1 else ylog=0
plot, xc, yc, /noerase, /nodata, yticklen=1., xticks=1, $
      xsty=1, ysty=1, xtickname=[' ',' '], xminor=1, $
      xtitle='', ytitle=units, title='', xtickv=[0,1], $
      xrange=[0,1], ylog=ylog

; c is colorindex, yy is position where this color goes
c=findgen(ncolors)
if keyword_set(reverse) then c=abs(c-(!d.n_colors-1))
if keyword_set(zlog) then $
       yy=10^(findgen(ncolors+1)/ncolors* $
          (alog10(zmax)-alog10(zmin))+alog10(zmin)) $
  else yy=findgen(ncolors+1)/ncolors*(zmax-zmin)+zmin
for n=0,ncolors-1 do begin
    if !d.n_colors le 2 then key={line_fill:1, spacing:1./(c(n)+.01)} $
                       else key={color:c(n)}
    polyfill, [0., 1., 1., 0., 0.], $
	    [yy(n),yy(n),yy(n+1),yy(n+1),yy(n)], _extra=key
endfor

; plot the axis again to make it look better
plot, xc, yc, /noerase, /nodata, yticklen=1., xticks=1, $
      xsty=1, ysty=1, xtickname=[' ',' '], xminor=1, $
      xtitle='', ytitle=units, title='', xtickv=[0,1], $
      xrange=[0,1], ylog=ylog

; reset defaults settings
!p.multi(0)=next_pmulti
!x.margin=save_xmargin

return
end
