; $Id: velovect.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

PRO VELOVECT,U,V,X,Y, Missing = Missing, Length = length, Dots = dots,  $
        Title = title, position=position, noerase=noerase, color=color
;
;+
; NAME:
;	VELOVECT
;
; PURPOSE:
;	Produce a two-dimensional velocity field plot.
;
;	A directed arrow is drawn at each point showing the direction and 
;	magnitude of the field.
;               
; CATEGORY:
;	Plotting, two-dimensional.
;
; CALLING SEQUENCE:
;	VELOVECT, U, V [, X, Y]
;
; INPUTS:
;	U:	The X component of the two-dimensional field.  
;		U must be a two-dimensional array.
;
;	V:	The Y component of the two dimensional field.  Y must have
;		the same dimensions as X.  The vector at point (i,j) has a 
;		magnitude of:
;
;			(U(i,j)^2 + V(i,j)^2)^0.5
;
;		and a direction of:
;
;			ATAN2(V(i,j),U(i,j)).
;
; OPTIONAL INPUT PARAMETERS:
; 	X:	Optional abcissae values.  X must be a vector with a length 
;		equal to the first dimension of U and V.
;
;	Y:	Optional ordinate values.  Y must be a vector with a length
;		equal to the first dimension of U and V.
;
; KEYWORD INPUT PARAMETERS:
;      MISSING:	Missing data value.  Vectors with a LENGTH greater
;		than MISSING are ignored.
;
;	LENGTH:	Length factor.  The default of 1.0 makes the longest (U,V)
;		vector the length of a cell.
;
;	DOTS:	Set this keyword to 1 to place a dot at each missing point. 
;		Set this keyword to 0 or omit it to draw nothing for missing
;		points.  Has effect only if MISSING is specified.
;
;	TITLE:	A string containing the plot title.
;
;     POSITION:	A four-element, floating-point vector of normalized 
;		coordinates for the rectangular plot window.
;		This vector has the form  [X0, Y0, X1, Y1], where (X0, Y0) 
;		is the origin, and (X1, Y1) is the upper-right corner.
;
;      NOERASE:	Set this keyword to inhibit erase before plot.
;
;	COLOR:	The color index used for the plot.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Plotting on the selected device is performed.  System
;	variables concerning plotting are changed.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Straightforward.  The system variables !XTITLE, !YTITLE and
;	!MTITLE can be set to title the axes.
;
; MODIFICATION HISTORY:
;	DMS, RSI, Oct., 1983.
;
;	For Sun, DMS, RSI, April, 1989.
;
;	Added TITLE, Oct, 1990.
;
;	Added POSITION, NOERASE, COLOR, Feb 91, RES.
;-
;
        on_error,2                      ;Return to caller if an error occurs
        s = size(u)
        t = size(v)
        if s(0) ne 2 then begin 
baduv:   message, 'U and V parameters must be 2D and same size.'
                endif
        if total(abs(s(0:2)-t(0:2))) ne 0 then goto,baduv
;
        if n_params(0) lt 3 then x = findgen(s(1)) else $
                if n_elements(x) ne s(1) then begin
badxy:                  message, 'X and Y arrays have incorrect size.'
                        endif
        if n_params(1) lt 4 then y = findgen(s(2)) else $
                if n_elements(y) ne s(2) then goto,badxy
;
        if n_elements(missing) le 0 then missing = 1.0e30
        if n_elements(length) le 0 then length = 1.0

        mag = sqrt(u^2+v^2)             ;magnitude.
                ;Subscripts of good elements
        nbad = 0                        ;# of missing points
        if n_elements(missing) gt 0 then begin
                good = where(mag lt missing) 
                if keyword_set(dots) then bad = where(mag ge missing, nbad)
        endif else begin
                good = lindgen(n_elements(mag))
        endelse

        mag = mag(good)                 ;Discard missing values
        maxmag = max(mag)
        ugood = u(good)
        vgood = v(good)
        x0 = min(x)                     ;get scaling
        x1 = max(x)
        y0 = min(y)
        y1 = max(y)
        sina = length * (x1-x0)/s(1)/maxmag*ugood ;sin & cosine components.
        cosa = length * (y1-y0)/s(2)/maxmag*vgood
;
        if n_elements(title) le 0 then title = ''
        ;--------------  plot to get axes  ---------------
        if n_elements(color) eq 0 then color = !p.color
        if n_elements(position) eq 0 then begin
          plot,[x0-1.,x1+1.],[y1+1.,y0-1.],/nodata,/xst,/yst,title=title, $
            noerase=noerase, color=color
        endif else begin
          plot,[x0-1.,x1+1.],[y1+1.,y0-1.],/nodata,/xst,/yst,title=title, $
            noerase=noerase, color=color, position=position
        endelse
;
        r = .3                          ;len of arrow head
        angle = 22.5 * !dtor            ;Angle of arrowhead
        st = r * sin(angle)             ;sin 22.5 degs * length of head
        ct = r * cos(angle)
;
        for i=0,n_elements(good)-1 do begin     ;Each point
                x0 = x(good(i) mod s(1))        ;get coords of start & end
                dx = sina(i)
                x1 = x0 + dx
                y0 = y(good(i) / s(1))
                dy = cosa(i)
                y1 = y0 + dy
                plots,[x0,x1,x1-(ct*dx-st*dy),x1,x1-(ct*dx+st*dy)], $
                      [y0,y1,y1-(ct*dy+st*dx),y1,y1-(ct*dy-st*dx)], $
                      color=color
                endfor
        if nbad gt 0 then $             ;Dots for missing?
                oplot, x(bad mod s(1)), y(bad / s(1)), psym=3, color=color
end
