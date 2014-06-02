
PRO VECT,U,V,X,Y, Missing = Missing, Length = length, Dots = dots,  $
        Title = title, position=position, noerase=noerase, color=color, $
        Sizerat = sizerat
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
;	yoff:  offset for y vectors.  use to get arrows off endlines.
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
;        u(1,19) = 2.    & v(1,19)=0.
;        u(3,19) = 1.414 & v(3,19)=1.414 
;        u(5,19) = 0.    & v(5,19)=2. 
        
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
        if n_elements(sizerat) le 0 then  sizerat = 1.0
        szsq=sizerat^2

        mag = sqrt(u^2+v^2)             ;magnitude.
                ;Subscripts of good elements
        nbad = 0                        ;# of missing points
        if n_elements(missing) gt 0 then begin
                good = where(mag lt missing) 
                if keyword_set(dots) then bad = where(mag ge missing, nbad)
        endif else begin
                good = lindgen(n_elements(mag))
        endelse

        mag = mag(good)       ;Discard missing values
        withp = where(mag ge 0.000001)
        maxmag = max(mag)
        ugood = u(good)
        vgood = v(good)
;        projv = mag
;        projv(*) = 1.
;        projv(withp) = mag(withp) $
;                        /sqrt(ugood(withp)^2*szsq+vgood(withp)^2)
        x0 = min(x)                     ;get scaling
        x1 = max(x)
        delx=x1-x0
        y0 = min(y)
;           -0.7*(max(y)-min(y))/float(s(2)-1)
        y1 = max(y)
;           +0.7*(max(y)-min(y))/float(s(2)-1)
        dely=y1-y0
        xzuy=sqrt(delx/dely)
        maxdgrid=max([(x1-x0)/s(1),(y1-y0)/s(2)])
        
        delnx=(!P.position(2)-!P.position(0))*!D.x_vsize
        delny=(!P.position(3)-!P.position(1))*!D.y_vsize
        frat=dely/delx/delny*delnx
        fratt=frat^2
        print,'delnx:',delnx,'  delny:',delny
        print,'delx:',delx,'  dely:',dely
        print,'pixelratio:', delny/delnx
        print,'sizeratio:', dely/delx
        print,'sizeratio/pixelratio:', frat
        
        sina = length * maxdgrid/maxmag*ugood ;sin & cosine components.
        cosa = length * maxdgrid/maxmag*vgood
        if fratt le 1. then abslen0 = sqrt(sina^2+cosa^2/fratt)
        if fratt gt 1. then abslen0 = sqrt(fratt*sina^2+cosa^2)
        abslen1 = sqrt(sina^2+cosa^2)
        sina = sina*abslen1/abslen0
        cosa = cosa*abslen1/abslen0
;        perpendicular direction
        siperp =  vgood/frat
        coperp = -ugood*frat
        if fratt le 1. then absperp0 = sqrt(siperp^2+coperp^2/fratt)
        if fratt gt 1. then absperp0 = sqrt(fratt*siperp^2+coperp^2)
        absperp1 = sqrt(siperp^2+coperp^2)
        siperp = 0.18*siperp/absperp0*abslen1
        coperp = 0.18*coperp/absperp0*abslen1

;
        if n_elements(title) le 0 then title = ''
        ;--------------  plot to get axes  ---------------
        if n_elements(color) eq 0 then color = !p.color
        if n_elements(position) eq 0 then begin
          plot,[x1,x0],[y1,y0],/nodata,/xst,/yst,title=title, $
            noerase=noerase, color=color
        endif else begin
          plot,[x1,x0],[y1,y0],/nodata,/xst,/yst,title=title, $
            noerase=noerase, color=color, position=position
        endelse
;
;        r = .3 
        r = .4                           ;len of arrow head
        ad=0.25                         
;        angle = 22.5 * !dtor   
        angle = 20.0 * !dtor   
        print, angle             ;Angle of arrowhead
        st = r * sin(angle)             ;sin 22.5 degs * length of head
        ct = r * cos(angle)
;
        for i=0,n_elements(good)-1 do begin     ;Each point
                x0 = x(good(i) mod s(1))        ;get coords of start & end
                dx = sina(i) & dxp = siperp(i)
                x1 = x0 + dx
                y0 = y(good(i) / s(1))
                dy = cosa(i) & dyp = coperp(i)
                y1 = y0 + dy
                if (y0 lt max(y)) then $
                 plots, $
                    [x0,x1,(x1-r*dx+dxp),x1,(x1-r*dx-dxp)], $
                    [y0,y1,(y1-r*dy+dyp),y1,(y1-r*dy-dyp)], $
;                    [x0,x1,x1-(ct*dx-st*dy),x1,x1-(ct*dx+st*dy)], $
;                    [y0,y1,y1-(ct*dy+st*dx),y1,y1-(ct*dy-st*dx)], $
                      color=color
                endfor
        dx = (max(x)-min(x))/s(1)
  if 1.1*abs(!x.range(1)-!x.range(0)) le abs(!y.range(1)-!y.range(0)) then begin
        if !x.range(1) gt !x.range(0) then x0 = max(x)+0.9*dx
        if !x.range(1) le !x.range(0) then x0 = min(x)-0.9*dx
        dx0=dx
        x1 = x0
        dx=x1-x0
        dy = (max(y)-min(y))/s(2)
        y0 = max(y)-1.3*dy
        dy = length*maxdgrid
        y1 = y0 + dy
        plots,[x0,x1,x1-r*dx+ad*dy*xzuy,x1,x1-r*dx-ad*dy*xzuy], $
              [y0,y1,y1-r*dy-ad*dx/xzuy,y1,y1-r*dy+ad*dx/xzuy], $
               color=color
        if !x.range(1) gt !x.range(0) then $
   	   xyouts,charsize=1.0,x0+0.4*dx0,y0+0.2*dy,'='
        if !x.range(1) le !x.range(0) then $
   	   xyouts,charsize=1.0,x0-0.6*dx0,y0+0.2*dy,'='
        form='(f4.2)'
        if abs(maxmag) gt 10. then form='(f4.1)'
        if abs(maxmag) gt 100. then form='(f4.0)'
        if abs(maxmag) gt 1000. then form='(f5.0)'
   	xyouts,charsize=1.0,x0-0.4*dx0,y0-0.8*dy,string(maxmag,form)
  endif 
  if 1.1*abs(!x.range(1)-!x.range(0)) gt abs(!y.range(1)-!y.range(0)) then begin
        if !x.range(1) gt !x.range(0) then x0 = max(x)-0.9*dx
        if !x.range(1) le !x.range(0) then x0 = min(x)+0.9*dx
        dx0=dx & dx = length*maxdgrid
        x1 = x0+dx
        dy = (max(y)-min(y))/s(2) & dy0=dy
       if !y.range(1) gt !y.range(0) then y0 = max(y)+1.*dy
       if !y.range(1) lt !y.range(0) then y0 = min(y)-1.*dy
;       print, max(y), min(y), dy, y0
        dy = 0.0
        y1 = y0 
        plots,[x0,x1,x1-r*dx+ad*dy*xzuy,x1,x1-r*dx-ad*dy*xzuy], $
              [y0,y1,y1-r*dy-ad*dx/xzuy,y1,y1-r*dy+ad*dx/xzuy], $
               color=color
        form='(f4.2)'
        if abs(maxmag) gt 10. then form='(f4.1)'
        if abs(maxmag) gt 100. then form='(f4.0)'
        if abs(maxmag) gt 1000. then form='(f5.0)'
        if !x.range(1) gt !x.range(0) then $
   	  xyouts,charsize=1.0,x1+0.2*dx,y0-0.2*dy0,'= '+string(maxmag,form)
        if !x.range(1) le !x.range(0) then $
   	  xyouts,charsize=1.0,x0-0.5*dx0,y0-0.2*dy0,'= '+string(maxmag,form)
  endif 

        if nbad gt 0 then $             ;Dots for missing?
                oplot, x(bad mod s(1)), y(bad / s(1)), psym=3, color=color

        
end
