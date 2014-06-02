pro time_axis, time, xtitle=xtitle, reset=reset, xrange=xrange, $
               steps=steps, minor=minor, debug=debug
;+
; NAME:
;          time_axis
;
; PURPOSE:
;          define x-axis as time axis with nice labels etc 
;
; CALLING SEQUENCE:
;          time_axis, time, ...
;
; INPUTS:
;          time: an array of time in decimal hours
;
; KEYWORD PARAMETERS:
;          /reset: reset axis setting to default.  After time_axis has
;              been called, every subsequent plotting routine will use the
;              x-axis defined here, unless one or more of the set parameters
;              are overwritten by plotting keywords (which take
;              precedence) or until time_axis is called again with the
;              /reset keyword.
;          xtitle: title for x-axis (default: ''). This can
;              also be changed with the xtitle keyword on the plotting
;              routine. 
;          xrange=[time_min, time_max]: specify a plotting range
;              between time_min and time_max.  Note that this sets the
;              time range to the next "nice" time limits.  The
;              equivalent of xstyle=1 is not yet implemented.
;          steps: (also requires minor) set the major and minor tickmark
;              intervals.  Step defines the time between labels (in hours)
;              and minor defines the number of intervals between the
;              labeled major tickmarks.  If these are not set, the program
;              selects sensible values itself.  For example, stting step=1
;              and minor=2 give a label every hour, and a tickmark every
;              30 minutes.
;
; RESTRICTIONS:
;          The array time must increase monotonically.  E.g., if more than
;          one day of data is plotted, define time by:
;              time=daynumber*24 + hour + minute/60. + second/3600.
;          where daynumber is a suitable daynumber, and hour minute
;          etc is the time of the day.
;         
; EXAMPLE:
;          time=findgen(121)/30. + 22.  ; make an array with values
;                                         from 22.0 to 26.0 
;          time_axis, time
;          plot, time, findgen(121), ytitle='test values'
;          time_axis, /reset
;
;          this makes a plot with x-axis lables starting at 18:00, and
;          going past 22:00 to 2:00.
;
; HISTORY:
;
;       Fri Sep 15 19:22:37 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;-

; return to user if an error occurred
if not keyword_set(debug) then on_error, 2 

; reset !x to the default
if keyword_set(reset) then begin
    !x.tickv=0.
    !x.minor=0.
    !x.ticks=0.
    !x.range=0.
    !x.style=0
    !x.tickname=''
    !x.title=''
    return
endif

; set up time axis
if keyword_set(xrange) then begin
    dummy=min(abs(time-xrange(0)), n0)
    dummy=min(abs(time-xrange(1)), nn)
endif else begin
    n0=0
    nn=n_elements(time)-1
endelse

dtime=max(time(n0:nn))-min(time(n0:nn))    ; range in hours

; set up tick mark intervals to give a nice appearance
if not(keyword_set(steps) and keyword_set(minor)) then begin
   if dtime gt 5*24 then begin & steps=24. & minor=2 & endif
   if 24 lt dtime and dtime le 5*24 then begin & steps=12. & minor=3 & endif
   if 12 lt dtime and dtime le 24 then begin & steps=4. & minor=4 & endif
   if 8 lt dtime and dtime le 14 then begin & steps=2. & minor=4 & endif
   if 4 lt dtime and dtime le 8 then begin & steps=1. & minor=4 & endif
   if 2 lt dtime and dtime le 4 then begin & steps=0.5 & minor=3 & endif
   if dtime le 2 then begin & steps=.25 & minor=3 & endif
endif

; define and set tickmarks
first_tick=fix(time(n0)/steps) * steps
last_tick=fix((time(nn) - 1e-2*steps)/steps) * steps+steps
nticks=fix((last_tick-first_tick)/steps) + 1            ; major tickmarks
labels=findgen(nticks)*steps + first_tick
if keyword_set(debug) then print, 'first, last, n:', first_tick, $
  last_tick, nticks 
if steps ge 1 then !x.tickname=strcompress(string(fix(labels mod 24))) $
              else !x.tickname=hhmm(labels mod 24) 
                 ; other idea: string((labels mod 24),format='(f5.2)')

; if the last tick mark turns out to be 24, write "24" or "24:00"
; instead of "0" or "0:00"
if (last_tick mod 24) eq 0 then begin
    if steps ge 1 then !x.tickname(nticks-1)='24' $
                  else !x.tickname(nticks-1)=hhmm(24) 
endif

!x.tickv=labels
!x.minor=minor
!x.ticks = nticks - 1
!x.range=[time(n0),time(nn)]
!x.style=1
if keyword_set(xtitle) then !x.title=xtitle else !x.title=''

if keyword_set(debug) then stop
return
end
