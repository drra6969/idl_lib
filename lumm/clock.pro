pro clock, hour, minute, xpos, ypos, size=size

; display a clock showing the given time
; this routine works in device coordiantes, thus xpos, ypos define
; the position and size gives the diameter of the plotted clock in
; device coordinates.

if not keyword_set(size) then size=50

; make tickmarks

ticks=indgen(12)
for i=0,11 do begin
   y=[0.5*cos(ticks(i)/12.*2.*!pi)+.5, .47*cos(ticks(i)/12.*2.*!pi)+.5]* $
        size+ypos
   x=[0.5*sin(ticks(i)/12.*2.*!pi)+.5, .47*sin(ticks(i)/12.*2.*!pi)+.5]* $
        size+xpos
   plots, x, y, thick=4, /dev
endfor

; hour hand first

rh=.3
y=rh*cos((hour+minute/60.)/12.*2.*!pi)+.5
x=rh*sin((hour+minute/60.)/12.*2.*!pi)+.5

plots, [.5,x]*size+xpos, [.5,y]*size+ypos, thick=4, /dev

; now the minute hand

rm=.45
y=rm*cos(minute/60.*2.*!pi)+.5
x=rm*sin(minute/60.*2.*!pi)+.5

plots, [.5,x]*size+xpos, [.5,y]*size+ypos, thick=2, /dev

return
end





