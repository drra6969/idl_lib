function doy_mmdd, doy=doy, year, month, day

;+
; NAME: 
;     doy_mmdd
;
; Purpose:
;     convert day-month to doy or reverse
; 
; use as:
;     doy=doy_mmdd(1994, 10, 13) 
;          to get day of year (in this case 13 Oct 1994 -> 286)
; or use as:
;     date=doy_mmdd(1994, doy=286) 
;          to get an array date=[year, month, day] 
;          (here date=[1994, 10, 13])
;-

; number of days in the months of the year
mon=[31,28,31,30,31,30,31,31,30,31,30,31]

; find out whether we are in a leap year and modify Febuary accordingly
if (year mod 4) eq 0 then mon(1)=29
if (year mod 100) eq 0 then mon(1)=28
if (year mod 1000) eq 0 then mon(1)=29

if not keyword_set(doy) then begin

;     find doy from month and day
    if month gt 1 then $
       return, fix(total(mon(0:month-2))) + day $
    else return, day
endif else begin

;     find month and day of month from doy
    month=0
    day=doy
    while day gt 0 do begin
        day=day-mon(month)
        month=month+1
    endwhile
;     day is negative after this and needs the last month added back on
    day=day+mon(month-1)
    return, [year,month,day]
endelse

end
