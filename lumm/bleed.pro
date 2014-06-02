function bleed, array, value=value, delta_value=delta_value,  $
                width=width, strength=strength

; function to bleed pixel values into thier immediate vicinity
; use as:
;     array_out=bleed(array_in)
; keywords:
;     value: gives the value to be bleeded (default=0)
;     delta_value: pixels with value +- delta_value are bleeded (default=0)
;     width: width (in pixels) of the extend of the bleed (default: 10)
;     strength: persistence of the bleeding - strongest bleeding for +1,
;               strength=0 causes no bleeding at all

; set default values
if not keyword_set(value) then value=0
if not keyword_set(delta_value) then delta_value=0
if not keyword_set(width) then width=10
if not keyword_set(strength) then test = 1  $
                             else test = min([(strength > 0), 1])

; determine which points are currently flagged
flag=where(abs(array-value) le delta_value, flag_ct)
good=where(abs(array-value) gt delta_value, good_ct)

; if none or all are flagged, return the original array
if flag_ct le 0 or good_ct le 0 then return, array

; make a new array so that the input array doesn't get altered
new_array=array

; set flagged values to a negative number, good points just above 
; zero, so that the smoothing `pulls' the zero line into the good point area
new_array(flag)=-1
new_array(good)=+1

; make an array that contains the input array in its center
nn=size(array)
if nn(0) ne 2 then begin
	print, 'array must have 2 dimensions'
	return, array
endif
temp=fltarr(nn(1)+2*width,nn(2)+2*width)
temp(width:nn(1)+width-1, width:nn(2)+width-1)=new_array

; fill the extra borders of the temporary array
for i=0,width-1 do temp(i,*)=temp(width,*)
for i=nn(1)+width, nn(1)-1+2*width do temp(i,*)=temp(nn(1)+width-1,*)
for i=0,width-1 do temp(*,i)=temp(*,width)
for i=nn(2)+width, nn(2)-1+2*width do temp(*,i)=temp(*,nn(2)+width-1)

; now smooth the array
temp=smooth(temp, 2*width)

; and extract the center portion of the array again
new_array=temp(width:nn(1)+width-1, width:nn(2)+width-1)

; and set the flagged points to the original flag-value again
flag=where(new_array lt test, flag_ct)
good=where(new_array ge test, good_ct)

; set flagged pixels to the flag value, and good pixels to their 
; original value
if flag_ct gt 0 then new_array(flag)=value
if good_ct gt 0 then new_array(good)=array(good)

return, new_array
end

