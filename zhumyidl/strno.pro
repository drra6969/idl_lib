FUNCTION strno, a

no=1 & b=strtrim(a) & length=strlen(b)
for i=0,length-1 do begin
   sub=strmid(b, i,1)
   if ( (sub ne '0') and (sub ne '1') and (sub ne '2') and (sub ne '3') $
        and (sub ne '4') and (sub ne '5') and (sub ne '6') $
        and (sub ne '7') and (sub ne '8') and (sub ne '9') )  then no=-1
endfor
if (no ne -1) then no=fix(b)
return, no
end

   



