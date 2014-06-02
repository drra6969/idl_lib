pro show_kp, year, out, date=date
;+
; NAME:
;         show_kp
;
; PURPOSE:
;         show geomagnetic indices
;
; CALLING SEQUENCE:
;         show_kp, year, date=[month, day-of-month]
;                  or
;         show_kp, year, date=doy
;         to get a display for a specific date or day of year
;
;                  or
;         show_kp, year, out
;         to get data for the entire year into structure "out"
;         use "help, out, /str" to see the structure that is returned.
; 
; INPUTS:
;         year: year (example: 1994, or 94)
;         date: date (either as array [month, day-of-month] or as
;               single integer to give the day-of-year)
;
; OPTIONAL INPUTS:
;         date is optional - if not specified, the Kp for the entire
;                            year are retrieved
;       
; OPTIONAL OUTPUTS:
;         if date is not specified, provide a variable out for output. 
;
; RESTRICTIONS:
;         This routine requires that a file with the geomagnetic
;         indices is available in subdirectory
;         ~lumm/indices/data/. These files can be obtained by
;         anonymous ftp via:   
;                ftp ftp.ngdc.noaa.gov
;                directory: /STP/GEOMAGNETIC_DATA/INDICES/KP_AP
;                filenames: "1932" etc.
;
; MODIFICATION HISTORY:
;
;       Thu Oct 5 16:56:17 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;-

if year lt 100 then year=1900+year
dummi=findfile('/lk/lumm/indices/data/' + $
               strcompress(year, /rem)+'*', count=count)

if count ne 1 then begin
   print, 'no data available for the specified year'
   print, 'you may obtain these data by'
   print, 'loke> cd ~lumm/indices/data'
   print, 'loke> ftp ftp.ngdc.noaa.gov'
   print, 'Name: anonymous'
   print, 'Password: you login name, e.g. lumm@loke.gi.alaska.edu'
   print, 'ftp> cd STP/GEOMAGNETIC_DATA/INDICES/KP_AP'
   print, 'ftp> get 1995'
   print, 'ftp> bye'
   print, 'loke> gzip 1995'
   return
endif

file='/tmp/'+strcompress(year,/re)
spawn, 'gunzip -c /lk/lumm/indices/data/'+strcompress(year,/re)+' > '+file
spawn, 'wc -l '+file, lines
lines=fix(lines(0))           ; number of lines in the file

format='(3i2,i4,9i2,10i3,f3.1,i1,i3,f5.1,i1)'
openr, unit, file, /get_lun
out=replicate({data, year:0, month:0, day:0, rot:0, rot_day:0, $
                     kp:intarr(8), sum_kp:0, ap:intarr(8), sum_ap:0, $
                     cp:0., c9:0, spot:0, sf107:0., q:0}, lines)
readf, unit, out, format=format
free_lun, unit
spawn, 'rm '+file

if keyword_set(date) then begin
    if n_elements(date) eq 2 then doy=doy_mmdd(year, date(0), date(1)) $
                             else doy=date
    if doy gt n_elements(out) then begin
        print, 'no data available for the specified date'
        return
    endif

    print, '               year=', out(doy-1).year
    print, '              month=', out(doy-1).month
    print, '                day=', out(doy-1).day
    print, '    rotation number=', out(doy-1).rot
    print, '       rotation day=', out(doy-1).rot_day
    print, '        3-hourly kp=', out(doy-1).kp, format='(a,8i3.2)'
    print, '             sum kp=', out(doy-1).sum_kp
    print, '        3-hourly ap=', out(doy-1).ap, format='(a,8i3)'
    print, '                 Ap=', out(doy-1).sum_ap
    print, '                 Cp=', out(doy-1).cp, format='(a,f10.1)'
    print, '                 C9=', out(doy-1).c9
    print, '     sunspot number=', out(doy-1).spot
    print, 'Ottawa 10.7-cm flux=', out(doy-1).sf107, format='(a,f10.1)'
    print, '     flux qualifier=', out(doy-1).q
endif

return
end
