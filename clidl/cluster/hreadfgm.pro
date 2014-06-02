PRO hreadfgm, namef, nfgm,timem,statsm,b,varbt,varbb

idfgm=CDF_OPEN(namef); Open a file.

; Basic file info for FGM
  fgm=CDF_INQUIRE(idfgm)  
  ;help,fgm,/STRUCT & CDF_CONTROL, idfgm, GET_FORMAT = cdfformat
;VARIABLES
;  print, 'No of zvariables: ', fgm.nzvars, '  No of attributes: ', fgm.natts
  format = '(i3, a16, a30, a16, a10, a10, (i3), a7, (i3))'
  
  for k=0,fgm.nzvars-1 do begin
     var = cdf_varinq(idfgm, k, /ZVARIABLE)
;     print, k, 'variable name:', var.name,'datatype:', var.datatype, $
;          'dimvar:', var.dimvar, 'dim:', var.dim, form=format
  endfor
;ATTRIBUTES
  for j=0,fgm.natts-1 do begin
     cdf_attinq, idfgm, j, name, scope, maxentry
     cdf_control, idfgm, attr=j, get_attr_info=x
;     print, j, 'attribute name:', name, scope, ' ', 'R_entr:', $
;            x.numrentries, form=format
  endfor
;Get Variables and dimensions:
;   nfgm   - dimension
;   timem  - time
;   statsm - status array of fgm
;   b      - magnetic field components
;   varbt  - varaince of B
;   varbb  - varaince of |B|
  CDF_CONTROL, idfgm, VARIABLE=2,GET_VAR_INFO=epom,/ZVARIABLE
  nfgm=epom.MAXREC+1 ;& print, 'No of time entries:', nfgm
  CDF_VARGET, idfgm, 2, timem, REC_COUNT=nfgm, /ZVARIABLE
  CDF_VARGET, idfgm, 3, statsm, REC_COUNT=nfgm, /ZVARIABLE
  CDF_VARGET, idfgm, 5, b, REC_COUNT=nfgm, /ZVARIABLE
  CDF_VARGET, idfgm, 7, varbt, REC_COUNT=nfgm, /ZVARIABLE
  CDF_VARGET, idfgm, 8, varbb, REC_COUNT=nfgm, /ZVARIABLE   
  
  tdim=size(timem) & stdim=size(statsm) & bdim=size(b)
  btdim=size(varbt) & bbdim=size(varbb)
  print,'Read Magnetic Data'
  print, 'Time array dim:', tdim
  
  if tdim(2) ne stdim(2) then $
            print, 'Size status array >< time array:', tdim(2),stdim(2)
  if tdim(2) ne bdim(2) then $
            print, 'Size b array >< time array:', tdim(2),bdim(2)
  if tdim(2) ne btdim(2) then $
            print, 'Size bt array >< time array:', tdim(2),btdim(2)
  if tdim(2) ne bbdim(2) then $
            print, 'Size bb array >< time array:', tdim(2),bbdim(2)

;Start and end time:
  formdate='(a7,i5, a1, i2.2, a1, i2.2, a3, i2.2, a1, i2.2, a1, i2.2)'
  CDF_EPOCH, timem(0), yr, mo, dy, hr, mn, sc, milli, /BREAK
  print, 'Start: ',yr,'/', mo, '/', dy, '/  ', hr, ':',  mn, ':', sc, $
                   form=formdate
  CDF_EPOCH, timem(nfgm-1), yr, mo, dy, hr, mn, sc, milli, /BREAK
  print, 'End:   ',yr,'/', mo, '/', dy, '/  ', hr, ':',  mn, ':', sc, $
                   form=formdate

  CDF_CLOSE, idfgm

  return
end
