PRO readaux, namea, naux,time,sccoor,scvel,$
                      scdr1,scdr2,scdr3,scdr4,gsegsm,diptild

idaux=CDF_OPEN(namea); Open  file.

; Basic file info for AUX (auxiliary data)
  aux=CDF_INQUIRE(idaux)  
  ;help,aux,/STRUCT & CDF_CONTROL, idaux, GET_FORMAT = cdfformat
;VARIABLES
  print, 'No of zvariables: ', aux.nzvars, '  No of attributes: ', aux.natts
  format = '(i3, a16, a30, a16, a10, a10, (i3), a7, (i3))'
  for k=0,aux.nzvars-1 do begin
     var = cdf_varinq(idaux, k, /ZVARIABLE)
;     print, k, 'variable name:', var.name,'datatype:', var.datatype, $
;          'dimvar:', var.dimvar, 'dim:', var.dim, form=format
  endfor
;ATTRIBUTES
  for j=0,aux.natts-1 do begin
     cdf_attinq, idaux, j, name, scope, maxentry
     cdf_control, idaux, attr=j, get_attr_info=x
;     print, j, 'attribute name:', name, scope, ' ', 'R_entr:', $
;            x.numrentries, form=format
  endfor

;Get Variables and dimensions:
;   naux   -  dimension
;   time   -  time
;   sccoor -  spacecraft coord
;   scvel  -  spacecraft velocity
;   scdr1  -  Rel location of spacecraft 1
;   scdr2  -  Rel location of spacecraft 2
;   scdr3  -  Rel location of spacecraft 3
;   scdr4  -  Rel location of spacecraft 4
;   gsegsm -  converion of gse to gsm
;   diptild - dipole tild in gsm
  CDF_CONTROL, idaux, VARIABLE='Epoch__CL_SP_AUX',GET_VAR_INFO=epo,/ZVARIABLE
  ;help,epo,/str
  naux=epo.MAXREC+1 & print, 'number of entries:', naux
  CDF_VARGET, idaux, 2, time, REC_COUNT=naux, /ZVARIABLE
  print, 'Size of the time array:            ', size(time)
  CDF_VARGET, idaux, 6, sccoor, REC_COUNT=naux, /ZVARIABLE
  print, 'Size of the spacecraft coord array:', size(sccoor) ; & print, sccoor
  CDF_VARGET, idaux, 8, scvel, REC_COUNT=naux, /ZVARIABLE
  print, 'Velocity of spacecraft array:      ', size(scvel) ; & print, scvel
  CDF_VARGET, idaux, 10, scdr1, REC_COUNT=naux, /ZVARIABLE
  print, 'Rel location of spacecraft 1 array:', size(scdr1) ; & print, scdr1
  CDF_VARGET, idaux, 11, scdr2, REC_COUNT=naux, /ZVARIABLE
  print, 'Rel location of spacecraft 2 array:', size(scdr2) ; & print, scdr2
  CDF_VARGET, idaux, 12, scdr3, REC_COUNT=naux, /ZVARIABLE
  print, 'Rel location of spacecraft 3 array:', size(scdr3) ; & print, scdr3
  CDF_VARGET, idaux, 13, scdr4, REC_COUNT=naux, /ZVARIABLE
  print, 'Rel location of spacecraft 4 array:', size(scdr4) ; & print, scdr4
  CDF_VARGET, idaux, 30, gsegsm, REC_COUNT=naux, /ZVARIABLE
  print, 'Size of the gsegsm array:          ', size(gsegsm) ; & print, gsegsm
  CDF_VARGET, idaux, 31, diptild, REC_COUNT=naux, /ZVARIABLE
  print, 'Size of the diptild array:         ', size(diptild) ; & print, diptild
;Start and end time:
  formdate='(i5, a1, i2.2, a1, i2.2, a3, i2.2, a1, i2.2, a1, i2.2)'
  CDF_EPOCH, time(0), yr, mo, dy, hr, mn, sc, milli, /BREAK
  print, yr,'/', mo, '/', dy, '/  ', hr, ':',  mn, ':', sc, form=formdate
  CDF_EPOCH, time(naux-1), yr, mo, dy, hr, mn, sc, milli, /BREAK
  print, yr,'/', mo, '/', dy, '/  ', hr, ':',  mn, ':', sc, form=formdate
  
CDF_CLOSE, idaux

  return
end

