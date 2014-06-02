PRO readcis, namec, ncis,timep,statsp,np,no,nhe,nhia,vp,vhia,tp,thia


idcis=CDF_OPEN(namec); Open a file.

; Basic file info for CIS
  cis=CDF_INQUIRE(idcis)  
  ;help,cis,/STRUCT & CDF_CONTROL, idcis, GET_FORMAT = cdfformat
;VARIABLES
  print, 'No of zvariables: ', cis.nzvars, '  No of attributes: ', cis.natts
  format = '(i3, a16, a30, a16, a10, a10, (i3), a7, (i3))'
  for k=0,cis.nzvars-1 do begin
     var = cdf_varinq(idcis, k, /ZVARIABLE)
;     print, k, 'variable name:', var.name,'datatype:', var.datatype, $
;          'dimvar:', var.dimvar, 'dim:', var.dim, form=format
  endfor
;ATTRIBUTES
  for j=0,cis.natts-1 do begin
     cdf_attinq, idcis, j, name, scope, maxentry
     cdf_control, idcis, attr=j, get_attr_info=x
;     print, j, 'attribute name:', name, scope, ' ', 'R_entr:', $
;            x.numrentries, form=format
  endfor
  print,'???'
;Get Variables and dimensions:
;   ncis   - dimension
;   timep  - time
;   statsp - status array of cis
;   np     - proton density (codif)
;   no     - o+ density (codif)
;   nhe    - he+ density (codif)
;   nhia   - hia density (codif)
;   vp     - proton velocity (codif)
;   vhia   - hia velocity (codif)
;   tp     - proton temp (codif)
;   thia   - hia temp (codif)
;   
  CDF_CONTROL, idcis, VARIABLE='Epoch__CL_SP_CIS',GET_VAR_INFO=epop,/ZVARIABLE
  ncis=epop.MAXREC+1 & print, 'No of time entries:', ncis
  CDF_VARGET, idcis, 2, timep, REC_COUNT=ncis, /ZVARIABLE
  print, 'Size of the time array:  ', size(timep)
  CDF_VARGET, idcis, 3, statsp, REC_COUNT=ncis, /ZVARIABLE
  print, 'Size of the status array:', size(statsp)
  CDF_VARGET, idcis, 5, np, REC_COUNT=ncis, /ZVARIABLE
  print, 'Size of the Np array:    ', size(np) ; & print, np
  CDF_VARGET, idcis, 6, no, REC_COUNT=ncis, /ZVARIABLE
  print, 'Size of the No array:    ', size(no) ; & print, no
  CDF_VARGET, idcis, 7, nhe, REC_COUNT=ncis, /ZVARIABLE
  print, 'Size of the Nhe array:   ', size(nhe) ; & print, nhe
  CDF_VARGET, idcis, 9, nhia, REC_COUNT=ncis, /ZVARIABLE
  print, 'Size of the Nhia array:  ', size(nhia) ; & print, nhia
  CDF_VARGET, idcis, 10, vp, REC_COUNT=ncis, /ZVARIABLE
  print, 'Size of the Vp array:    ', size(vp) ; & print, vp
  CDF_VARGET, idcis, 12, vhia, REC_COUNT=ncis, /ZVARIABLE
  print, 'Size of the Vhia array:  ', size(vhia) ; & print, vhia
  CDF_VARGET, idcis, 14, vo, REC_COUNT=ncis, /ZVARIABLE
  print, 'Size of the Vo array:    ', size(vo) ; & print, vo
  CDF_VARGET, idcis, 16, tp, REC_COUNT=ncis, /ZVARIABLE
  print, 'Size of the Tp array:    ', size(tp) ; & print, tp
  CDF_VARGET, idcis, 17, thia, REC_COUNT=ncis, /ZVARIABLE
  print, 'Size of the Thia array:  ', size(tp) ; & print, thia
  CDF_EPOCH, timep(0), yr, mo, dy, hr, mn, sc, milli, /BREAK
  print, yr,'/', mo, '/', dy, '/  ', hr, ':',  mn, ':', sc, form=formdate
  CDF_EPOCH, timep(ncis-1), yr, mo, dy, hr, mn, sc, milli, /BREAK
  print, yr,'/', mo, '/', dy, '/  ', hr, ':',  mn, ':', sc, form=formdate

CDF_CLOSE, idcis

  return
end
