PRO hreadcis, namec, ncis,timep,statsp,np,no,nhe,nhia,vp,vhia,tp,thia


idcis=CDF_OPEN(namec); Open a file.

; Basic file info for CIS
  cis=CDF_INQUIRE(idcis)  
  ;help,cis,/STRUCT & CDF_CONTROL, idcis, GET_FORMAT = cdfformat
;VARIABLES
;  print, 'No of zvariables: ', cis.nzvars, '  No of attributes: ', cis.natts
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
  CDF_CONTROL, idcis, VARIABLE=2,GET_VAR_INFO=epop,/ZVARIABLE
  ncis=epop.MAXREC+1 ;& print, 'No of time entries:', ncis
  CDF_VARGET, idcis, 2, timep, REC_COUNT=ncis, /ZVARIABLE
  CDF_VARGET, idcis, 3, statsp, REC_COUNT=ncis, /ZVARIABLE
  CDF_VARGET, idcis, 5, np, REC_COUNT=ncis, /ZVARIABLE
  CDF_VARGET, idcis, 6, no, REC_COUNT=ncis, /ZVARIABLE
  CDF_VARGET, idcis, 7, nhe, REC_COUNT=ncis, /ZVARIABLE
  CDF_VARGET, idcis, 9, nhia, REC_COUNT=ncis, /ZVARIABLE
  CDF_VARGET, idcis, 10, vp, REC_COUNT=ncis, /ZVARIABLE
  CDF_VARGET, idcis, 12, vhia, REC_COUNT=ncis, /ZVARIABLE
  CDF_VARGET, idcis, 14, vo, REC_COUNT=ncis, /ZVARIABLE
  CDF_VARGET, idcis, 16, tp, REC_COUNT=ncis, /ZVARIABLE
  CDF_VARGET, idcis, 17, thia, REC_COUNT=ncis, /ZVARIABLE
  
  tdim=size(timep) & stdim=size(statsp) & npdim=size(np) 
  nodim=size(no) & nhedim=size(nhe) & nhiadim=size(nhia)
  vpdim=size(vp) & vhiadim=size(vhia)
  tpdim=size(tp) & thiadim=size(thia)
  print,'Read Plasma Data'
  print, 'Time array dim:', tdim
  
  if tdim(2) ne stdim(2) then $
            print, 'Size status array >< time array:', tdim(2),stdim(2)
  if tdim(2) ne npdim(2) then $
            print, 'Size np array >< time array:', tdim(2),npdim(2)
  if tdim(2) ne nhiadim(2) then $
            print, 'Size nhia array >< time array:', tdim(2),nhiadim(2)
  if tdim(2) ne nodim(2) then $
            print, 'Size no array >< time array:', tdim(2),nodim(2)
  if tdim(2) ne nhedim(2) then $
            print, 'Size nhe array >< time array:', tdim(2),nhedim(2)
  if tdim(2) ne vpdim(2) then $
            print, 'Size vp array >< time array:', tdim(2),vpdim(2)
  if tdim(2) ne vhiadim(2) then $
            print, 'Size vhia array >< time array:', tdim(2),vhiadim(2)
  if tdim(2) ne tpdim(2) then $
            print, 'Size tp array >< time array:', tdim(2),tpdim(2)
  if tdim(2) ne thiadim(2) then $
            print, 'Size thia array >< time array:', tdim(2),thiadim(2)

;Start and end time:
  formdate='(a7,i5, a1, i2.2, a1, i2.2, a3, i2.2, a1, i2.2, a1, i2.2)'
  CDF_EPOCH, timep(0), yr, mo, dy, hr, mn, sc, milli, /BREAK
  print, 'Start: ',yr,'/', mo, '/', dy, '/  ', hr, ':',  mn, ':', sc, $
                 form=formdate
  CDF_EPOCH, timep(ncis-1), yr, mo, dy, hr, mn, sc, milli, /BREAK
  print, 'End:   ',yr,'/', mo, '/', dy, '/  ', hr, ':',  mn, ':', sc, $
                 form=formdate
  

CDF_CLOSE, idcis

  return
end
