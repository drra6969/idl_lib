PRO hreadaux, namea, naux,time,sccoor,scvel,$
                      scdr1,scdr2,scdr3,scdr4,gsegsm,diptild

idaux=CDF_OPEN(namea); Open  file.

; Basic file info for AUX (auxiliary data)
  aux=CDF_INQUIRE(idaux)  
  ;help,aux,/STRUCT & CDF_CONTROL, idaux, GET_FORMAT = cdfformat
;VARIABLES
;  print, 'No of zvariables: ', aux.nzvars, '  No of attributes: ', aux.natts
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
  naux=epo.MAXREC+1 ;& print, 'number of entries:', naux
  CDF_VARGET, idaux, 2, time, REC_COUNT=naux, /ZVARIABLE
  CDF_VARGET, idaux, 6, sccoor, REC_COUNT=naux, /ZVARIABLE
  CDF_VARGET, idaux, 8, scvel, REC_COUNT=naux, /ZVARIABLE
  CDF_VARGET, idaux, 10, scdr1, REC_COUNT=naux, /ZVARIABLE
  CDF_VARGET, idaux, 11, scdr2, REC_COUNT=naux, /ZVARIABLE
  CDF_VARGET, idaux, 12, scdr3, REC_COUNT=naux, /ZVARIABLE
  CDF_VARGET, idaux, 13, scdr4, REC_COUNT=naux, /ZVARIABLE
  CDF_VARGET, idaux, 30, gsegsm, REC_COUNT=naux, /ZVARIABLE
  CDF_VARGET, idaux, 31, diptild, REC_COUNT=naux, /ZVARIABLE

  tdim=size(time) & scoordim=size(sccoor) & scveldim=size(scvel)
  scdr1dim=size(scdr1) & scdr2dim=size(scdr2) & scdr3dim=size(scdr3) 
  scdr4dim=size(scdr4) & gsegsmdim=size(gsegsm) & dipdim=size(diptild)
  print,'Read Auxiliary Data'
  print, 'Time array dim:', tdim
  if tdim(2) ne scoordim(2) then $
            print, 'Size SC coord array >< time array:', tdim(2),scoordim(2)
  if tdim(2) ne scveldim(2) then $
            print, 'Size SC vel array >< time array:', tdim(2),scveldim(2)
  if tdim(2) ne scdr1dim(2) then $
            print, 'Size SC dr1 array >< time array:', tdim(2),scdr1dim(2)
  if tdim(2) ne scdr2dim(2) then $
            print, 'Size SC dr2 array >< time array:', tdim(2),scdr2dim(2)
  if tdim(2) ne scdr3dim(2) then $
            print, 'Size SC dr3 array >< time array:', tdim(2),scdr3dim(2)
  if tdim(2) ne scdr4dim(2) then $
            print, 'Size SC dr4 array >< time array:', tdim(2),scdr4dim(2)
  if tdim(2) ne gsegsmdim(2) then $
            print, 'Size gsegsm array >< time array:', tdim(2),gsegsmdim(2)
  if tdim(2) ne dipdim(2) then $
            print, 'Size diptild array >< time array:', tdim(2),dipdim(2)

;Start and end time:
  formdate='(a7,i5, a1, i2.2, a1, i2.2, a3, i2.2, a1, i2.2, a1, i2.2)'
  CDF_EPOCH, time(0), yr, mo, dy, hr, mn, sc, milli, /BREAK
  print, 'Start: ',yr,'/', mo, '/', dy, '/  ', hr, ':',  mn, ':', sc, $
                 form=formdate
  CDF_EPOCH, time(naux-1), yr, mo, dy, hr, mn, sc, milli, /BREAK
  print, 'End:   ',yr,'/', mo, '/', dy, '/  ', hr, ':',  mn, ':', sc, $
                 form=formdate
  
CDF_CLOSE, idaux

  return
end

