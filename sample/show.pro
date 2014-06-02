PRO show_event, event
; This procedure is the event handler for the widgets.

; COMMON blocks are used because both 'show' and 'show_event' 
; should know about any widget that can be touched: 
; The "groupleader" COMMON block is needed for the popup widget (FIELD)

COMMON salar, mx, my, mz, mf, msx, msy, msz
COMMON things, list, file_list,data_list, ifile, start_up, time, $
	       directory, log_plots,field_label
COMMON groupleader, base, file_base, input_base, rightbase, $
               leftbase, err_base, ps_base
common buttons, time1, mx1, my1, mz1, mf1, listbutton
common data, xm, x, y, z
common local, a, a_out, title, expand_flag, prof_flag, $
       bilin_flag, win_x, win_y, win_del, clrbar_flag, unit, b_w, filenumber, ps_type
common stuff, s
value=1
; Make an array that holds the filenames with data
if not keyword_set(start_up) then begin 
  unit='string with units'
  directory = '/home/weima/mxr3d/test0'
;  directory = getenv('show_dir')
  file_list = findfile(directory+'/m3d*')
  win_del = 40
  win_x=win_del
  win_y=win_del
  log_plots=1
  ps_type=' image'
  b_w=1
  filenumber=0
  prof_flag=0
  expand_flag=0
  clrbar_flag=1
  bilin_flag=0
  start_up=1
  field_label=['Plasma density','Plasma velocity Vx', $
	     'Plasma velocity Vy','Plasma velocity Vz', $
	     'Magnetic field Bx','Magnetic field By',$
             'Magnetic field Bz','Total Energy',$ 
	     'Plasma pressure','Magnetic pressure',$
	     'Tension force Ftx','Magnetic pressure force Fmx',$
	     'Plasma pressure force Fpx','Tension force Fty',$
	     'Magnetic pressure force Fmy','Plasma pressure force Fpy']
endif

WIDGET_CONTROL, event.id, GET_UVALUE = eventval

; This CASE statement branches based upon the value of 'eventval':
CASE eventval OF
   "FILE"  : begin
	       file_base = WIDGET_BASE(TITLE='Select file',/COLUMN, $
				       XSIZE=300, XOFFSET=110, YOFFSET=200)
	       list = WIDGET_LIST(file_base, VALUE=file_list, $
 		                  UVALUE='LIST', YSIZE=5)
	       changedir = WIDGET_BUTTON(file_base, $
				  VALUE='Change directory', UVALUE='CHDIR')
	       compressbutton = WIDGET_BUTTON(file_base, $
				  VALUE='(Un)compress the file', UVALUE='COMPR')
               readbutton = WIDGET_BUTTON(file_base, $
				  VALUE='Read the file', UVALUE='READ')
	       exitbutton = WIDGET_BUTTON(file_base, VALUE='Return', $
				  UVALUE='RETFIL')
	       WIDGET_CONTROL, base, SENSITIVE=0
	       WIDGET_CONTROL, file_base, /REALIZE
	       XMANAGER, "show", file_base, GROUP_LEADER=base
	     end
   "LIST"  : begin
	       ifile = event.index + 1
	       print, 'selected file: ',file_list(ifile-1)
	     end
   "CHDIR" : begin
	       dir_base = WIDGET_BASE(TITLE='Select directory', $
			       xsize=300, xoffset=120, yoffset=220)
	       changedir = WIDGET_TEXT(dir_base, /EDITABLE, XSIZE=30, YSIZE=1, $
			       VALUE=directory, UVALUE='CHDIR1')
	       WIDGET_CONTROL, file_base, SENSITIVE=0
	       WIDGET_CONTROL, dir_base, /REALIZE
	       XMANAGER, "show", dir_base, GROUP_LEADER=base
	     end
   "CHDIR1": begin
	       WIDGET_CONTROL, event.id, GET_VALUE=dir
	       directory=dir(0)
	       file_list = findfile(directory+'/m3d*')
	       if not keyword_set(file_list) then file_list=' no files found'
	       WIDGET_CONTROL, event.top, /DESTROY
	       WIDGET_CONTROL, file_base, SENSITIVE=1
	       WIDGET_CONTROL, list, SET_VALUE=file_list
	     end
   "COMPR" : begin
	       if not keyword_set(ifile) then print_err, ' Must select file' $
	       else begin
	         i = strpos(file_list(ifile-1), '.Z',0)
	         if i ge 1 then command = 'uncompress -f ' + file_list(ifile-1) $
		   else command = 'compress -f ' + file_list(ifile-1) 
	         print, command
	         spawn, command
	         file_list = findfile(directory+'/m3d*')
	         WIDGET_CONTROL, list, SET_VALUE=file_list
	       endelse
	     end
   "READ"  : begin
	       if keyword_set(ifile) then begin
	          WIDGET_CONTROL, event.id, SET_VALUE=' ...Reading...'
	          read_file, file_list(ifile-1),xm,x,y,z
	          WIDGET_CONTROL, listbutton, SENSITIVE=0
	       endif else print_err, ' No file to read '
	       WIDGET_CONTROL, event.top, /DESTROY
	       WIDGET_CONTROL, rightbase, SENSITIVE=1
	       WIDGET_CONTROL, base, SENSITIVE=1
	     end
   "RETFIL": begin
	       WIDGET_CONTROL, event.top, /DESTROY
	       WIDGET_CONTROL, base, SENSITIVE=1
	     end  
   "FIELD" : begin
	       file_base = WIDGET_BASE(TITLE='Select field',/COLUMN, $
				       XSIZE=220, XOFFSET=110, YOFFSET=200)
   	       pick = WIDGET_LIST(file_base, VALUE=field_label, $	
 	                          UVALUE = 'PICK', YSIZE = 15)	
		; Desensitize the parent of popup
	       WIDGET_CONTROL, base, SENSITIVE=0
		; Realize the popup:
	       WIDGET_CONTROL, file_base, /REALIZE
		; Hand off control of the widget to the XMANAGER:
	       XMANAGER, "show", file_base, GROUP_LEADER=base
	     end
   "SELECT": begin
	       file_base = WIDGET_BASE(TITLE='Select period',/COLUMN, $
				       XSIZE=300, XOFFSET=110, YOFFSET=200)
	       frombase  = WIDGET_BASE(file_base, /ROW, /FRAME)
	       fromlabel = WIDGET_LABEL(frombase, VALUE='from day:')
	       from_day = WIDGET_TEXT(frombase, /EDITABLE, XSIZE=15, YSIZE=1, $
			       VALUE=string(start_time(0)), $
			       UVALUE='RETSEL')
	       from_hour = WIDGET_TEXT(frombase, /EDITABLE, XSIZE=15, YSIZE=1, $
			       VALUE=string(start_time(1)), $
			       UVALUE='RETSEL')
	       tobase  = WIDGET_BASE(file_base, /ROW, /FRAME)
	       tolabel = WIDGET_LABEL(tobase, VALUE='from day:')
	       to_day = WIDGET_TEXT(tobase, /EDITABLE, XSIZE=15, YSIZE=1, $
			       VALUE=string(stop_time(0)), $
			       UVALUE='RETSEL')
	       to_hour = WIDGET_TEXT(tobase, /EDITABLE, XSIZE=15, YSIZE=1, $
			       VALUE=string(stop_time(1)), $
			       UVALUE='RETSEL')
               returnbutton = WIDGET_BUTTON(file_base, $
	                                    VALUE = 'Pick this period', $
	                                    UVALUE = 'RETSEL')
	       WIDGET_CONTROL, base, SENSITIVE=0
	       WIDGET_CONTROL, file_base, /REALIZE
	       XMANAGER, "show", file_base, GROUP_LEADER=base
	     end
   "COLOR" : xloadct
   "PICK"  : begin
	       ifield=event.index
	       title=field_label(ifield)
	       print, "picked ", field_label(ifield)
	       case ifield of
	         0:    begin & a=xm(*,*,*,0) & unit=' (cm!e-3!n s!e-1!n)' & end
	         1:    begin & a=xm(*,*,*,1) & unit=' (cm!e-3!n s!e-1!n)' & end
	         2:    begin & a=xm(*,*,*,2) & unit=' (cm!e-3!n s!e-1!n)' & end
	         3:    begin & a=xm(*,*,*,3) & unit=' (cm!e-3!n s!e-1!n)' & end
	         4:    begin & a=xm(*,*,*,4) & unit=' (cm!e-3!n s!e-1!n)' & end
	         5:    begin & a=xm(*,*,*,5) & unit=' (cm!e-3!n)' & end
	         6:    begin & a=xm(*,*,*,6) & unit=' (cm!e-3!n)' & end
	         7:    begin & a=xm(*,*,*,7) & unit=' (cm!e-3!n)' & end
	         8:    begin & a=xm(*,*,*,8) & unit=' (cm!e-3!n)' & end
	         9:    begin & a=xm(*,*,*,9) & unit=' (cm!e-3!n)' & end
		 10:   begin & a=xm(*,*,*,10) & unit=' (cm!e-3!n)' & end
	         11:   begin & a=xm(*,*,*,11) & unit=' (cm!e-3!n)' & end
		 12:   begin & a=xm(*,*,*,12) & unit=' (cm!e-3!n)' & end
		 13:   begin & a=xm(*,*,*,13) & unit=' (cm!e-3!n)' & end
		 14:   begin & a=xm(*,*,*,14) & unit=' (cm!e-3!n)' & end
		 15:   begin & a=xm(*,*,*,15) & unit=' (cm!e-3!n)' & end
	         else: print, 'error - try again'
	       endcase
	       WIDGET_CONTROL, event.id, $
			SET_VALUE=[' ',' ',' ',' ',' Interpolating ',title]
	       returnbutton = WIDGET_BUTTON( base, VALUE='select', $
				  UVALUE='INPUTS')
	       if log_plots eq 1 then expand, alog10(a), a_out $
	                         else expand, a, a_out
	       expand_flag=1
	       print, 'interpolated field:',title
	       WIDGET_CONTROL, event.top, /DESTROY
	       WIDGET_CONTROL, base, SENSITIVE=1
	     end
   "INPUTS" : begin
	       inputS_base = WIDGET_BASE(TITLE='select the location', $
			     xsize=500,ysize=300, xoffset=120, yoffset=220)
	       msybase = WIDGET_BASE( input_base, /ROW, /FRAME)
	       msylabel = WIDGET_LABEL(msybase, VALUE='msy=')
	       msy = WIDGET_SLIDER(msybase,XSIZE=400, YSIZE=50,UVALUE='INPUT1')
	       WIDGET_CONTROL, base, SENSITIVE=0
	       WIDGET_CONTROL, input_base, /REALIZE
	       XMANAGER, "show", input_base, GROUP_LEADER=base
	     end
   "IMAGE" : begin
	       if keyword_set(expand_flag) then begin
	          if clrbar_flag eq 1 then $
		    window, /free, xsize=720, xpos=win_x, ypos=win_y else $
		    window, /free, xpos=win_x, ypos=win_y 
		  win_x=win_x + win_del
		  win_y=win_y + win_del
	          image, a_out, clrbar_flag, bilin_flag, title=title, units=unit
		  prof_flag=1
	       endif else print_err, ' must interpolate before plotting '
	     end
   "PROF":   begin
	       if keyword_set(prof_flag) then begin
	         label=title+unit
                 if log_plots eq 1 then label='log(' + label + ' )'
	         prof, label, directory, filenumber
	       endif else print_err, ' need image to make profile '
	     end
   "INPUT" : begin
	       input_base = WIDGET_BASE(TITLE='Input Data for Parameter', $
			 /COLUMN, xsize=300, xoffset=120, yoffset=220)
	       timebase = WIDGET_BASE( input_base, /ROW, /FRAME)
	       timelabel = WIDGET_LABEL(timebase, VALUE='time=')
	       time1 = WIDGET_TEXT(timebase, /EDITABLE, XSIZE=15, YSIZE=1,$
	       VALUE=string(time), UVALUE='RETSEL')
	       mxbase = WIDGET_BASE( input_base, /ROW, /FRAME)
	       mxlabel = WIDGET_LABEL(mxbase, VALUE='mx=')
	       mx1 = WIDGET_TEXT(mxbase, /EDITABLE, XSIZE=150, YSIZE=1,$
	       VALUE=string(mx), UVALUE='RETSEL')
	       mybase = WIDGET_BASE( input_base, /ROW, /FRAME)
	       mylabel = WIDGET_LABEL(mybase, VALUE='my=')
	       my1 = WIDGET_TEXT(mybase,/EDITABLE,XSIZE=15, YSIZE=1,$
	       VALUE=string(my), UVALUE='RETSEL')
	       mzbase = WIDGET_BASE( input_base, /ROW, /FRAME)
	       mzlabel = WIDGET_LABEL(mzbase, VALUE='mz=')
	       mz1 = WIDGET_TEXT(mzbase,/EDITABLE,XSIZE=15, YSIZE=1,$
	       VALUE=string(mz), UVALUE='RETSEL')
	       mfbase = WIDGET_BASE( input_base, /ROW, /FRAME)
	       mflabel = WIDGET_LABEL(mfbase, VALUE='mf=')
	       mf1 = WIDGET_TEXT(mfbase,/EDITABLE,XSIZE=15, YSIZE=1,$
	       VALUE=string(mf), UVALUE='RETSEL')
	       returnbutton = WIDGET_BUTTON( input_base, VALUE='Return', $
				  UVALUE='RETSEL')
	       WIDGET_CONTROL, base, SENSITIVE=0
	       WIDGET_CONTROL, input_base, /REALIZE
	       XMANAGER, "show", input_base, GROUP_LEADER=base
	     end
   "PS"    : begin
	       ps_base = WIDGET_BASE(TITLE='Make PS file',/COLUMN, $
				   XSIZE=220, XOFFSET=180, YOFFSET=200)
               b_w_button = WIDGET_BUTTON(ps_base, $
		                   VALUE=' B/W PS printer', UVALUE='B_WPS')
               pick_button = WIDGET_BUTTON(ps_base, $
		                   VALUE=ps_type, UVALUE='PSPICK')
               draw_button = WIDGET_BUTTON(ps_base, $
		                   VALUE=' Make PS file', UVALUE='PS_IM')
               exit_button = WIDGET_BUTTON(ps_base, $
		                   VALUE=' Do not make a file', UVALUE='PS_RET')
	       WIDGET_CONTROL, rightbase, SENSITIVE=0
	       WIDGET_CONTROL, ps_base, /REALIZE
	       XMANAGER, "show", ps_base, GROUP_LEADER=base
	     end
   "PSPICK": begin
	       WIDGET_CONTROL, event.id, GET_VALUE=ps_type
	       print, ps_type
	       if ps_type eq ' image' then $
	         WIDGET_CONTROL, event.id, SET_VALUE=' grid surface'
	       if ps_type eq ' grid surface' then $
		 WIDGET_CONTROL, event.id, SET_VALUE=' input parameters'
	       if ps_type eq ' input parameters' then $
		 WIDGET_CONTROL, event.id, SET_VALUE=' image'
	       WIDGET_CONTROL, event.id, GET_VALUE=ps_type
	     end
   "PS_IM" : begin
	       if keyword_set(expand_flag) then begin
                 filenumber=filenumber+1
                 old_device=!D.NAME
                 set_plot, "PS"
                 if b_w eq 1 then begin
                   file=directory+'/image'+strcompress(filenumber,/remove_all)+'.ps' 
                   device, filename=file, bits_per_pixel=4, /landscape 
                 endif else begin
                   file=directory+'/image'+strcompress(filenumber,/remove_all)+'.cps' 
                   device, filename=file, bits_per_pixel=8, /landscape, /color
                 endelse
	         if log_plots eq 0 then a_temp=a_out $
		   else a_temp=(a_out > (max(a_out)-6))
		 print, 'plotting: ',ps_type
	         if ps_type eq ' image' then $
                   image, a_temp, clrbar_flag, bilin_flag, title=title, units=unit
	         if ps_type eq ' grid surface' then  $
			surf1, a_temp, log_plots, title=title, units=unit
	         if ps_type eq ' input parameters' then  plot_input
                 device, /close_file
                 print, "postscipt file "+file+" is created"
                 set_plot, old_device
	       endif else begin
		 print_err, ' must interpolate before plotting '
	       endelse
	       WIDGET_CONTROL, ps_base, /DESTROY
	       WIDGET_CONTROL, rightbase, SENSITIVE=1
	     end
   "B_WPS" : begin
	       if b_w eq 1 then begin
	         b_w=0
	         WIDGET_CONTROL, event.id, SET_VALUE=' Color PS printer'
	       endif else begin
	         b_w=1
	         WIDGET_CONTROL, event.id, SET_VALUE=' B/W PS printer'
	       endelse
	     end
   "PS_RET": begin
	       WIDGET_CONTROL, ps_base, /DESTROY
	       WIDGET_CONTROL, rightbase, SENSITIVE=1
	     end
   "SURF"  : begin
	       if keyword_set(expand_flag) then $
	          xsurf, a_out, title=title, group=base $
	       else print_err, ' must interpolate before plotting '
             end
   "RETSEL": begin
	       WIDGET_CONTROL, time1, GET_VALUE=value
	       time=float(value)
	       WIDGET_CONTROL, mx1, GET_VALUE=value
	       mx=fix(value)
	       WIDGET_CONTROL, my1, GET_VALUE=value
	       my=fix(value)
	       WIDGET_CONTROL, mz1, GET_VALUE=value
	       mz=fix(value)
	       WIDGET_CONTROL, mf1, GET_VALUE=value
	       mf=fix(value)
	       expand_flag=0
	       prof_flag=0
	       WIDGET_CONTROL, event.top, /DESTROY
	       WIDGET_CONTROL, base, SENSITIVE=1
	     end
   "RETERR": begin
	       WIDGET_CONTROL, err_base, /DESTROY
	       WIDGET_CONTROL, base, SENSITIVE=1
	     end   
   "DEL"   : if !D.WINDOW ne -1 then begin
	       wdelete
	       win_x=win_x - win_del
	       win_y=win_y - win_del
	       prof_flag=0
	     end
   "CONBAR": begin
	       if clrbar_flag eq 1 then begin
	         clrbar_flag=0
	         WIDGET_CONTROL, event.id, SET_VALUE=' Contour Lines'
	       endif else begin
	         clrbar_flag=1
	         WIDGET_CONTROL, event.id, SET_VALUE=' Color Bar'
	       endelse
	     end
   "BILIN" : begin
	       if bilin_flag eq 0 then begin
	         bilin_flag=1
	         WIDGET_CONTROL, event.id, SET_VALUE=' Smooth (takes long!!)'
	       endif else begin
	         bilin_flag=0
	         WIDGET_CONTROL, event.id, SET_VALUE=' Step function'
	       endelse
	     end
   "LINLOG": begin
	       expand_flag=0
	       prof_flag=0
	       if log_plots eq 1 then begin
	         log_plots=0
	         WIDGET_CONTROL, event.id, SET_VALUE=' Linear plots'
	       endif else begin
	         log_plots=1
	         WIDGET_CONTROL, event.id, SET_VALUE=' Log plots'
	       endelse
	     end
   "DONE"  : begin
	       WIDGET_CONTROL, event.top, /DESTROY
	       while !D.WINDOW ne -1 do wdelete
	      end

ENDCASE
END


PRO show, GROUP = GROUP
; This procedure creates the top level widgets and runs the show...
COMMON salar, mx, my, mz, mf, msx, msy, msz
COMMON things, list, file_list,data_list, ifile, start_up, time, $
	       directory, log_plots,field_label
COMMON groupleader, base, file_base, input_base, rightbase, $
               leftbase, err_base, ps_base
common buttons, time1, mx1, my1, mz1, mf1, listbutton
common data, xm,x,y,z
common stuff, s
common local, a, a_out, title, expand_flag, prof_flag, $
       bilin_flag, win_x, win_y, win_del, clrbar_flag, unit, b_w, filenumber, ps_type
time=1&mx=1&my=1&mz=1&mf=1
; Make the top-level base. The XSIZE keyword is used to make
; the base wide enough so that the full title shows:
base = WIDGET_BASE(TITLE = 'MHD Plots', /ROW, $
		   XOFFSET=100, YOFFSET=100)
leftbase = WIDGET_BASE(base, /COLUMN, XSIZE=150)
rightbase = WIDGET_BASE(base, /COLUMN, XSIZE=200)

; action buttons
inputbutton = WIDGET_BUTTON(leftbase, $
		    VALUE='input parameters', UVALUE='INPUT')
listbutton =  WIDGET_BUTTON(leftbase, $
		    VALUE='Select a file', UVALUE='FILE')
selectpopup = WIDGET_BUTTON(rightbase, $
		    VALUE='Select period for plots', UVALUE='SELECT')
fieldpopup  = WIDGET_BUTTON(rightbase, $
		    VALUE='Select a field', UVALUE='FIELD')
imagebutton = WIDGET_BUTTON(rightbase, $
		    VALUE='Plot as image', UVALUE='IMAGE')
ps_button =   WIDGET_BUTTON(rightbase, $
		    VALUE='Make PS file', UVALUE='PS')
surfbutton =  WIDGET_BUTTON(rightbase, $
		    VALUE='Plot as surface', UVALUE='SURF')
profbutton =   WIDGET_BUTTON(rightbase, $
		    VALUE='Extract and plot profile', UVALUE='PROF')
colorbutton = WIDGET_BUTTON(rightbase, $
		    VALUE='Change color table', UVALUE='COLOR')
clrbar_button = WIDGET_BUTTON(leftbase, $
		    VALUE='Color Bar', UVALUE='CONBAR')
lin_log_button = WIDGET_BUTTON(leftbase, $
		    VALUE='Log plots', UVALUE='LINLOG')
bilinear_button = WIDGET_BUTTON(leftbase, $
		    VALUE='Step function', UVALUE='BILIN')
del_window =  WIDGET_BUTTON(leftbase, $
		    VALUE='Delete the last plot', UVALUE='DEL')
donebutton  = WIDGET_BUTTON(leftbase, $
		    VALUE='Done', UVALUE='DONE')
							 
; Realize the widgets:
WIDGET_CONTROL, base, /REALIZE
WIDGET_CONTROL, leftbase, /REALIZE
WIDGET_CONTROL, rightbase, /REALIZE
WIDGET_CONTROL, rightbase, sensitive=0

; Hand off to the XMANAGER:
XMANAGER, 'show', base, GROUP_LEADER = GROUP

END
