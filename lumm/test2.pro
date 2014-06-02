; demonstrate the faulty button resizing

; make a base with buttons  
base = widget_base(title = 'Test Button', /column, $
		   xoffset=100, yoffset=100)
mybutton = widget_button(base, value=' One (1) ', uvalue='MINE') 
donebutton  = widget_button(base, $
		    value=' --------- Done --------- ', uvalue='DONE')
; get the size of the buttons and print out
help, widget_info(mybutton, /geo), widget_info(donebutton, /geo), /str
; realize the base to see how it looks
widget_control, base, /realize

i=0
read, 'continue? (enter 1)', i

; change the value of the button and force its size to remain what is
; was (note that this doesn't work: the button actually shrinks)
geo=widget_info(mybutton, /geo)
widget_control, mybutton, set_value=' Three (3) ', xsize=geo.xsize
; show what IDL thinks the size of the button is (note that this is
; indeed the size that I would like to see, but it is not the size
; that the button actually has)
help, widget_info(mybutton, /geo), widget_info(donebutton, /geo), /str

read, 'continue? (enter 1)', i

; try again to force the button size...  nothing changes in the
; display, the button is still much too small)
widget_control, mybutton, xsize=202
