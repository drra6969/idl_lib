pro test_event, event
common my_c, my, mybutton
widget_control, event.id, get_uvalue = eventval
; toggle the text on the button and
; toggle the value of variable 'my' between 1 and 2
case eventval of
  'MINE' : begin
      if my gt 1 then $
        widget_control, mybutton, set_value=' One (1)' $
      else $
        widget_control, mybutton, set_value=' Three (3)'
      my=(my mod 2) + 1
  end
  'DONE' : widget_control, event.top, /destroy
endcase
end

pro test
common my_c, my, mybutton
my=1
base = widget_base(title = 'Test Button', /column, $
		   xoffset=100, yoffset=100)
base1=widget_base(base,/row)
base2=widget_base(base,/row)
mybutton = widget_button(base1, $
		    value=' One (1) ', $
		    uvalue='MINE'); , xsize=400)
donebutton  = widget_button(base2, $
		    value=' --------- Done --------- ', $
		    uvalue='DONE')

widget_control, base, /realize
xmanager, 'test', base
end
