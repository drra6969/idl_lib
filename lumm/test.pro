pro test_event, event

common my_c, my

widget_control, event.id, get_uvalue = eventval

; toggle the text on the button and
; toggle the value of variable 'my' between 1 and 2 
case eventval of
  'MINE' : begin
                                ; get and show the current button geometry
      geo = widget_info(event.id, /geometry)
      help, geo, /str
                                ; toggle the label on the button.  Note: I
                                ; tried to put xsize=geo.size on the same
                                ; widget_control, but this did not work
                                ; right either.
      if my gt 1 then widget_control, event.id, set_value='One (1)' $
                 else widget_control, event.id, set_value='Three (3)'
                                ; set the button size explicitly.  Note that
                                ; the button's actual size is much smaller
                                ; than the value that is given as xsize, i.e.
                                ; this line seems to have no effect at all.
      widget_control, event.id, xsize=geo.xsize
                                ; toggle the value of `my'
      my = (my mod 2) + 1
                                ; get and show the botton geometry again.
                                ; Note that the value of geo.xsize is not
                                ; consistent with the size that the button
                                ; actually has.
      geo = widget_info(event.id, /geometry)
      help, geo, /str
  end
  'DONE' : widget_control, event.top, /destroy
endcase

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro test

; make a base with two buttons.  The action of the first button is to
; just change the label on itself.  Note that the button changes its size
; whenever the label changes, even though it is not supposed to do so
; (according to the manual).  It even keeps changing its size if the
; value `xsize' is explicitly provided.

common my_c, my
my = 1

base = widget_base(title = 'Test Button', /column, $
		   xoffset=100, yoffset=100)
mybutton = widget_button(base, $
		    value=' One (1) ', $
		    uvalue='MINE') ;, /dynamic_resize) ;, xsize=200)
donebutton  = widget_button(base, $
		    value=' --------- Done --------- ', $
		    uvalue='DONE')

widget_control, base, /realize
xmanager, 'test', base

end

; Here is RSI's solution (and it works - thanks to Carol...):

;First on the widgets,  I have a different approach that will give 
;you the results that you expect.  Comment out this line in your
;event handler :

;;     widget_control, event.id, xsize= geo.xsize


;And in your main Pro file edit the following line accordingly:

;mybutton = widget_button(base, $
;                    value=' One (1) ', $
;                    uvalue='MINE',/dynamic_resize)
