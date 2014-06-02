pro lhelp, update=update
;+
; NAME:
;          lhelp
;
; PURPOSE:
;          provide help for local routines
;
; CALLING SEQUENCE:
;          lhelp
;
;          lhelp, /update
;                 this keyword updates the HTML file in ~lumm/plot/idl
;
; INPUTS:
;          none
;
; MODIFICATION HISTORY:
;
;       Fri Sep 15 14:34:47 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;		
;
;-

if keyword_set(update) then $
    mk_html_help, '/usr2/users/lumm/plot/idl', $
                  '/usr2/users/lumm/plot/idl/dirk.hlp.html', $
                  title="Dirk's Programs" 

spawn, 'mosaic -home ~lumm/plot/idl/dirk.hlp.html &', status
;help, status
return
end
