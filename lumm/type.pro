function type, item, text=text

;+
; NAME:
;     type
;
; Purpose:
;     determine and return the type of the item
;     Note that the same information can be obtained with the IDL
;     routine SIZE.  The advantage of this routine (TYPE) is that it
;     does not return an array with additional information as SIZE
;     does. 
;
; use as 
;       print, type(itme, [/text])
;       if /text is given, the type is returned as english text,
;       otherwise the code below (same as in SIZE) is returned.
;
; description of the codes that are returned:
;       0   undefined
;       1   byte
;       2   integer
;       3   longword integer
;       4   floating point
;       5   double-precision floating
;       6   complex floating
;       7   string
;       8   structure
;       9   double-precision complex 
;-

english=['undefined', 'byte', 'integer', 'longword integer', $
        'floating point', 'double-precision floating', $
        'complex floating', 'string', 'structure', $
        'double-precision complex']

n=size(item)
if keyword_set(text) then return, english(n(n(0)+1)) $
                     else return, n(n(0)+1)
end
