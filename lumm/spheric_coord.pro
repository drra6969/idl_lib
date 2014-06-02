;+
;
; NAME:
;      spheric_coord
;
; PURPOSE:
;      Do a forward or reverse transformation between spherical and
;      carthesian coordinates
;         a_out = spheric_coord( a, theta, phi, [/inverse])
;      transforms a vector a=[a_x, a_y, a_z] from a carthesian coordinate 
;      system to spherical coordinates
;         a is a three component vector in carthesian coordinates:
;         a(0) = x-component, (1) = y-comonent, (2) = z-component (input)
;         (theta,phi) = position of the sperical vector (input)
; output is:
;      a_out(0) = radial-component, 
;      a_out(1) = theta-component, 
;      a_out(2) = phi-component of the new vector
;
;      if inverse is set, the transformation goes from spherical to
;      carthesian coordinates.
;
; see Rottmann page 74
;-

function spheric_coord, a_in, theta, phi, inverse=inverse

m=[[sin(theta)*cos(phi), sin(theta)*sin(phi),  cos(theta)], $
   [cos(theta)*cos(phi), cos(theta)*sin(phi), -sin(theta)], $
   [          -sin(phi),            cos(phi),           0]]

m=transpose(m)   ; idl's way of specifying matrices

if keyword_set(inverse) then m=invert(m)

return, m#a_in
end
