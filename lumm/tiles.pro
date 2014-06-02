;+
; NAME:
;         tiles
;
; PURPOSE:
;          Game modeled after something I've seen on PC, and which was
;          called something stupid like "Minesearcher"....
;
; CALLING SEQUENCE:
;          initiate the program by the command "tiles"
;          it will open a widget and give some rudimentary additional 
;          information in the calling IDL window.
;
;          tiles, [n], [/bw], [/color]
;
; INPUTS:
;   	   none
;
; OPTIONAL INPUTS:
;   	   n specifies the number of tiles
;	   /bw forces the black/white version (b/w works without
;	   specifying /bw, but it looks much better if /bw is given)
;	   /color allows to change the color table 
;	
; MODIFICATION HISTORY:
;
;	(DL - Jan 94)
;-

pro startup
common tiles, a, b, c, x, y, n
; initialize all arrays
a=intarr(n,n)
b=intarr(n,n)
bb=intarr(n+2,n+2)	; like array b, but with an extra border of tiles
m=fix(1.5*n)		; number of objects
i=fix(randomu(seed,m)*n)+1
j=fix(randomu(seed,m)*n)+1
bb(i,j)=1
; bb(1:3,1:2)=0
c=intarr(n,n)
for i=1,n do begin
  for j=1,n do begin
    ii=[i-1,i-1,i-1,i,i+1,i+1,i+1,i]
    jj=[j-1,j,j+1,j+1,j+1,j,j-1,j-1]
    c(i-1,j-1)=total(bb(ii,jj))
  endfor
endfor
b=bb(1:n,1:n)

return
end


pro show_all
common tiles, a, b, c, x, y, n
common my_colors, c_covered, c_uncovered, c_suspect, c_object, c_flag, $
       c_line, delta
; uncover all hidden objects
for i=0,n-1 do for j=0,n-1 do begin
  if b(i,j) eq 1 then square, x(i), y(j), 1./n-.01, color=c_object
endfor
return
end


pro check_neighbors, i, j
common tiles, a, b, c, x, y, n
; uncover all neighbors that are around a "zero neighboring object" tile
ii=[i-1,i-1,i-1,i,i+1,i+1,i+1,i]
jj=[j-1,j,j+1,j+1,j+1,j,j-1,j-1]
i0=where(ii ge 0 and ii le n-1 and jj ge 0 and jj le n-1, icount)
j0=where(ii ge 0 and ii le n-1 and jj ge 0 and jj le n-1, icount)
ii=ii(i0)
jj=jj(j0)
uncover, ii, jj, ii0, jj0, zeros
; now uncover all tiles where new zeros were found.
if zeros gt 0 then for i=0,zeros-1 do check_neighbors, ii0(i), jj0(i)
return
end

pro uncover, iin, jin, iout, jout, zeros
; uncover all tiles that are specified by the indices in arrays iin, jin,
; and return in arrays iout, jout the zeros that were found during this 
; uncovering.  zeros contains then the number of zeros found.
; Uncover only virgin tiles!!!!
common tiles, a, b, c, x, y, n
common my_colors, c_covered, c_uncovered, c_suspect, c_object, c_flag, $
       c_line, delta
nn=size(iin)
nn=nn(1)
iout=intarr(nn)
jout=intarr(nn)
zeros=0
for ii=0,nn-1 do begin
  i=iin(ii) & j=jin(ii)
  if a(i,j) eq 0 then begin
     square, x(i)+0.01*c_flag, y(j)+0.01*c_flag, 1./n-.01-0.02*c_flag, $
             color=c_uncovered
     xyouts, x(i)+delta, y(j)+delta-0.02, strcompress(c(i,j),/remove_all), $
	     alignment=.5, charsize=2, /normal, color=c_line
    ; square, x(i), y(j), 1./n-.01, color=c_uncovered, pattern=c_flag
    ; xyouts, x(i)-delta, y(j)-delta, strcompress(c(i,j),/remove_all), $
    ;         alignment=.5, charsize=2, /normal
     if c(i,j) eq 0 then begin
       iout(zeros)=i
       jout(zeros)=j
       zeros=zeros+1
     endif
     ; mark this tile as being uncovered
     a(i,j)=-1
  endif
endfor

return
end


pro change_color
common my_colors, c_covered, c_uncovered, c_suspect, c_object, c_flag, $
       c_line, delta
square, 0, 0, 1, color=!d.n_colors
square, .1, .1, .18, color=c_covered
square, .3, .3, .18, color=c_object
square, .5, .5, .18, color=c_uncovered
xyouts, .59, .59, '1', alignment=.5, charsize=2, /normal, color=c_line
square, .7, .7, .18, color=c_suspect
xloadct
return
end



pro tiles, in, bw=bw, color=color
common tiles, a, b, c, x, y, n
common my_colors, c_covered, c_uncovered, c_suspect, c_object, c_flag, $
       c_line, delta
; array a: 0 - this tile has not been touched yet
;         -1 - has been turned over
;          1 - has been marked
; array b: 0 - no object under this tile
;          1 - there is a hidden object
; array c: number of neighboring tiles with hiddne objects
;  (x,y) : position of a tile in normalized coordinates

; start up a square window and load a nice colortable
if not keyword_set(in) then in=8
if !d.window eq -1 then if in le 10 then square_window else square_window, /big
if !d.n_colors gt 2 and not keyword_set(bw) then begin 
  c_covered=!d.n_colors*0.55
  c_uncovered=!d.n_colors*0.37
  c_suspect=!d.n_colors*0.46
  c_object=!d.n_colors
  c_line=1
  if keyword_set(color) then change_color else loadct, 22
  c_flag=0
endif else begin
  c_covered=0*!d.n_colors
  c_uncovered=1*!d.n_colors
  c_suspect=c_uncovered
  c_object=1*!d.n_colors
  c_line=0
  c_flag=1
endelse

n=max([2,in])
; x,y contains the left lower corner of each square in normalized coordinates
x=findgen(n)/n+0.005
y=x
delta=(x(1)-0.01-x(0))/2.

; fill all squares with a color indicating "not yet checked"
square, 0, 0, 1, color=!d.n_colors
for i=0,n-1 do for j=0,n-1 do square, x(i), y(j), 1./n-.01, color=c_covered

; startup sets up array a, fills array b with 0 and 1, 1 indicating an object.
; array c will contain the number of neighboring tiles with objects.
startup

print, 'number of objects to find ',total(b)
print, 'left mouse button uncovers a tile, right button marks a tile'
; let the player try his/her luck 
continue=1
!err=0
while continue gt 0 do begin
  ; get cursor position and which button was clicked
  cursor, i, j, /down, /normal 
  i=fix(i*n)
  j=fix(j*n)
  ; for left button do this:
  if !err eq 1 then begin 
    ; if there was no object under the tile, uncover the tile and show
    ; the number of neighboring objects
    if b(i,j) eq 0 then begin
      square, x(i)+0.01*c_flag, y(j)+0.01*c_flag, 1./n-.01-0.02*c_flag, $
              color=c_uncovered
      xyouts, x(i)+delta, y(j)+delta-0.02, strcompress(c(i,j),/remove_all), $
	      alignment=.5, charsize=2, /normal, color=c_line
      ; mark this tile as being uncovered
      a(i,j)=-1
      ; uncover all the trivial neighbors 
      if c(i,j) eq 0 then check_neighbors, i, j
    endif else begin
    ; if there was an object the game is over....
      print, 'Try again...'
      show_all
      return
    endelse
  endif

  ; for right button do this:
  if !err eq 4 then begin
    ; paint the tile to identify it as a suspected object
    square, x(i)+0.02*c_flag, y(j)+0.02*c_flag, 1./n-.01-0.04*c_flag, $
            color=c_suspect
    ; mark the tile as a hidden object
    a(i,j)=1
  endif

; check if all tiles with hidden objects are marked
continue = total(abs(b-(a>0)))

endwhile

print, 'You found them ALL!'
show_all
return
end 
