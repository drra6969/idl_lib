; START OF MAIN PROGRAM

  fname='' & nc=120 & nl=long(16000)
  xd1=long(995) & xd=2*xd1
  im=strarr(long(16000))

  dumm='' 

  print, 'Input filename'
  read, fname
  infile=fname+'.ps' & outfile=fname+'a.ps' 
  print, infile, ',  ',outfile
  openr, 8, infile
  openw, 9, outfile
  readf, 8, dumm & im(0)=dumm
  readf, 8, dumm & im(1)=dumm

;    for iy=0,ny-1 do begin
;     readf, 8, dumm
;    endfor
  close, 8
  close, 9
print, dumm
print, im(0)
print, im(1)
print, im(2)

      
end

