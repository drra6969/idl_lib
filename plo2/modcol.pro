; START OF MAIN PROGRAM
;
; program to develob basic color tables

   n=17
   red=findgen(n) & green=red & blue=red
   choice='i' & choice0=choice
   igrid=1 & ig0=igrid & igval=''
   idval='111' & redval=0.5 
   isave=0 & fout=''
   fname='colpar' & fin=''
   ctno='' & ctnum=40 & ctnom='' 

   r = findgen(256) & g=r & b=r

 tvlct ,r1,g1,b1, /get
 print,'size(red): ', size(r1)
 tvscl, dist(400)
 tt=findgen(200,400)
 tvscl, tt

   nmax = !D.TABLE_SIZE
   print, 'table size: ',!D.TABLE_SIZE
   print, 'number of colors: ',!D.N_COLORS
   print, 'default color: ',!P.COLOR

   r = findgen(256) & del=(nmax-1)/float(255) & r=r*del
   r(255)=float(nmax-1)  &      g=r & b=r
   r = interpolate(r1,r)
   g = interpolate(g1,g)
   b = interpolate(b1,b)
print,r1
print,r

  print, 'Save to file? <y> for yes'
  read, choice

  if choice eq 'y' then begin
    rm=bytscl(r) & gm=bytscl(g) & bm=bytscl(b)
    ctno=ctno+1
    print, 'Input number of colortable, default: ', ctnum
    read, ctno & if (ctno ne '') then ctnum=fix(ctno)
    ctname='colortab'+string(ctno,'(i3.3)')
    print, 'Input name of colortable, default: ', ctname
    read, ctnom & if (ctnom ne '') then ctname=ctnom
    modifyct, file='/users/ao/idl_lib/colortab.priv', ctnum, ctname, rm,gm,bm
  endif


end
