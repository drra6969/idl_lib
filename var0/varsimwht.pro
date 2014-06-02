; PROGRAM to read and plot satellite data
;----------------------------------------
COMMON procommon, nsat,startime,itot,pi,ntmax, $
                  nnorm,bnorm,vnorm,pnorm,lnorm,tnorm, $
                  nfac,bfac,vfac,pfac,lfac,tfac,tempfac, $
                  rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd, $
                  xsi,ysi,zsi,vxsi,vysi,vzsi, $
                  time,bxs,bys,bzs,vxs,vys,vzs,rhos,ps,bs,ptots, $
                  jxs,jys,jzs,xs,ys,zs,cutalong
COMMON ref, bxr,byr,bzr,vxr,vyr,vzr, $
            rhor,pr,babr,ptotr,pbr,tempr,beta,t, $
            xsat1,ysat1,zsat1,vxsat1,vysat1,vzsat1, $
            xsat2,ysat2,zsat2,vxsat2,vysat2,vzsat2, $
            index, starttime,xtit, withps
COMMON simdat3, nx,ny,nz,x,y,z,dx,dy,dz,bx,by,bz,vx,vy,vz,rho,u,res,p,$
                jx,jy,jz,xmin,xmax,ymin,ymax,zmin,zmax,linealong,icut

  nsat = long(60) & ntmax = 4001 & pi = 3.1415926553
  startime = 0.0 & time = fltarr(ntmax)
  time2d=0.0 
  cutalong='x' & linealong='y' & icut=1
  
  coor = 's' & withunits='n' & satchoice='b'
  whatindex='0' & index=0 & conti='y' 
  contin='' & again='y' & withps='n' & closeps='n' & psname='ps' 
  countps=0 & wcount=0 & scount=0 & hcount=0 & rcount=0 
  fall = '' & change='' & cas=''
  xtit='time (s)'   &   plotvars='t' & vardat='absent'
  ebasis='b' & ebst='B'
  evo1=[1.,0.,0.] & evo2=[0.,1.,0.] & evo3=[0.,0.,1.] & ewo=[1.,1.,1.]

  slwal0 = 0.8 & slwal1 = 1.3 &  slht0 = 0.9 & slht1 = 1.1 &  itest=0
  succes=0  & iterror='n' & bntest=0 & satraj=1

reads:
; READ INPUT DATA FOR NSAT SATELLITES
;------------------------------------
; determines: nsat,startime,itot,nnorm,bnorm,vnorm,pnorm,lnorm,tnorm,$
;              rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd,$
;              xsat0,ysat0,zsat0,vxsat,vysat,vzsat,$
;              time,bxs,bys,bzs,vxs,vys,vzs,rhos,ps,bs,ptots,xs,ys,zs
  print, 'Choose format of sat data set: obtions:'
  print, 'Default choice:           b -> binary simulation data'
  print, '                          2 -> 2D grid'
  print, '                          3 -> 3D grid'
  read, satchoice
  if satchoice eq '' or satchoice eq 'b' then readsat
  if satchoice eq '2' then begin
     read2sim,time2d
     print,'cutalong=',cutalong
  endif
  if satchoice eq '3' then  begin
    read3sim,time2d
    cut3d
    print,'cutalong=',cutalong,'  linealong=',linealong,'cut at ',icut
  endif
  if (satchoice ne '') and (satchoice ne 'b') and (satchoice ne '2') $
    and (satchoice ne '3')    then goto, reads
  if satchoice eq '2' then xtit='x'

;  NORMALISATION  
;-----------------
  parnorm,satchoice,withunits     

  tminall=time(0)
  tmaxall=time(itot-1)
  tmin=tminall & tmax=tmaxall


    t=time  &  it1=0 & it2=itot-1 & nt=itot
    iv1=0 & iv2=itot-1 & nv=itot & tvmin=t(0) & tvmax=t(iv2)
    xr=t & yr=t & zr=t & bxr=t & byr=t & bzr=t & vxr=t & vyr=t & vzr=t
    rhor=t & pr=t & br=t & ptotr=t & tempr=t
; SIMULATION COORDINATES:
; Boundary values:                  rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd
; Probe coordinates and positions:  xsi,ysi,zsi,vxsi,vysi,vzsi
; Probe data:                       time, bxs, bys, bzs, vxs, vys, vzs,
;                                   rhos, ps, bs, ptots, xs, ys, zs
  
; Open some windows before starting menues:
  window,0,xsize=640,ysize=512,title='Overview Plot'
  window,1,xsize=640,ysize=512,title='Variance Plots'
  window,2,xsize=640,ysize=512,title='Var Data'

; declare some variable to record and print boundary values
  bmsp=fltarr(3) & bmsh=bmsp & vmsp=bmsp & vmsh=bmsp 
  bmsp(0)=bfac*bxbd(0) & bmsp(1)=bfac*bybd(0) & bmsp(2)=bfac*bzbd(0) 
  bmsh(0)=bfac*bxbd(1) & bmsh(1)=bfac*bybd(1) & bmsh(2)=bfac*bzbd(1) 
  vmsp(0)=0.0 & vmsp(1)=0.0 & vmsp(2)=0.0
  vmsh(0)=vfac*(vxbd(1)-vxbd(0))
  vmsh(1)=vfac*(vybd(1)-vybd(0))
  vmsh(2)=vfac*(vzbd(1)-vzbd(0))
  rhotmsp=fltarr(2) & rhotmsh=rhotmsp
  rhotmsp(0)=nfac*rhobd(0) & rhotmsp(1)=tempfac*pbd(0)/rhobd(0)
  rhotmsh(0)=nfac*rhobd(1) & rhotmsh(1)=tempfac*pbd(1)/rhobd(1)
  
satindex:
  parprint
    
cut: 
  if withps eq 'y' then  begin 
    print, 'Postscript device is still open! Close postscript device?',$
                ' <y> or <n>'
    read, closeps
    if closeps eq 'y' then begin
         withps = 'n' & closeps='n' & device,/close & set_plot,'x'
         !P.THICK=1. & !X.THICK=1. & !Y.THICK=1. & !P.CHARTHICK=1.
    endif
  endif
  print, '   Present time(plot) range:',tmin., tmax
  print, '   Maximum Time(plot) range:',tminall, tmaxall
  print, '   Selected test: itest=',itest
  print, '   Selected variance method=',bntest
  print, '   Required slope for Walenrel (min,max):',slwal0,slwal1
  print, '   Required slope for HT frame(min,max):',slht0,slht1
  print, '   Search interval (min) and increment analysis:',delt,fdelt
  if smo eq 'y' then print, 'Smoothing velocity is presently on!'$ 
       else print, 'Smoothing velocity is presently off!'
  
  print, 'CHOOSE PROBE INDEX: '
  print, 'Present choices:  Probe index ='+string(index,'(i3)'))
  if unitype eq 's' then print, '        Simulation units' else $
                         print, '        Physical units'
  print, '        Time range for overview plots (t)   :',tmin, tmax
  print, '        Time range for variance analysis (v):',tvmin, tvmax
  print, '        Variance plots use time range:  ', plotvars
  print, '        Variance plots (h) and (r) use basis from:  ', ebst
                         
  print, 'OPTIONS:   integer  -> Probe index'
  print, '            return  -> No changes applied'
  print, '                 i  -> Show probe indices and parameters again'
  print, '                 n  -> Normalized units'
  print, '                 o  -> Physical units'
  print, '                 p  -> Postscrip output'
  print, '                 t  -> Time(data) range for plots'
  print, '                 tt  -> Set time range back to total range'

  print, '                 d  -> change shiftfactor in % of delta t'
  print, '                 c  -> contract interval by',rsfact,'% delta t'
  print, '                 e  -> expand interval by',rsfact,'% delta t'
  print, '                 f  -> change contract/expand factor in % of delta t'
  print, '                 g  -> smooth on (y) and off (n) (for analysis only)'
  print, '                 s  -> Start search and plot results'
  print, '                 u  -> Time period and increment for search'
  print, '                 h  -> Change required slope for HT frame'
  print, '                 w  -> Change required slope for Walen relation'
  print, '                 m  -> Choose test mode: i=0 -> Walen rel and HT frame'
  print, '                                         i=1 -> Walen rel only'
  print, '                                         i=2 -> HT frame only'
  print, '                 b  -> define necessary delta b'
  print, '                 bn -> define variance test:i=0 -> b-field'
  print, '                                            i=1 -> e-field'
  print, '                 q  -> TERMINATE'
  read, whatindex
  if whatindex eq '' then print,'index=',index,' not altered'
  if whatindex eq 'i' then goto, satindex
  if whatindex eq 'n' then begin
    withunits='n' & parnorm
  endif
  if whatindex eq 'o' then begin
    withunits='y' & parnorm
    t=tfac*time
  endif
  if whatindex eq 'p' then begin
     withps = 'y'
     !P.THICK=2.
     !X.THICK=1.5
     !Y.THICK=1.5
     !P.CHARTHICK=2.
     psname='satp'+string(index,'(i2.2)')+'p'+string(countps,'(i2.2)')
     countps=countps+1 & set_plot,'ps'
  endif
  if whatindex eq 't' then begin
     timerange, time,tfac, tmin,tmax,it1,it2,nt,success
     if success eq 'none' then goto, cut
     if (vardat eq 'present') and (plotvars eq 't') then goto, plotvart
  endif
  if whatindex eq 'tt' then begin
     tmin=tfac*time(0) & tmax=tfac*time(itot-1)
  endif
    if whatindex eq 'q' then stop
 






  if (vardat ne 'present') and $
    ( (whatindex eq 'vt') or (whatindex eq 'vv') or (whatindex eq 's') or $
      (whatindex eq 'h') or (whatindex eq 'r') or (whatindex eq 'bb') or $
      (whatindex eq 'be') or (whatindex eq 'beh') or (whatindex eq 'bv') or $
      (whatindex eq 'w') )   then begin
      print, 'Determine Variance data first!!!! -> v'
      goto, cut
  endif
  
  if ( (whatindex ne '') and (whatindex ne 'a') and (whatindex ne 'i') $
     and (whatindex ne 'n') and (whatindex ne 'o') and (whatindex ne 'p') $
     and (whatindex ne 't') and (whatindex ne 'u') and (whatindex ne 'v') $
     and (whatindex ne 'vt') and (whatindex ne 'vv') and (whatindex ne 's') $
     and (whatindex ne 'h') and (whatindex ne 'r') and (whatindex ne 'bb') $
     and (whatindex ne 'be') and (whatindex ne 'beh') and (whatindex ne 'bv') $
     and (whatindex ne 'bo') $
     and (whatindex ne 'w') and (whatindex ne 'q') )    then begin
    index=fix(whatindex)
    print, 'Chosen satellite index: ',index
    if index gt nsat-1 then begin 
       print, 'Probe index is too large -> return to menue!'
    endif  
    vardat='absent'
  endif
  
  
; Determine the actual data set to be analyzed and give it physical 
;     (or normalized) units
; 
  bxr=bfac*bxs1(index,*)  & byr=bfac*bys1(index,*) & bzr=bfac*bzs1(index,*)
  vxr=vfac*vxs1(index,*)  & vyr=vfac*vys1(index,*) & vzr=vfac*vzs1(index,*)
  rhor=nfac*rhos(index,*) & pr=pfac*ps(index,*)    & babr=bfac*bs(index,*)  
  ptotr=pfac*ptots(index,*) & pbr=pfac*bs(index,*)*bs(index,*) 
  tempr=tempfac*ps(index,*)/rhos(index,*) & beta=ps(index,*)/bs(index,*)^2
  t=tfac*time
  efield, withunits,vxr,vyr,vzr, bxr,byr,bzr, exr,eyr,ezr
; Same for the boundary values
  bmsp(0)=bfac*bxbd(0) & bmsp(1)=bfac*bybd(0) & bmsp(2)=bfac*bzbd(0) 
  bmsh(0)=bfac*bxbd(1) & bmsh(1)=bfac*bybd(1) & bmsh(2)=bfac*bzbd(1) 
  vmsp(0)=0.0 & vmsp(1)=0.0 & vmsp(2)=0.0
  vmsh(0)=vfac*(vxbd(1)-vxbd(0))
  vmsh(1)=vfac*(vybd(1)-vybd(0))
  vmsh(2)=vfac*(vzbd(1)-vzbd(0))
  rhotmsp(0)=nfac*rhobd(0) & rhotmsp(1)=tempfac*pbd(0)/rhobd(0)
  rhotmsh(0)=nfac*rhobd(1) & rhotmsh(1)=tempfac*pbd(1)/rhobd(1)
  if (satchoice eq '2' or satchoice eq '3') then  $
      strnn=cas+'Cut at z= '+string(zsi(index),'(f5.1)')$
                 +', time='+string(time2d,'(f5.0)')


; CARRY OUT THE VARIANCE ANALYSIS
; -------------------------------
  if (whatindex eq 'v') then begin
    timerange, time,tfac, tvmin,tvmax,iv1,iv2,nv,success
    if success eq 'none' then goto, cut
    vardat='present'
    print, 'Variance Analysis for the Time Range:', tvmin, tvmax
    print, '  Number of data points:', nv
    strnn='!17'+cas+'Probe '+string(index,'(i2.2)'))
    if (satchoice eq '2' or satchoice eq '3') then  $
      strnn='!17'+cas+'Cut at z= '+string(zsi(index),'(f5.1)')$
                 +', time='+string(time2d,'(f5.0)')
    print,strnn
    tv=t(iv1:iv2)
; Variance for V, B, and E
    vxv=vxr(iv1:iv2) & vyv=vyr(iv1:iv2) & vzv=vzr(iv1:iv2)
    bxv=bxr(iv1:iv2) & byv=byr(iv1:iv2) & bzv=bzr(iv1:iv2)
    exv=exr(iv1:iv2) & eyv=eyr(iv1:iv2) & ezv=ezr(iv1:iv2)
    varmat,nv,vxv,vyv,vzv,vav,vmat
    varmat,nv,bxv,byv,bzv,bav,bmat
    varmat,nv,exv,eyv,ezv,eav,emat
    eigen, vmat,evv1,evv2,evv3,ewv
    eigen, bmat,evb1,evb2,evb3,ewb
    eigen, emat,eve1,eve2,eve3,ewe
    sorteigenb, evb1,evb2,evb3
    sorteigen, evb1,evb2,evb3,evv1,evv2,evv3
    sorteigen, evb1,evb2,evb3,eve1,eve2,eve3
; Hoffmann Teller velocity and variance for Eht
    htcoor, nv, vxv,vyv,vzv, bxv,byv,bzv, vht
    ehtfield, withunits,vht,bxr,byr,bzr,ehxr,ehyr,ehzr
    ehxv=ehxr(iv1:iv2) & ehyv=ehyr(iv1:iv2) & ehzv=ehzr(iv1:iv2)
    varmat,nv,ehxv,ehyv,ehzv,ehav,ehmat
    eigen, ehmat,eveh1,eveh2,eveh3,eweh
    sorteigen, evb1,evb2,evb3,eveh1,eveh2,eveh3
; Variance for Vht
    vmhtx=vxv-vht(0)  & vmhty=vyv-vht(1)  & vmhtz=vzv-vht(2)
    varmat,nv,vmhtx,vmhty,vmhtz,vmhtav,vmhtmat
    eigen, vmhtmat,evmhtv1,evmhtv2,evmhtv3,ewmhtv
    sorteigen, evb1,evb2,evb3,evmhtv1,evmhtv2,evmhtv3
; Some stuff for plots testing the walen relation and the HT frame
    vmhp=fltarr(nv,3) & vmhp(*,0)=vmhtx & vmhp(*,1)=vmhty  & vmhp(*,2)=vmhtz
    valfv=vmhp 
    valfv(*,0)=bxv/sqrt(rhor(iv1:iv2))
    valfv(*,1)=byv/sqrt(rhor(iv1:iv2))
    valfv(*,2)=bzv/sqrt(rhor(iv1:iv2))
    if withunits eq 'p' then valfv=vafact*valfv
    ewh=vmhp & ewh(*,0)=exv & ewh(*,1)=eyv &  ewh(*,2)=ezv
    ewht=vmhp & ewht(*,0)=ehxv & ewht(*,1)=ehyv &  ewht(*,2)=ehzv
; Testing the walen relation and the HT frame
    if withps eq 'n' then begin 
      wset,1
      walenplot, tv,vmhp,valfv,ewh,ewht,vht,tv(0),tv(nv-1),ccoef0,cstd,creg,strnn
      wset,2
      printpl, nv, tvmin,tvmax,evv1,evv2,evv3,ewv,vav,evb1,evb2,evb3,ewb,bav,$
             eve1,eve2,eve3,ewe,eav,eveh1,eveh2,eveh3,eweh,ehav,vht,$
             bmsp,bmsh,vmsp,vmsh,rhotmsp,rhotmsh,ccoef0,cstd,creg,strnn
    endif
; Determine actual velocity and field components for variance plots        
    if ebasis eq 'v' then begin
       ebst='V' & eb1=evv1 & eb2=evv2 & eb3=evv3 & eb=ewv & endif
    if ebasis eq 'b' then begin
       ebst='B' & eb1=evb1 & eb2=evb2 & eb3=evb3 & eb=ewb & endif
    if ebasis eq 'e' then begin
       ebst='E' & eb1=eve1 & eb2=eve2 & eb3=eve3 & eb=ewe & endif
    if ebasis eq 'eh' then begin
       ebst='Eht' & eb1=eveh1 & eb2=eveh2 & eb3=eveh3 & eb=eweh & endif
    if ebasis eq 'o' then begin
       ebst='Orig' & eb1=evo1 & eb2=evo2 & eb3=evo3 & eb=ewo & endif
    if plotvars eq 'v' then goto, plotvarv
    if plotvars eq 't' then goto, plotvart
    goto, cut
  endif
  
  
  if (whatindex eq 'vt') then begin
plotvart:
    plotvars='t'
    np=nt
    vp=fltarr(np,3) & bp=vp & ep=vp & ehp=vp & rhop=fltarr(np)
    tp=t(it1:it2)  & rhop=rhor(it1:it2)
    vp(*,0) =vxr(it1:it2)  & vp(*,1) =vyr(it1:it2)  &  vp(*,2)=vzr(it1:it2)
    bp(*,0) =bxr(it1:it2)  & bp(*,1) =byr(it1:it2)  &  bp(*,2)=bzr(it1:it2)
    ep(*,0) =exr(it1:it2)  & ep(*,1) =eyr(it1:it2)  &  ep(*,2)=ezr(it1:it2)
    ehp(*,0)=ehxr(it1:it2) & ehp(*,1)=ehyr(it1:it2) &  ehp(*,2)=ehzr(it1:it2)
    goto, cut
  endif


  if (whatindex eq 'vv') then begin
plotvarv:
    plotvars='v'
    np=nv
    vp=fltarr(np,3) & bp=vp & ep=vp & ehp=vp & rhop=fltarr(np)
    tp=tv  & rhop=rhor(iv1:iv2)
    vp(*,0)=vxv & vp(*,1)=vyv &  vp(*,2)=vzv
    bp(*,0)=bxv & bp(*,1)=byv &  bp(*,2)=bzv
    ep(*,0)=exv & ep(*,1)=eyv &  ep(*,2)=ezv
    ehp(*,0)=ehxv & ehp(*,1)=ehyv &  ehp(*,2)=ehzv
    goto, cut
  endif


; Determine basis for variance plots
  if (whatindex eq 'bv') then begin
    ebasis='v' & ebst='V' & eb1=evv1 & eb2=evv2 & eb3=evv3 & eb=ewv & goto, cut 
  endif
  if (whatindex eq 'bb') then begin
    ebasis='b' & ebst='B' & eb1=evb1 & eb2=evb2 & eb3=evb3 & eb=ewb & goto, cut
  endif
  if (whatindex eq 'be') then begin
    ebasis='e' & ebst='E' & eb1=eve1 & eb2=eve2 & eb3=eve3 & eb=ewe & goto, cut
  endif
  if (whatindex eq 'beh') then begin
    ebasis='eh' & ebst='Eht' & eb1=eveh1 & eb2=eveh2 & eb3=eveh3 & eb=eweh 
    goto, cut
  endif
  
  if (whatindex eq 'bo') then begin
    ebasis='o' & ebst='Orig' & eb1=evo1 & eb2=evo2 & eb3=evo3 & eb=ewo 
    goto, cut
  endif

  if (whatindex eq 'w') then begin
    if withps eq 'y' then begin & device,/close & set_plot,'x' & endif
    wset,1
    walenplot, tv,vmhp,valfv,ewh,ewht,vht,tv(0),tv(nv-1),ccoef0,cstd,creg,strnn
    wset,2
    printpl, nv, tvmin,tvmax,evv1,evv2,evv3,ewv,vav,evb1,evb2,evb3,ewb,bav,$
             eve1,eve2,eve3,ewe,eav,eveh1,eveh2,eveh3,eweh,ehav,vht,$
             bmsp,bmsh,vmsp,vmsh,rhotmsp,rhotmsh,ccoef0,cstd,creg,strnn
    if withps eq 'y' then begin
      set_plot,'ps' & wcount=wcount+1
      device,filename=psname+'w'+string(wcount,'(i2.2)')+'.ps'
      device, /inches, xsize=8., ysize=4., scale_factor=1.0, xoffset=0.5
      device, /landscape, /times, /bold, font_index=3
      walenplot, tv,vmhp,valfv,ewh,ewht,vht,tv(0),tv(nv-1),ccoef0,cstd,creg,strnn
      printpl, nv, tvmin,tvmax,evv1,evv2,evv3,ewv,vav,evb1,evb2,evb3,ewb,bav,$
             eve1,eve2,eve3,ewe,eav,eveh1,eveh2,eveh3,eweh,ehav,vht,$
             bmsp,bmsh,vmsp,vmsh,rhotmsp,rhotmsh,ccoef0,cstd,creg,strnn
    endif
    goto, cut
  endif
  
   
  itv1=iv1-it1 & itv2=iv2-it1 
; Standard (predetermined) variance plots
  if (whatindex eq 's') then begin
    if withps eq 'n' then wset,1
    if withps eq 'y' then begin
      device,/close & set_plot,'ps' & scount=scount+1
      device,filename=psname+'s'+string(scount,'(i2.2)')+'.ps'
      device, /inches, xsize=8., ysize=4., scale_factor=1.0, xoffset=0.5
      device, /landscape, /times, /bold, font_index=3
    endif
   !p.multi=[0,2,2,0,0]    
    hodo, np,tp,tvmin,tvmax,itv1,itv2,bp,evb1,evb2,evb3,ewb,$
          'Magnetic Field','B',strnn,0,vht
    hodo, np,tp,tvmin,tvmax,itv1,itv2,vp,evv1,evv2,evv3,ewv,$
          'Velocity','V',strnn,1,vht
    print,'Continue with electric field ?'
    read, conti
    if (conti eq 'y') or (conti eq '') then begin
      !p.multi=[0,2,2,0,0]    
      hodo, np,tp,tvmin,tvmax,itv1,itv2,ep,eve1,eve2,eve3,ewe,$
          'Electric Field','E',strnn,0,vht
      hodo, np,tp,tvmin,tvmax,itv1,itv2,ehp,eveh1,eveh2,eveh3,eweh,$
          'HT Electric Field','Eht',strnn,1,vht
    endif
    print,'Continue with regular plot?'
    read, conti
    if (conti eq 'y') or (conti eq '') then  begin
      !p.multi=[0,1,3,0,0]    
      plobve, np,tp,tvmin,tvmax,bp,vp,ep,eb1,eb2,eb3,eb,ebst,strnn,xtit,vht
    endif
    goto, cut
  endif
  
    
; Hodograms in variance coordinates
  if (whatindex eq 'h') then begin
    if withps eq 'n' then wset,1
    if withps eq 'y' then begin
      device,/close & set_plot,'ps' & hcount=hcount+1
      device,filename=psname+'h'+string(hcount,'(i2.2)')+'.ps'
      device, /inches, xsize=8., ysize=4., scale_factor=1.0, xoffset=0.5
      device, /landscape, /times, /bold, font_index=3
    endif
   !p.multi=[0,2,2,0,0]    
    hodo, np,tp,tvmin,tvmax,itv1,itv2,bp,eb1,eb2,eb3,eb,$
          'Magnetic Field',ebst,strnn,0,vht
    hodo, np,tp,tvmin,tvmax,itv1,itv2,vp,eb1,eb2,eb3,eb,$
          'Velocity',ebst,strnn,1,vht
    print,'Continue with electric field plots?'
    read, conti
    if (conti eq 'y') or (conti eq '') then begin
      !p.multi=[0,2,2,0,0]    
      hodo, np,tp,tvmin,tvmax,itv1,itv2,ep,eb1,eb2,eb3,eb,$
          'Electric Field',ebst,strnn,0,vht
      hodo, np,tp,tvmin,tvmax,itv1,itv2,ehp,eb1,eb2,eb3,eb,$
          'HT Electric Field',ebst,strnn,1,vht
    endif
    goto, cut
  endif
  
    
; Regular variance plots
  if (whatindex eq 'r') then begin
    if withps eq 'n' then wset,1
    if withps eq 'y' then begin
      device,/close & set_plot,'ps' & rcount=rcount+1
      device,filename=psname+'r'+string(rcount,'(i2.2)')+'.ps'
      device, /inches, xsize=8., ysize=4., scale_factor=1.0, xoffset=0.5
      device, /landscape, /times, /bold, font_index=3
    endif
    !p.multi=[0,1,3,0,0]    
    plobve, np,tp,tvmin,tvmax,bp,vp,ep,eb1,eb2,eb3,eb,ebst,strnn,xtit,vht
    goto, cut
  endif
  

; Finally the overview plots:
; for simulation frame
  xsat1=xsi(index)    & ysat1=ysi(index)   & zsat1=zsi(index) 
  vxsat1=vxsi(index)+vxbd(1)   & vysat1=vysi(index)+vybd(1) 
  vzsat1=vzsi(index)+vzbd(1) 
; for rotated, magnetospheric frame with physical units
  xsat2=lnorm*xsi(index) & ysat2=lnorm*ysi(index) & zsat2=lnorm*zsi(index) 
  vxsat2=vnorm*vxsi(index) & vysat2=vnorm*vysi(index)
  vzsat2=vnorm*vzsi(index) 

  if withps eq 'n' then wset,0
  if withps eq 'y' then begin
     device,/close & set_plot,'ps' & device,filename=psname+'.ps'
     device,/portrait
     device,/inches,xsize=8.,ysize=10.,scale_factor=1.0,xoffset=0.5,yoffset=0.5
     !P.THICK=2. & !X.THICK=1.5 & !Y.THICK=1.5 & !P.CHARTHICK=2.
  endif
  pltsimvar, itot,tmin,tmax,time2d,satchoice,strnn,cutalong
  goto, cut

end
