; PROGRAM to display CLUSTER Data and to do variance and other analysis
;----------------------------------------------------------------------
FUNCTION XTICKS, axis, index, value
COMMON plaxis,tax
;print,'tax:',tax
if tax eq 'h' then begin
 hour = LONG(value)
 minute = LONG(60*(value-hour)) 
 sec = 3600*value mod 60
endif
if (tax eq 'm') or (tax eq 'ms') then begin
; print, 'value:',LONG(value)
 hour = LONG(value)/60
 minute = LONG(60*value-3600 * hour) / 60
 sec = 60*value mod 60
endif
if tax eq 's' then begin
 print, 'value:',LONG(value)
 hour = LONG(value)/3600
 minute = LONG(value-3600 * hour) / 60
 sec = value mod 60
endif
RETURN, STRING(hour, minute, sec, $
        FORMAT="(i2.2, ':', i2.2, ':', i2.2)")
END


;START OF MAIN PROGRAM

COMMON bfield, nfgm1,timem1,statsm1,b1,varbt1,varbb1, $
               nfgm2,timem2,statsm2,b2,varbt2,varbb2, $
               nfgm3,timem3,statsm3,b3,varbt3,varbb3, $
               nfgm4,timem4,statsm4,b4,varbt4,varbb4
COMMON pfield,ncis1,timep1,statsp1,np1,no1,nhe1,nhia1,vp1,vhia1,tp1,thia1, $
              ncis3,timep3,statsp3,np3,no3,nhe3,nhia3,vp3,vhia3,tp3,thia3, $
              ncis4,timep4,statsp4,np4,no4,nhe4,nhia4,vp4,vhia4,tp4,thia4 
COMMON sc, naux,time,sccoor,scvel,scdr1,scdr2,scdr3,scdr4,gsegsm,diptild, $
           re, rsc,dxsc,dysc,dzsc,volsc,volrat
COMMON bulk, n1,v1,t1,p1,n3,v3,t3,p3,n4,v4,t4,p4
COMMON current, njc,timec,jc,divb
COMMON plaxis,tax
COMMON test, timeqs,nnn,neqs,veqs,bave
COMMON pltvar, vplot1,vplot3,vplot4,bplot1,bplot2,bplot3,bplot4
   
  withps='n' & closeps='n' & psname='ps' & smo='n'
  whatindex='0' & index=0 & conti='y'& rsfact=20. & shiftfact=100.
  contin='' & again='y' & lastcommand='' & coordstrn='GSM' & pscoord='g' 
  varpresent='n' & htpresent='n'
  re=6371.2
  
  delt = 2.0 &  fdelt = 0.2 & isc=1
  slwal0 = 0.8 & slwal1 = 1.3 &  slht0 = 0.9 & slht1 = 1.1 &  itest=0
  succes=0  & iterror='n' & base=fltarr(3,3)
  base(0,*)=[1.,0.,0.] & base(1,*)=[0.,1.,0.] & base(2,*)=[0.,0.,1.]
  vht=[0.,0.,0.]

; READ DATA
  hreadcl, nameaux,namecis,namefgm

; ELIMINATE BAD DATA
  hnobadb
  hnobadp
           
; ROTATE INTO GSM
  psi = !pi*gsegsm/180.
  hgsetogsmaux, psi,sccoor & hgsetogsmaux,psi,scvel 
  hgsetogsmaux,psi,scdr1   & hgsetogsmaux,psi,scdr2 & hgsetogsmaux,psi,scdr3
  hgsetogsmaux,psi,scdr4
  hgsetogsm,time,psi,timem1,b1 & hgsetogsm,time,psi,timem2,b2
  hgsetogsm,time,psi,timem3,b3 & hgsetogsm,time,psi,timem4,b4
  hgsetogsm,time,psi,timep1,vp1 & hgsetogsm,time,psi,timep3,vp3
  hgsetogsm,time,psi,timep4,vp4 
  hgsetogsm,time,psi,timep1,vhia1 & hgsetogsm,time,psi,timep3,vhia3
  hgsetogsm,time,psi,timep4,vhia4 


; DETERMINE CLUSTER CONFIGURATION PARAMETERS
;  - MAX SEPARATION IN X, Y, AND Z
;  - CLUSTER VOLUME/IDEAL VOLUME
   hclconfig
  
; CURRENT DENSITY
  hcurrent

; BULK PROPERITIES
  hbulk

; DATA STRINGS AND INTERVALS
  hcldate, time, datestrn, strnn
  
  hclint, nfgm1,timem1,nintb1,it1b1,it2b1
  hclint, nfgm2,timem2,nintb2,it1b2,it2b2
  hclint, nfgm3,timem3,nintb3,it1b3,it2b3
  hclint, nfgm4,timem4,nintb4,it1b4,it2b4
  hclint, ncis1,timep1,nintp1,it1p1,it2p1
  hclint, ncis3,timep3,nintp3,it1p3,it2p3
  hclint, ncis4,timep4,nintp4,it1p4,it2p4
  hclint, naux,time,nint,it1,it2
  hclint, njc,timec,nintj,itj1,itj2
  
  tminall=min([timem1(0),timem2(0),timem3(0),timem4(0),$
                        timep1(0),timep3(0),timep4(0)])
  tmaxall=max([timem1(nfgm1-1),timem2(nfgm2-1),timem3(nfgm3-1),timem4(nfgm4-1),$
                              timep1(ncis1-1),timep3(ncis3-1),timep4(ncis4-1)])
  tmin=tminall & tmax=tmaxall
  tvarmin=tmin & tvarmax=tmax
  vartmin=tvarmin & vartmax=tvarmin
  htmin=tvarmin & htmax=tvarmin

; iscvar=isc & ischt=isc
; 3. MENU:
;---------
cut: 
;the line under is moved here from above 
  iscvar=isc & ischt=isc

  if ( ((lastcommand eq 'v') or (lastcommand eq 'w')) $
           and (whatindex eq 'p') ) then lastcommand=lastcommand $
    else lastcommand=whatindex
  iterror='n'
  if withps eq 'y' then  begin 
     withps = 'n' & closeps='n' & device,/close & set_plot,'x'
     !P.THICK=1. & !X.THICK=1. & !Y.THICK=1. & !P.CHARTHICK=1.
  endif
  
;  hprintint,nfgm1,timem1,nintb1,it1b1,it2b1,nfgm2,timem2,nintb2,it1b2,it2b2, $
;            nfgm3,timem3,nintb3,it1b3,it2b3,nfgm4,timem4,nintb4,it1b4,it2b4, $
;            ncis1,timep1,nintp1,it1p1,it2p1,ncis3,timep3,nintp3,it1p3,it2p3, $
;            ncis4,timep4,nintp4,it1p4,it2p4
  print, '   Present time range:',tmin/60., tmax/60.
  print, '   Maximum Time range:',tminall/60., tmaxall/60.
  print, '   Selected test: itest=',itest
  print, '   Required slope for Walenrel (min,max):',slwal0,slwal1
  print, '   Required slope for HT frame(min,max):',slht0,slht1
  print, '   Search interval (min) and increment analysis:',delt,fdelt
  print, '   Sc for analysis analysis:',isc
  if smo eq 'y' then print, 'Smoothing velocity is presently on!'$ 
       else print, 'Smoothing velocity is presently off!'
                         
  print, 'OPTIONS: <return> -> re-plot available data in tmin,tmax'
  print, '               a  -> plot all data'
  print, '               t  -> data interval for tmin, tmax in h'
  print, '               r  -> shift interval right by ',shiftfact,'% delta t'
  print, '               l  -> shift interval  left by ',shiftfact,'% delta t'
  print, '               d  -> change shiftfactor in % of delta t'
  print, '               c  -> contract interval by',rsfact,'% delta t'
  print, '               e  -> expand interval by',rsfact,'% delta t'
  print, '               f  -> change contract/expand factor in % of delta t'
  print, '               i  -> spacecraft to use for variance +other analysis'
  print, '               g  -> smooth on (y) and off (n) (for analysis only)'
  print, '               s  -> Set variance time range to plotrange'
  print, '               u  -> Select variance time different from plotrange'
  print, '               v  -> compute magnetic/electric field variance + coord'
  print, '               x  -> switch to magnetic field variance coord'
  print, '               y  -> switch to electric field variance coord'
  print, '               z  -> switch back to GSM coordinates'
  print, '               w  -> dHT frame and Walen relation'
  print, '               p  -> Postscrip output'
  print, '               q  -> TERMINATE'
  read, whatindex

  if whatindex eq 'q' then stop
  
; FIX TIME INTERVAL
  if whatindex eq '' then begin
    print, 'tmin, tmax:', tmin, tmax
    goto, plotall
  endif
  if whatindex eq 'a' then begin
    tmin=tminall & tmax=tmaxall
    print, 'tmin, tmax:', tmin, tmax
  endif
  if whatindex eq 't' then begin
    print,'Input tmin>=',tminall/60.,'  and tmax<=',tmaxall/60.
    read, tmin,tmax
    tmin=tmin*60. & tmax=tmax*60.
    if((tmin lt tminall) or (tmin ge tmaxall) $
                         or (tmin gt tmax)) then tmin=tminall
    if((tmax le tminall) or (tmax gt tmaxall)) then tmax=tmaxall
    print, 'tmin, tmax:', tmin, tmax
  endif
  if whatindex eq 'r' then begin
    ddelt=shiftfact/100.*(tmax-tmin) & tmin=tmin+ddelt & tmax=tmax+ddelt
    if((tmin lt tminall) or (tmin gt tmaxall)) then tmin=tminall
    if((tmax lt tminall) or (tmax gt tmaxall)) then tmax=tmaxall
  endif
  if whatindex eq 'l' then begin
    ddelt=shiftfact/100.*(tmax-tmin) & tmin=tmin-ddelt & tmax=tmax-ddelt
    if((tmin lt tminall) or (tmin gt tmaxall)) then tmin=tminall
    if((tmax lt tminall) or (tmax gt tmaxall)) then tmax=tmaxall
  endif
  if whatindex eq 'd' then begin
    print,'Present shift factor=',shiftfact,' in % of delta t'
    print,'Input new factor!' & read, shiftfact & goto, cut
  endif
  if whatindex eq 'c' then begin
    ddelt=rsfact/200.*(tmax-tmin) & tmin=tmin+ddelt & tmax=tmax-ddelt
  endif
  if whatindex eq 'e' then begin
    ddelt=rsfact/200.*(tmax-tmin) & tmin=tmin-ddelt & tmax=tmax+ddelt
    if((tmin lt tminall) or (tmin gt tmaxall)) then tmin=tminall
    if((tmax lt tminall) or (tmax gt tmaxall)) then tmax=tmaxall
  endif
  if whatindex eq 'f' then begin
    print,'Present resize factor=',rsfact,' in % of delta t'
    print,'Input new factor!' & read, rsfact & goto, cut
  endif

; DHT AND WALEN TEST Parameters
  if whatindex eq 'i' then begin
     print, 'Old spacraft index:', isc
     print, 'New choice of SC for analysis:'
     read, isc 
     if ((isc ne 1) and (isc ne 3)  and (isc ne 4)) then begin
       print,'isc=',isc,' not possible. CHOOSE different!!' & isc=1
     endif
     goto, cut
  endif 
  if whatindex eq 'g' then begin
     if smo eq 'y' then print, 'Smoothing velocity is presently on!'$ 
       else print, 'Smoothing velocity is presently off!'
     print, 'Switch smoothing on (y) or off (n):'
     read, smo
     if smo ne 'y' then smo='n'
     goto, cut
  endif
  if whatindex eq 's' then begin
     print, 'Analysis time range set to tmin, tmax:', tmin, tmax
     tvarmin=tmin & tvarmax=tmax
     goto, cut
  endif
  if whatindex eq 'u' then begin
     print,'Set analysis time range!'
     print,'Input tmin>=',tminall/60.,'  and tmax<=',tmaxall/60.
     print,'Present plot range', tmin, tmax
     read, tvarmin,tvarmax
     tmin=tmin*60. & tmax=tmax*60.
     if((tmin lt tminall) or (tmin ge tmaxall) $
                          or (tmin gt tmax)) then tmin=tminall
     if((tmax le tminall) or (tmax gt tmaxall)) then tmax=tmaxall
     goto, cut
  endif
  if whatindex eq 'h' then begin
     print, 'Old slope for HT search:', slht0, slht1
     print, 'Input new value:'
     read, slht0, slht1
     goto, cut
  endif
  if ( (whatindex eq 'v') or $
       ((whatindex eq 'p') and (lastcommand eq 'v')) ) then begin
     hcltestvar,timep1,timep3,timep4,ncis1,ncis3,ncis4,$
                tvarmin,tvarmax,isc,smo,iterror
     if (whatindex ne 'p') then  $
       window,1,xsize=600,ysize=480,title='Variance Plot' $
     else begin
       withps='y'
       !P.THICK=2.
       !X.THICK=1.5
       !Y.THICK=1.5
       !P.CHARTHICK=2.
       ih1=fix(tmin/60.) & ih2=fix(tmax/60.)
       im1=fix(tmin-ih1*60) & im2=fix(tmax-ih2*60)
       psname='cl'+datestrn+':'+string(ih1,'(i2.2)')+string(im1,'(i2.2)')$
                         +'_'+string(ih2,'(i2.2)')+string(im2,'(i2.2)')$
                         +'sc'+string(isc,'(i1.1)')+'var'
       set_plot,'ps'
       device,/color,bits=8
       device,filename=psname+'.ps'
       device, /portrait, /times, /bold, font_index=3
       device, /inches, xsize=8., ysize=6.4, scale_factor=0.8, $
                      xoffset=1.0,yoffset=5.3
     endelse
     hclvarb,tvarmin,tvarmax,withps,smo,eb1,eb2,eb3,ee1,ee2,ee3,strnn
     if withps eq 'y' then  begin 
       withps='n' & device,/close & set_plot,'x'
       !P.THICK=1. & !X.THICK=1. & !Y.THICK=1. & !P.CHARTHICK=1. 
     endif    
     varpresent='y'
     vartmin=tvarmin & vartmax=tvarmin
  endif
  if ( (whatindex eq 'w') or $
       ((whatindex eq 'p') and (lastcommand eq 'w')) ) then begin
     hcltestvar,timep1,timep3,timep4,ncis1,ncis3,ncis4,$
                tvarmin,tvarmax,isc,smo,iterror
     if (whatindex ne 'p') then  $
       window,2,xsize=600,ysize=480,title='Walen and dHT Plot' $
     else begin
       withps='y'
       !P.THICK=2.
       !X.THICK=1.5
       !Y.THICK=1.5
       !P.CHARTHICK=2.
       ih1=fix(tmin/60.) & ih2=fix(tmax/60.)
       im1=fix(tmin-ih1*60) & im2=fix(tmax-ih2*60)
       psname='cl'+datestrn+':'+string(ih1,'(i2.2)')+string(im1,'(i2.2)')$
                         +'_'+string(ih2,'(i2.2)')+string(im2,'(i2.2)')$
                         +'sc'+string(isc,'(i1.1)')+'wht'
       set_plot,'ps'
       device,/color,bits=8
       device,filename=psname+'.ps'
       device, /portrait, /times, /bold, font_index=3
       device, /inches, xsize=8., ysize=6.4, scale_factor=0.65, $
                      xoffset=1.8,yoffset=0.8
     endelse
     hcltestwal,tvarmin,tvarmax,withps,smo,strnn, vht
     if withps eq 'y' then  begin 
       withps='n' & device,/close & set_plot,'x'
       !P.THICK=1. & !X.THICK=1. & !Y.THICK=1. & !P.CHARTHICK=1.     
     endif    
     htpresent='y'
     htmin=tvarmin & htmax=tvarmin
  endif
  if (whatindex eq 'x') then begin
     if varpresent  ne 'y' then begin
       print,'No variance coord present. Determine variance first!!!'
       goto, cut & endif
     coordstrn='Var B Coord.' & pscoord='b'
     base(0,*)=eb1 & base(1,*)=eb2 & base(2,*)=eb3
  endif
  if (whatindex eq 'y') then begin
     if varpresent  ne 'y' then begin
       print,'No variance coord present. Determine variance first!!!'
       goto, cut & endif
     coordstrn='Var E Coord.' & pscoord='e'
     base(0,*)=ee1 & base(1,*)=ee2 & base(2,*)=ee3
  endif
  if (whatindex eq 'z') then begin
     coordstrn='GSM' & pscoord='g'
     base(0,*)=[1.,0.,0.] & base(1,*)=[0.,1.,0.] & base(2,*)=[0.,0.,1.]
  endif

  if whatindex eq 'p' then begin
     withps = 'y'
     !P.THICK=2.
     !X.THICK=1.5
     !Y.THICK=1.5
     !P.CHARTHICK=2.
     ih1=fix(tmin/60.) & ih2=fix(tmax/60.)
     im1=fix(tmin-ih1*60) & im2=fix(tmax-ih2*60)
     psname='cl'+datestrn+':'+string(ih1,'(i2.2)')+string(im1,'(i2.2)')$
                         +'_'+string(ih2,'(i2.2)')+string(im2,'(i2.2)')$
                         +'sc'+string(isc,'(i1.1)')+pscoord
     set_plot,'ps'
     device,/color,bits=8
     device,filename=psname+'.ps'
     device, /landscape, /times, /bold, font_index=3
     device, /inches, xsize=10., ysize=8., scale_factor=0.95, $
                      xoffset=0.5,yoffset=10.1
  endif

plotall:
  hclplotvar,coordstrn,base
  if (withps ne 'y') then  window,0,xsize=900,ysize=720,title='Overview Plot'
  hrplotvarsum, tmin, tmax, strnn,withps,coordstrn,base,eb1,eb2,eb3,vht,ischt,$
                htpresent,varpresent,isc,iscvar
goto, cut

  
end
     
