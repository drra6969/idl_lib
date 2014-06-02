pro les
@globan3
NX = 1l & NY = 1l & NZ = 1l &NZZZ = 1l&ANZPOS = 1l & ANZROW = 1l
NV = 1l
ISCHRITT = 1l & IAUS = 1l & ISAFE = 1l & IEND = 1l
ITART = 1l & IVISRHO = 1l & NGLATT = 1l & NAUS = 1l
IFEHLER = 1l & IGRID = 1l & IDIAG = 1l
ISAT = 1l & fnumber=1

DT = 0.1 & ZEIT = 0.1 & GAMMA = 0.1 & ETA = 0.1 & RHON0 = 0.1 
XMIN = 0.1 & XMAX = 0.1 & YMIN = 0.1 & YMAX = 0.1 & ZMIN = 0.1 & ZMAX = 0.1
FEHLER = 0.1
AEQUI = FLTARR(3)
ZENTR = FLTARR(3)
PERIO = LINDGEN(3)
LSYM = LINDGEN(6)
FILESTATUS = FSTAT(14)
ZRANGE=FLTARR(2)
IF FILESTATUS.OPEN NE 0 THEN CLOSE, 14
PRINTFILE ='test.ps'
  print, 'Input filenumber'
  read, fnumber

altertitel =  'old title'
x1alt=1l & x2alt=2l & y1alt=1l & y2alt=2l
gittschon = 'n'
CFILE = 'NAGTAP'+string(fnumber,'(i1)')

OPENR, 14, CFILE, /F77_UNFORMATTED
READU, 14, NX,NY,NZ,ZEIT,NZZZ
                     

NV = NX * NY

BX = FLTARR (NX,NY,NZ, /NOZERO)
BY = FLTARR (NX,NY,NZ, /NOZERO)
BZ = FLTARR (NX,NY,NZ, /NOZERO)

SNX = FLTARR (NX,NY,NZ, /NOZERO)
SNY = FLTARR (NX,NY,NZ, /NOZERO)
SNZ = FLTARR (NX,NY,NZ, /NOZERO)

RHON =  FLTARR (NX,NY,NZ, /NOZERO)
UN =    FLTARR (NX,NY,NZ, /NOZERO)
RES =  FLTARR (NX,NY,NZ, /NOZERO)
PROF = FLTARR (NX,NY,NZ, /NOZERO)
NU12S =  FLTARR (NX,NY,NZ, /NOZERO)
NU21S = FLTARR (NX,NY,NZ, /NOZERO)


XVEC = FLTARR (NV)
YVEC = FLTARR (NV)
ZVEC = FLTARR (NZ)
EPS = FLTARR (3)
DIFX = FLTARR (NV)
DIFY = FLTARR (NV)
DIFZ = FLTARR (NZ)
DDIFX = FLTARR (NV)
DDIFY = FLTARR (NV)
DDIFZ = FLTARR (NZ)
DDIFPX = FLTARR (NV)
DDIFPY = FLTARR (NV)
DDIFPZ = FLTARR (NZ)
DDIFMX = FLTARR (NV)
DDIFMY = FLTARR (NV)
DDIFMZ = FLTARR (NZ)
MEANMX = FLTARR (NV)
MEANMY = FLTARR (NV)
MEANMZ = FLTARR (NZ)
MEANPX = FLTARR (NV)
MEANPY = FLTARR (NV)
MEANPZ = FLTARR (NZ)
X      = FLTARR (NX)
Y      = FLTARR (NY)
Z      = FLTARR (NZ)

READU, 14, XVEC,DIFX,DDIFX,DDIFPX,DDIFMX,MEANPX,MEANMX,$
           YVEC,DIFY,DDIFY,DDIFPY,DDIFMY,MEANPY,MEANMY,$
           ZVEC,DIFZ,DDIFZ,DDIFPZ,DDIFMZ,MEANPZ,MEANMZ
READU, 14, SNX,SNY,SNZ
READU, 14, RHON,UN,NU12S,NU21S
READU, 14, ISCHRITT,IAUS,ISAFE,IEND,DT,GAMMA,ETA,RHON0
READU, 14, XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX           

X = XVEC(0:NX-1)
Z = ZVEC

I = 1
FOR I=0,NY-1 DO Y(I) = YVEC(I*NX)

g_feld  = make_array(nx,ny,nz, /float)
	norm_koord = 'z'
	ax_n=30
	az_n=20
	nlev_n = 20
	if nz eq 1 then iz_n = 0 $
	      else iz_n = 1
	;iz_n = (nz+1)/2
g_feld1  = make_array(nx,ny,nz, /float)
	norm_koord = 'z'
	ax_n=30
	az_n=20
	nlev_n = 20
	if nz eq 1 then iz_n = 0 $
	      else iz_n = 1

plinit
zeit = zeit
close, 14
end
;****************************************
; FELDWAHL
;****************************************
PRO FELDWAHL
@globan3
FELDX = FLTARR (NX,NY,NZ, /NOZERO)
FELDY = FLTARR (NX,NY,NZ, /NOZERO)
FELDZ = FLTARR (NX,NY,NZ, /NOZERO)
HELP = FLTARR (NX,NY,NZ, /NOZERO)
IX=1
IY=1
IZ=1
NX1=NX-2
NY1=NY-2
NZ1=NZ-2


PLOWAHL = 'AAA'
plwl1   = 'bb'
plwl2   = 'cc'
PRINT, 'WAS HAETTEN WIR DENN GERNE?'
PRINT, ''
PRINT, 'BFELD(BX,BY v BZ), JFELD(JX,JY v JZ), GESCHW(VNX,VNY v VNZ)?'
PRINT, 'DICHTE(DN), DRUCK(PN),NU12S(N12), NU21S(N21)?' 
PRINT, 'JPARALLEL(JP), EPARALLEL(EP), GRADIENTP(PGX,PGY,PGZ), IMPDIFF(IM)?'
PRINT, 'ODER LIEBER PFEILCHENMALEN(PF)
READ, PLOWAHL

CASE PLOWAHL OF 'DN':  g_feld = RHON
                'PN':  g_feld = 2.*UN^GAMMA
                'N12': g_feld = NU12S
                'N21': g_feld = NU21S
               'PGX': begin
       FOR IZ = 0,NZ1 DO BEGIN
       FOR IY = 0,NY1 DO BEGIN
        FOR IX = 0,NX1 DO BEGIN
        FELDX(IX,IY,IZ) = 2.*UN(IX,IY,IZ)^GAMMA
                         ENDFOR
                         ENDFOR
                         ENDFOR
      FOR  IZ = 0,NZ1 DO BEGIN
       FOR IY = 0,NY1 DO BEGIN
        FOR IX = 1,NX-2 DO BEGIN
        HELP(IX,IY,IZ) = DIFX(IX)*(FELDX(IX+1,IY,IZ)-FELDX(IX-1,IY,IZ))
                         ENDFOR
                         ENDFOR
                         ENDFOR
                        g_feld = help
                      end
               'PGY': begin
       FOR IZ = 0,NZ1 DO BEGIN
       FOR IY = 0,NY1 DO BEGIN
        FOR IX = 0,NX1 DO BEGIN
        FELDY(IX,IY,IZ) = 2.*UN(IX,IY,IZ)^GAMMA
                         ENDFOR
                         ENDFOR
                         ENDFOR
      FOR  IZ = 0,NZ1 DO BEGIN
       FOR IY = 1,NY-2 DO BEGIN
        FOR IX = 0,NX1 DO BEGIN
        HELP(IX,IY,IZ) = DIFY(IY*NX+1)*(FELDY(IX,IY+1,IZ)-FELDY(IX,IY-1,IZ))
                         ENDFOR
                         ENDFOR
                         ENDFOR
                        g_feld = help
                      end
               'PGZ': begin
       FOR IZ = 0,NZ1 DO BEGIN
       FOR IY = 0,NY1 DO BEGIN
        FOR IX = 0,NX1 DO BEGIN
        FELDZ(IX,IY,IZ) = 2.*UN(IX,IY,IZ)^GAMMA
                         ENDFOR
                         ENDFOR
                         ENDFOR
      FOR  IZ = 1,NZ-2 DO BEGIN
       FOR IY = 0,NY1 DO BEGIN
        FOR IX = 0,NX1 DO BEGIN
        HELP(IX,IY,IZ) = DIFZ(IZ)*(FELDZ(IX,IY,IZ+1)-FELDZ(IX,IY,IZ-1))
                         ENDFOR
                         ENDFOR
                         ENDFOR
                        g_feld = help
                      end
                 'IM': begin
                FOR IZ = 0,NZ1 DO BEGIN
       FOR IY = 0,NY1 DO BEGIN
        FOR IX = 0,NX1 DO BEGIN
        FELDZ(IX,IY,IZ) = UN(IX,IY,IZ)^GAMMA
                         ENDFOR
                         ENDFOR
                         ENDFOR
      FOR  IZ = 1,NZ-2 DO BEGIN
       FOR IY = 0,NY1 DO BEGIN
        FOR IX = 0,NX1 DO BEGIN
        HELP(IX,IY,IZ) = DIFZ(IZ)*(FELDZ(IX,IY,IZ+1)-FELDZ(IX,IY,IZ-1))
        HELP(IX,IY,IZ) = HELP(IX,IY,IZ) + GRAV * RHON(IX,IY,IZ)
                         ENDFOR
                         ENDFOR
                         ENDFOR
                         g_feld = help
                       end

                'BX': g_feld = BX
                'BY': g_feld = BY
                'BZ': g_feld = BZ
                'VNX': g_feld = SNX / RHON
                'VNY': g_feld = SNY / RHON 
                'VNZ': g_feld = SNZ / RHON
                'JX': begin
      FOR IZ = 1,NZ1 DO BEGIN
       FOR IY = 1,NY1 DO BEGIN
        FOR IX = 1,NX1 DO BEGIN
	 FELDX(IX,IY,IZ) = DIFY(IY*NX+1)*(BZ(IX,IY+1,IZ)-BZ(IX,IY-1,IZ))$
                         - DIFZ(IZ)*(BY(IX,IY,IZ+1)-BY(IX,IY,IZ-1))
                         ENDFOR
                         ENDFOR
                         ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IY = 0,NY-1 DO BEGIN
       FELDX(0,IY,IZ)= FELDX(1,IY,IZ)
       FELDX(NX-1,IY,IZ) = FELDX(NX-2,IY,IZ)
       ENDFOR
       ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDX(IX,0,IZ)= FELDX(IX,1,IZ)
       FELDX(IX,NY-1,IZ) = FELDX(IX,NY-2,IZ)
       ENDFOR
       ENDFOR
       FOR IY = 0,NY-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDX(IX,IY,0)= FELDX(IX,IY,1)
       FELDX(IX,IY,NZ-1) = FELDX(IX,IY,NZ-2)
       ENDFOR
       ENDFOR
   
                g_feld = feldx
                      end
                'JY' : begin                
    FOR IZ = 1,NZ1 DO BEGIN
       FOR IY = 1,NY1 DO BEGIN
        FOR IX = 1,NX1 DO BEGIN
   	 FELDY(IX,IY,IZ) = DIFZ(IZ)*(BX(IX,IY,IZ+1)-BX(IX,IY,IZ-1))$ 
   	                 -DIFX(IX)*(BZ(IX+1,IY,IZ)-BZ(IX-1,IY,IZ))
                         ENDFOR
                         ENDFOR
                         ENDFOR
  FOR IZ = 0,NZ-1 DO BEGIN
       FOR IY = 0,NY-1 DO BEGIN
       FELDY(0,IY,IZ)= FELDY(1,IY,IZ)
       FELDY(NX-1,IY,IZ) = FELDY(NX-2,IY,IZ)
       ENDFOR
       ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDY(IX,0,IZ)= FELDY(IX,1,IZ)
       FELDY(IX,NY-1,IZ) = FELDY(IX,NY-2,IZ)
       ENDFOR
       ENDFOR
       FOR IY = 0,NY-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDY(IX,IY,0)= FELDY(IX,IY,1)
       FELDY(IX,IY,NZ-1) = FELDY(IX,IY,NZ-2)
       ENDFOR
       ENDFOR
                g_feld = feldy
                       end
                       
               'JZ' : begin                
    FOR IZ = 1,NZ1 DO BEGIN
       FOR IY = 1,NY1 DO BEGIN
        FOR IX = 1,NX1 DO BEGIN
	 FELDZ(IX,IY,IZ) = DIFX(IX)*(BY(IX+1,IY,IZ)-BY(IX-1,IY,IZ))$
                         - DIFY(IY*NX+1)*(BX(IX,IY+1,IZ)-BX(IX,IY-1,IZ))
                         ENDFOR
                         ENDFOR
                         ENDFOR
   FOR IZ = 0,NZ-1 DO BEGIN
       FOR IY = 0,NY-1 DO BEGIN
       FELDZ(0,IY,IZ)= FELDZ(1,IY,IZ)
       FELDZ(NX-1,IY,IZ) = FELDZ(NX-2,IY,IZ)
       ENDFOR
       ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDZ(IX,0,IZ)= FELDZ(IX,1,IZ)
       FELDZ(IX,NY-1,IZ) = FELDZ(IX,NY-2,IZ)
       ENDFOR
       ENDFOR
       FOR IY = 0,NY-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDZ(IX,IY,0)= FELDZ(IX,IY,1)
       FELDZ(IX,IY,NZ-1) = FELDZ(IX,IY,NZ-2)
       ENDFOR
       ENDFOR
                     g_feld = feldz
                       end
             'JP'  : begin
      FOR IZ = 1,NZ1 DO BEGIN
       FOR IY = 1,NY1 DO BEGIN
        FOR IX = 1,NX1 DO BEGIN
	 FELDX(IX,IY,IZ) = DIFY(IY*NX+1)*(BZ(IX,IY+1,IZ)-BZ(IX,IY-1,IZ))$
                         - DIFZ(IZ)*(BY(IX,IY,IZ+1)-BY(IX,IY,IZ-1))
   	 FELDY(IX,IY,IZ) = DIFZ(IZ)*(BX(IX,IY,IZ+1)-BX(IX,IY,IZ-1))$ 
   	                 -DIFX(IX)*(BZ(IX+1,IY,IZ)-BZ(IX-1,IY,IZ))
 	 FELDZ(IX,IY,IZ) = DIFX(IX)*(BY(IX+1,IY,IZ)-BY(IX-1,IY,IZ))$
                         - DIFY(IY*NX+1)*(BX(IX,IY+1,IZ)-BX(IX,IY-1,IZ))
                         ENDFOR
                         ENDFOR
                         ENDFOR
       FOR IZ = 1,NZ1 DO BEGIN
       FOR IY = 1,NY1 DO BEGIN
        FOR IX = 1,NX1 DO BEGIN
         HELP(IX,IY,IZ)=(BX(IX,IY,IZ)*FELDX(IX,IY,IZ)+BY(IX,IY,IZ)$
                       *FELDY(IX,IY,IZ)+BZ(IX,IY,IZ)*FELDZ(IX,IY,IZ))$
                       /SQRT(BX(IX,IY,IZ)*BX(IX,IY,IZ)$
                +BY(IX,IY,IZ)*BY(IX,IY,IZ)+BZ(IX,IY,IZ)*BZ(IX,IY,IZ))
                        ENDFOR
                         ENDFOR
                         ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IY = 0,NY-1 DO BEGIN
       HELP(0,IY,IZ)= HELP(1,IY,IZ)
       HELP(NX-1,IY,IZ) = HELP(NX-2,IY,IZ)
       ENDFOR
       ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       HELP(IX,0,IZ)= HELP(IX,1,IZ)
       HELP(IX,NY-1,IZ) = HELP(IX,NY-2,IZ)
       ENDFOR
       ENDFOR
       FOR IY = 0,NY-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       HELP(IX,IY,0)= HELP(IX,IY,1)
       HELP(IX,IY,NZ-1) = HELP(IX,IY,NZ-2)
       ENDFOR
       ENDFOR
                g_feld = help
                       end
             'EP'  : begin
      FOR IZ = 1,NZ1 DO BEGIN
       FOR IY = 1,NY1 DO BEGIN
        FOR IX = 1,NX1 DO BEGIN
	 FELDX(IX,IY,IZ) = DIFY(IY*NX+1)*(BZ(IX,IY+1,IZ)-BZ(IX,IY-1,IZ))$
                         - DIFZ(IZ)*(BY(IX,IY,IZ+1)-BY(IX,IY,IZ-1))
   	 FELDY(IX,IY,IZ) = DIFZ(IZ)*(BX(IX,IY,IZ+1)-BX(IX,IY,IZ-1))$ 
   	                 -DIFX(IX)*(BZ(IX+1,IY,IZ)-BZ(IX-1,IY,IZ))
 	 FELDZ(IX,IY,IZ) = DIFX(IX)*(BY(IX+1,IY,IZ)-BY(IX-1,IY,IZ))$
                         - DIFY(IY*NX+1)*(BX(IX,IY+1,IZ)-BX(IX,IY-1,IZ))
                         ENDFOR
                         ENDFOR
                         ENDFOR
       FOR IZ = 1,NZ1 DO BEGIN
       FOR IY = 1,NY1 DO BEGIN
        FOR IX = 1,NX1 DO BEGIN
         HELP(IX,IY,IZ)=(BX(IX,IY,IZ)*FELDX(IX,IY,IZ)+BY(IX,IY,IZ)$
                       *FELDY(IX,IY,IZ)+BZ(IX,IY,IZ)*FELDZ(IX,IY,IZ))$
                       /SQRT(BX(IX,IY,IZ)*BX(IX,IY,IZ)$
                +BY(IX,IY,IZ)*BY(IX,IY,IZ)+BZ(IX,IY,IZ)*BZ(IX,IY,IZ))
         HELP(IX,IY,IZ) = HELP(IX,IY,IZ) * RES(IX,IY,IZ)
                        ENDFOR
                         ENDFOR
                         ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IY = 0,NY-1 DO BEGIN
       HELP(0,IY,IZ)= HELP(1,IY,IZ)
       HELP(NX-1,IY,IZ) = HELP(NX-2,IY,IZ)
       ENDFOR
       ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       HELP(IX,0,IZ)= HELP(IX,1,IZ)
       HELP(IX,NY-1,IZ) = HELP(IX,NY-2,IZ)
       ENDFOR
       ENDFOR
       FOR IY = 0,NY-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       HELP(IX,IY,0)= HELP(IX,IY,1)
       HELP(IX,IY,NZ-1) = HELP(IX,IY,NZ-2)
       ENDFOR
       ENDFOR
                g_feld = help
                       end
               'PF' : begin
     print, '1. Komponente des Feldes in der zu plottenden Ebene?'
     read, plwl1
     case plwl1 of 'BX': g_feld = BX 
                   'BY': g_feld = BY 
                   'BZ': g_feld = BZ
                   'VNX': g_feld = SNX / RHON 
                   'VNY': g_feld = SNY / RHON 
                   'VNZ': g_feld = SNZ / RHON
                   'JX': begin
      FOR IZ = 1,NZ1 DO BEGIN
       FOR IY = 1,NY1 DO BEGIN
        FOR IX = 1,NX1 DO BEGIN
	 FELDX(IX,IY,IZ) = DIFY(IY*NX+1)*(BZ(IX,IY+1,IZ)-BZ(IX,IY-1,IZ))$
                         - DIFZ(IZ)*(BY(IX,IY,IZ+1)-BY(IX,IY,IZ-1))
                         ENDFOR
                         ENDFOR
                         ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IY = 0,NY-1 DO BEGIN
       FELDX(0,IY,IZ)= FELDX(1,IY,IZ)
       FELDX(NX-1,IY,IZ) = FELDX(NX-2,IY,IZ)
       ENDFOR
       ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDX(IX,0,IZ)= FELDX(IX,1,IZ)
       FELDX(IX,NY-1,IZ) = FELDX(IX,NY-2,IZ)
       ENDFOR
       ENDFOR
       FOR IY = 0,NY-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDX(IX,IY,0)= FELDX(IX,IY,1)
       FELDX(IX,IY,NZ-1) = FELDX(IX,IY,NZ-2)
       ENDFOR
       ENDFOR
                        g_feld = feldx
                        end
                   'JY': begin
   FOR IZ = 1,NZ1 DO BEGIN
       FOR IY = 1,NY1 DO BEGIN
        FOR IX = 1,NX1 DO BEGIN
   	 FELDY(IX,IY,IZ) = DIFZ(IZ)*(BX(IX,IY,IZ+1)-BX(IX,IY,IZ-1))$ 
   	                 -DIFX(IX)*(BZ(IX+1,IY,IZ)-BZ(IX-1,IY,IZ))
                         ENDFOR
                         ENDFOR                         
                         ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IY = 0,NY-1 DO BEGIN
       FELDY(0,IY,IZ)= FELDY(1,IY,IZ)
       FELDY(NX-1,IY,IZ) = FELDY(NX-2,IY,IZ)
       ENDFOR
       ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDY(IX,0,IZ)= FELDY(IX,1,IZ)
       FELDY(IX,NY-1,IZ) = FELDY(IX,NY-2,IZ)
       ENDFOR
       ENDFOR
       FOR IY = 0,NY-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDY(IX,IY,0)= FELDY(IX,IY,1)
       FELDY(IX,IY,NZ-1) = FELDY(IX,IY,NZ-2)
       ENDFOR
       ENDFOR
                    g_feld = feldy 
                   end
                   'JZ': begin
    FOR IZ = 1,NZ1 DO BEGIN
       FOR IY = 1,NY1 DO BEGIN
        FOR IX = 1,NX1 DO BEGIN
	 FELDZ(IX,IY,IZ) = DIFX(IX)*(BY(IX+1,IY,IZ)-BY(IX-1,IY,IZ))$
                         - DIFY(IY*NX+1)*(BX(IX,IY+1,IZ)-BX(IX,IY-1,IZ))
                         ENDFOR
                         ENDFOR
                         ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IY = 0,NY-1 DO BEGIN
       FELDZ(0,IY,IZ)= FELDZ(1,IY,IZ)
       FELDZ(NX-1,IY,IZ) = FELDZ(NX-2,IY,IZ)
       ENDFOR
       ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDZ(IX,0,IZ)= FELDZ(IX,1,IZ)
       FELDZ(IX,NY-1,IZ) = FELDZ(IX,NY-2,IZ)
       ENDFOR
       ENDFOR
       FOR IY = 0,NY-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDZ(IX,IY,0)= FELDZ(IX,IY,1)
       FELDZ(IX,IY,NZ-1) = FELDZ(IX,IY,NZ-2)
       ENDFOR
       ENDFOR
                   g_feld = feldz
                         end
     endcase
     print, '2. Komponente des Feldes in der zu plottenden Ebene?'
     read, plwl2
     case plwl2 of 'BX': g_feld1 = BX 
                   'BY': g_feld1 = BY 
                   'BZ': g_feld1 = BZ
                   'VNX': g_feld1 = SNX / RHON 
                   'VNY': g_feld1 = SNY / RHON 
                   'VNZ': g_feld1 = SNZ / RHON
                   'JX': begin
      FOR IZ = 1,NZ1 DO BEGIN
       FOR IY = 1,NY1 DO BEGIN
        FOR IX = 1,NX1 DO BEGIN
	 FELDX(IX,IY,IZ) = DIFY(IY*NX+1)*(BZ(IX,IY+1,IZ)-BZ(IX,IY-1,IZ))$
                         - DIFZ(IZ)*(BY(IX,IY,IZ+1)-BY(IX,IY,IZ-1))
                         ENDFOR
                         ENDFOR
                         ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IY = 0,NY-1 DO BEGIN
       FELDX(0,IY,IZ)= FELDX(1,IY,IZ)
       FELDX(NX-1,IY,IZ) = FELDX(NX-2,IY,IZ)
       ENDFOR
       ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDX(IX,0,IZ)= FELDX(IX,1,IZ)
       FELDX(IX,NY-1,IZ) = FELDX(IX,NY-2,IZ)
       ENDFOR
       ENDFOR
       FOR IY = 0,NY-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDX(IX,IY,0)= FELDX(IX,IY,1)
       FELDX(IX,IY,NZ-1) = FELDX(IX,IY,NZ-2)
       ENDFOR
       ENDFOR
                   g_feld1 = feldx 
                         end
                   'JY': begin
   FOR IZ = 1,NZ1 DO BEGIN
       FOR IY = 1,NY1 DO BEGIN
        FOR IX = 1,NX1 DO BEGIN
   	 FELDY(IX,IY,IZ) = DIFZ(IZ)*(BX(IX,IY,IZ+1)-BX(IX,IY,IZ-1))$ 
   	                 -DIFX(IX)*(BZ(IX+1,IY,IZ)-BZ(IX-1,IY,IZ))
                         ENDFOR
                         ENDFOR
                         ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IY = 0,NY-1 DO BEGIN
       FELDY(0,IY,IZ)= FELDY(1,IY,IZ)
       FELDY(NX-1,IY,IZ) = FELDY(NX-2,IY,IZ)
       ENDFOR
       ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDY(IX,0,IZ)= FELDY(IX,1,IZ)
       FELDY(IX,NY-1,IZ) = FELDY(IX,NY-2,IZ)
       ENDFOR
       ENDFOR
       FOR IY = 0,NY-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDY(IX,IY,0)= FELDY(IX,IY,1)
       FELDY(IX,IY,NZ-1) = FELDY(IX,IY,NZ-2)
       ENDFOR
       ENDFOR
                         g_feld1 = feldy 
                         end
                   'JZ': begin                   
    FOR IZ = 1,NZ1 DO BEGIN
       FOR IY = 1,NY1 DO BEGIN
        FOR IX = 1,NX1 DO BEGIN
	 FELDZ(IX,IY,IZ) = DIFX(IX)*(BY(IX+1,IY,IZ)-BY(IX-1,IY,IZ))$
                         - DIFY(IY*NX+1)*(BX(IX,IY+1,IZ)-BX(IX,IY-1,IZ))
                         ENDFOR
                         ENDFOR
                         ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IY = 0,NY-1 DO BEGIN
       FELDZ(0,IY,IZ)= FELDZ(1,IY,IZ)
       FELDZ(NX-1,IY,IZ) = FELDZ(NX-2,IY,IZ)
       ENDFOR
       ENDFOR
       FOR IZ = 0,NZ-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDZ(IX,0,IZ)= FELDZ(IX,1,IZ)
       FELDZ(IX,NY-1,IZ) = FELDZ(IX,NY-2,IZ)
       ENDFOR
       ENDFOR
       FOR IY = 0,NY-1 DO BEGIN
       FOR IX = 0,NX-1 DO BEGIN
       FELDZ(IX,IY,0)= FELDZ(IX,IY,1)
       FELDZ(IX,IY,NZ-1) = FELDZ(IX,IY,NZ-2)
       ENDFOR
       ENDFOR
                         g_feld1 = feldz
                         end
     endcase
                       end


ENDCASE
                 g_pmin = min(g_feld)
                 g_pmax = max(g_feld)

END
;*************************************
pro plinit
;*************************************
; Plinit wird von liesg aufgerufen und
; initialisiert die Parameter fuer den
; 2-D-SURFACE-Plot der Prozedur plg
; nach Angabe der Senkrechtkoordinate
; norm_koord. Ferner bewirkt eine 
; Aenderung von normkoord einen Aufruf
; von plinit.
;*********************************************

@globan3
;*****************************************
; Es werden die Randwerte der Groesse g
; bei *_min = 0,(nx,ny oder nz) -1 ausge-
; schlossen, es sei denn, einer der Werte
; nx,ny oder nz ist kleiner oder gleich 3.
;*****************************************
; y-z-Ebene
;**********
if norm_koord eq 'x' then begin
;	if ny gt 3 then begin
;	  achs_min1 = 0
;	  achs_max1 = ny - 1
;	endif else begin
	  achs_min1 = 0
	  achs_max1 = ny - 1	
;	endelse
;	if nz gt 3 then begin
;	  achs_min2 = 1
;	  achs_max2 = nz - 2
;	endif else begin
	  achs_min2 = 0
	  achs_max2 = nz - 1
;	endelse
	achse1_g = y
	achse2_g = z
	achs_kom1 = 'y'
	achs_kom2 = 'z'
	g_plot_g  = reform(g_feld(ix_n,*,*))
	g_plot_g1  = reform(g_feld1(ix_n,*,*))
; laesst eine Dimension wegfallen
	titel_kom = ' x = '+string(x(ix_n),format='(F8.2)')

	return
endif
;*****************************************************************
; x-z-Ebene
;**********
if norm_koord eq 'y' then begin
;	if nx gt 3 then begin
;	  achs_min1 = 1
;	  achs_max1 = nx - 2
;	endif else begin
	  achs_min1 = 0
	  achs_max1 = nx - 1
;	endelse
;	if nz gt 3 then begin
;	  achs_min2 = 1
;	  achs_max2 = nz - 2
;	endif else begin
	  achs_min2 = 0
	  achs_max2 = nz - 1
;	endelse
	achse1_g = x
	achse2_g = z
	achs_kom1 = 'x'
	achs_kom2 = 'z'
	g_plot_g  = g_feld(*,iy_n,*)
	g_plot_g  = reform(g_plot_g , nx, nz, /overwrite)
	g_plot_g1  = g_feld1(*,iy_n,*)
	g_plot_g1  = reform(g_plot_g1 , nx, nz, /overwrite)
; hier mittlere Dimension ny=1 entfernen
	titel_kom = '  y = '+string(y(iy_n),format='(F8.2)')

	return
endif
;****************************************************************
; x-y-Ebene
;**********

if norm_koord eq 'z' then begin
;	if nx gt 3 then begin
;	  achs_min1 = 1
;	  achs_max1 = nx - 2
;	endif else begin
	  achs_min1 = 0
	  achs_max1 = nx - 1
;	endelse
;	if ny gt 3 then begin
;	  achs_min2 = 1
;	  achs_max2 = ny - 2
;	endif else begin
	  achs_min2 = 0
	  achs_max2 = ny - 1
;	endelse
	achse1_g = x
	achse2_g = y
	achs_kom1 = 'x'
	achs_kom2 = 'y'
	g_plot_g  = g_feld(*,*,iz_n)
	g_plot_g1  = g_feld1(*,*,iz_n)
; hier geht's automatisch!
	titel_kom = ' z = '+string(z(iz_n),format='(F8.2)')

	return
endif
print,'Keine Initialisierung!'
return
end

;*****************************************************************
pro plg, xnull=ixn,ynull=iyn,znull=izn,mi1=achs_mi1,ma1=achs_ma1,$
         mi2=achs_mi2,ma2=achs_ma2,cp=c_plot,$
         ax=temp_ax,az=temp_az,nl=temp_nlev
;**********************************************
; Sollte das Datenfeld g_feld geaendert worden
; sein, so ist plinit aufzurufen!
;**********************************************
@globan3
common numnamen2d, ueberschrift1, ueberschrift2, ueberschrift3, $
		   xbezeichnung, ybezeichnung, zbezeichnung
w='g'
fra='n'
zrw='m'
groesse = 'MAGNETFELD'
eingebe = 'a'
absfra = 'j' & ordfra='n'
koo=''
x_index =1
y_index =1
z_index =1
I1 = 1
I2 = 1
lll=1.0
print, ''
print,'Ueberschrift aendern(a) oder beibehalten(b)?'
read, eingebe
if  (eingebe eq 'a') then begin
  print,'BITTE TITELUEBERSCHRIFT EINGEBEN'
  read, groesse
  altertitel = groesse
endif else begin
  groesse = altertitel
endelse
;**********************************************
; Es wird nachgesehen, ob die (Vor-) Einstellungen
; geaendert werden sollen.
;***********************************************
if keyword_set(temp_ax) then ax_n = temp_ax
if keyword_set(temp_az) then az_n = temp_az

if (keyword_set(ixn)) then begin
    if (ixn ge 1) and (ixn le nx) then begin
	norm_koord = 'x'
	ix_n=long(ixn)
	plinit
    end
endif else begin
   	if (keyword_set(iyn)) then begin
   	   if (iyn ge 1) and (iyn le ny) then begin
             norm_koord = 'y'
             iy_n = long(iyn-1)
             plinit
           end
        endif else begin
             if (keyword_set(izn)) then begin
                if (izn ge 1) and (izn le nz) then begin
		 	norm_koord = 'z'
		 	iz_n = long(izn-1)
		 	plinit
	        end
	     endif else begin
			print,"Keine neue Auswahl der Schnittebene getroffen!"
	     endelse
	endelse
endelse
;
;if keyword_set(achs_mi1) then achs_min1 = achs_mi1 - 1 $
;	else achs_min1=0L ; mit ELSE-Anweisung
;
if keyword_set(achs_mi1) then achs_min1 = achs_mi1 - 1
if keyword_set(achs_ma1) then achs_max1 = achs_ma1 - 1 
;	else achs_max1=n_elements(achse1)-1
if keyword_set(achs_mi2) then achs_min2 = achs_mi2 - 1 
;	else achs_min2=0L
if keyword_set(achs_ma2) then achs_max2 = achs_ma2 - 1 
;	else achs_max2=n_elements(achse2)-1

;************************************************
;  Darzustellende Bereiche werden ausgeschnitten.
;************************************************
;
; Ausgabe der Nummer der Randwerte (um 1 groesser
; als der Array-Index von IDL)
;
;print,achs_min1+1,achs_max1+1,achs_min2+1,achs_max2+1
;

;print, achse1_g
print,''
;print, achse2_g
print, ' '
print, ' '
if (c_plot eq 5) or (c_plot eq 1) then goto, jump40
print, 'GESAMTGEBIET ODER NEUEN AUSCHNITT WAEHLEN (g/a)?'
read, w
if w eq 'g' then goto, jump40
plinit
print, ' '
for i=0,(2*(achs_max1-1))/2,2 do begin
if (i eq achs_max1) then print, '[',i,']:',achse1_g(i)$
else print, '[',i,']:',achse1_g(i),'   [',i+1,']:',achse1_g(i+1)
endfor
print, ' '
print, 'INDICES FUER DIE ABSZISSE [',x1alt,x2alt,'] behalten(j/n)?'
read, absfra
if  (absfra eq 'n') then begin
read, achs_min1, achs_max1
x1alt=achs_min1 & x2alt=achs_max1
endif else begin
achs_min1 = x1alt & achs_max1 = x2alt
endelse

print, ' '
print, ' '
for i=0,(2*(achs_max2-1))/2,2 do begin
if (i eq achs_max2) then print, '[',i,']:',achse2_g(i)$
else print, '[',i,']:',achse2_g(i),'   [',i+1,']:',achse2_g(i+1)
endfor
print, 'INDICES FUER DIE ORDINATE [',y1alt,y2alt,'] behalten?'
read, ordfra
if  (ordfra eq 'n') then begin
print, ' '
read, achs_min2, achs_max2
y1alt=achs_min2 & y2alt=achs_max2
endif else begin
achs_min2 = y1alt & achs_max2 = y2alt
endelse
jump40:  achse1 = achse1_g(achs_min1:achs_max1)
achse2 = achse2_g(achs_min2:achs_max2)
g_plot = g_plot_g(achs_min1:achs_max1,achs_min2:achs_max2)
g_plot1 = g_plot_g1(achs_min1:achs_max1,achs_min2:achs_max2)

zrw = 'm'
if (c_plot eq 3) or (c_plot eq 4) or (c_plot eq 1) then begin
print, 'zrange aendern (a) beibehalten (b) oder min/max (m)?'
read, zrw
case zrw of 'a': begin
                 print, 'unterer wert, oberer wert?'
                 read, g_pmin, g_pmax
                 end
            'b': print, ''
            'm': begin
                 g_pmin = min(g_plot)
                 g_pmax = max(g_plot)
                 end
endcase
endif

zrange= [g_pmin, g_pmax]


;**********************
;  Endlich wird gemalt.
;**********************

	
if keyword_set(c_plot) then begin
;
; Zahl der Hoehenlinien aktualisieren (Max_nlevels=29)
;
  if keyword_set(temp_nlev) then nlev_n = temp_nlev
;
    if c_plot eq 1 then begin
      print,'ABHAENGIGKEIT VON WELCHER KOORDINATE (x,y,z)?'
      read, koo

   case koo of 'x': begin
                 print,'y-index, z-index?'
                 read,y_index,z_index
                 yin=long(y_index)
                 zin=long(z_index)
                 print,' '
                 print, y(y_index), z(z_index)
                 print,' '
                 print,'INTERVALLGRENZEN (I1, I2)?'
                 read, I1, I2
                 g_f1 = g_feld(*,y_index,z_index)
                 titel_kom = ' y = '+string(y(yin),format='(F8.2)')$
                 +'   '+'z = '+string(z(zin),format='(F8.2)')
                 plot, x, g_f1, xrange = [I1, I2], $
	        xtitle='x',ytitle=groesse $
	       ,title=groesse + ", Zeit = " + string(zeit,format='(F8.2)') $
	       ,subtitle = " I-Schritt = " $
	       + string(ischritt,format='(I5)') + '  '  $
	       + titel_kom ,charsize = 1.6,yrange=zrange
                 end
               'y': begin
                 print,'x-index, z-index?'
                 read,x_index,z_index
                 print,' '
                 print, x(x_index), z(z_index)
                 print,' '
                 print,'INTERVALLGRENZEN (I1, I2)?'
                 read, I1, I2
                 g_f1 = g_feld(x_index,*,z_index)
                 titel_kom = ' x = '+string(x(x_index),format='(F8.2)')$
                 +'   '+'z = '+string(z(z_index),format='(F8.2)')
                 plot, y, g_f1, xrange = [I1, I2],$
	        xtitle='y',ytitle=groesse $
	       ,title=groesse + ", Zeit = " + string(zeit,format='(F7.2)') $
	       ,subtitle = " I-Schritt = " $
	       + string(ischritt,format='(I5)') + '  '  $
	       + titel_kom ,charsize = 1.6,yrange=zrange
                 end
              'z': begin
                 print,'x-index, y-index?'
                 read,x_index,y_index
                 print,' '
                 print, x(x_index), y(y_index)
                 print,' '

                 print,'INTERVALLGRENZEN (I1, I2)?'
                 read, I1, I2
                 g_f1 = g_feld(x_index,y_index,*)
                 titel_kom = ' x = '+string(x(x_index),format='(F8.2)')$
                 +'  '+'y = '+string(y(y_index),format='(F8.2)')

                 plot, z, g_f1, xrange = [I1 ,I2],$
	        xtitle='z',ytitle=groesse $
	       ,title=groesse + ", Zeit = " + string(zeit,format='(F7.2)') $
	       ,subtitle = " I-Schritt = " $
	       + string(ischritt,format='(I5)') + '  '  $
	       + titel_kom ,charsize = 1.6,yrange=zrange

                 end
                 
   endcase
      endif
    if c_plot eq 2 then begin
        LEVELS=fltarr(nlev_n)
        for i=0,nlev_n-1 do begin
         LEVELS(i)=0
        endfor
        for i=0,nlev_n-1,4 do begin
         LEVELS(i)=1
        endfor
	contour, g_plot, achse1, achse2, nlevels=nlev_n $
	,xtitle=achs_kom1,ytitle=achs_kom2 $
	,title=groesse + ", Zeit = " + string(zeit,format='(F7.2)') $
	,subtitle = " I-Schritt = " $
	+ string(ischritt,format='(I6)') + '  '  $
	+ titel_kom ,charsize = 1.6,zrange=zrange,C_labels=levels
    endif
    if c_plot eq 3 then begin
	surface,g_plot,achse1,achse2,xstyle=1,ystyle=1, $
	xtitle=achs_kom1,ytitle=achs_kom2 $
	,title=groesse + ", Zeit = " + string(zeit,format='(F7.2)') $
	,subtitle = " !CI-Schritt = " $
	+ string(ischritt,format='(I5)') + '  '  $
	+ titel_kom ,charsize = 1.7,az=az_n,ax=ax_n,zrange=zrange,/save	
	contour, /T3D, /NOERASE, zvalue = 0,g_plot, achse1, achse2, $
	title='Hoehenlinien',xtitle=achs_kom1,ytitle=achs_kom2,charsize = 1.7, $
	 xstyle=1,ystyle=1,nlevels=nlev_n,/NOCLIP
    endif
    if c_plot eq 4 then begin
        print, 's/w (0), blau(1), rot(3), blau-rot(11)?'
        read, farb 
        loadct, farb
        print, 'farbe gemaess unterschiedlichen werten (j/n)?'
        read, fra
        case fra of 
        'n': shade_surf, g_plot, achse1, achse2,xstyle=1,ystyle=1,$
        xtitle=achs_kom1,ytitle=achs_kom2,$
	title=groesse + ", Zeit = " + string(zeit,format='(F7.2)') $
	,subtitle = " !CI-Schritt = " $
	+ string(ischritt,format='(I5)') + '  '  $
	+ titel_kom ,charsize = 1.7,az=az_n,ax=ax_n,zrange=zrange
	'j': begin
	shade_surf, g_plot, shade=bytscl(g_plot), achse1, achse2,$
        xstyle=1,ystyle=1,xtitle=achs_kom1,ytitle=achs_kom2,$
	title=groesse + ", Zeit = " + string(zeit,format='(F7.2)') $
	,subtitle = " !CI-Schritt = " $  
	+ string(ischritt,format='(I5)') + '  '  $
	+ titel_kom ,charsize = 1.7,zrange=zrange
;	+ titel_kom ,charsize = 1.7,az=az_n,ax=ax_n,zrange=zrange
	end
	endcase
     endif
     if c_plot eq 5 then begin
          print, 'LAENGE DER PFEILE?'
          read, lll
         pfeile,g_plot,g_plot1,$
         achse1,achse2,length=lll,xstyle=1,ystyle=1,$
        xtitle=achs_kom1,ytitle=achs_kom2,$
	title=groesse + ", Zeit = " + string(zeit,format='(F7.2)') $
	,subtitle = " !CI-Schritt = " $
	+ string(ischritt,format='(I5)') + '  '  $
	+ titel_kom ,charsize = 1.7
         end
endif else begin
	surface,g_plot,achse1,achse2,xstyle=1,ystyle=1,$
	xtitle=achs_kom1+' !C ',ytitle=achs_kom2, $
	title=groesse + ", Zeit = " + string(zeit,format='(F7.2)') $
	,subtitle = " I-Schritt = " $
	+ string(ischritt,format='(I5)') + '      '  $
	+ titel_kom ,charsize = 1.7,az=az_n,ax=ax_n,zrange=zrange
endelse

ueberschrift1 = groesse + ", Zeit = " + string(zeit,format='(F7.2)')
ueberschrift2 =" I-Schritt = " $
	+ string(ischritt,format='(I5)') + '  '  $
	+ titel_kom 
ueberschrift3=''
xbezeichnung=achs_kom1
ybezeichnung=achs_kom2
zbezeichnung=''

return
end

;****************************
PRO GLATT
;****************************
@globan3
ISAFE = 1l & IEND = 1l
ITART = 1l & IVISRHO = 1l & NGLATT = 1l & NAUS = 1l
IFEHLER = 1l & IGRID = 1l & IDIAG = 1l
ISAT = 1l
DT = 0.1  & GAMMA = 0.1  & RHON0 = 1.0
FEHLER = 0.1
AEQUI = FLTARR(3)
ZENTR = FLTARR(3)
PERIO = LINDGEN(3)
LSYM = LINDGEN(6)
;CLOSE, 14
OPENR, 14,CFILE, /F77_UNFORMATTED
READU, 14, NX,NY,NZ
                       

MEANMX = FLTARR (NV)
MEANMY = FLTARR (NV)
MEANMZ = FLTARR (NZ)
MEANPX = FLTARR (NV)
MEANPY = FLTARR (NV)
MEANPZ = FLTARR (NZ)
READU, 14, ISCHRITT,IAUS,ISAFE,IEND,DT,ZEIT,GAMMA,ETA,RHON0
READU, 14, XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX           
READU, 14, XVEC,DIFX,DDIFX,DDIFPX,DDIFMX,MEANPX,MEANMX,$
           YVEC,DIFY,DDIFY,DDIFPY,DDIFMY,MEANPY,MEANMY,$
           ZVEC,DIFZ,DDIFZ,DDIFPZ,DDIFMZ,MEANPZ,MEANMZ


HELP  = FLTARR (NX,NY,NZ)
FELDX = FLTARR (NX,NY,NZ)
FELDY = FLTARR (NX,NY,NZ)
FELDZ = FLTARR (NX,NY,NZ)
IV = 1l & IZ = 1l

;---------------------------------------------------------

      FOR IZ = 1,NZ-2 DO BEGIN
       FOR IY = 1,NY-2 DO BEGIN
        FOR IX = 1,NX-2 DO BEGIN
          IV = IX + (IY-1)*NX
          HELP(IX,IY,IZ) =  (1.-0.75)*RES(IX,IY,IZ)$
                        + 0.25*(MEANPX(IV)*RES(IX+1,IY,IZ)$
                              + MEANMX(IV)*RES(IX-1,IY,IZ)$
                              + MEANPY(IV)*RES(IX,IY+1,IZ)$
                              + MEANMY(IV)*RES(IX,IY-1,IZ)$
                              + MEANPZ(IZ)*RES(IX,IY,IZ+1)$
                              + MEANMZ(IZ)*RES(IX,IY,IZ-1) )
        ENDFOR
         ENDFOR
          ENDFOR
            RES = HELP
;----------------------------------------------------------

;      FOR IZ = 1,NZ-2 DO BEGIN
;       FOR IY = 1,NY-2 DO BEGIN
;        FOR IX = 1,NX-2 DO BEGIN
;          IV = IX + (IY-1)*NX
;          HELP(IX,IY,IZ) =  (1.-0.75)*UN(IX,IY,IZ)$
;                        + 0.25*(MEANPX(IV)*UN(IX+1,IY,IZ)$
;                              + MEANMX(IV)*UN(IX-1,IY,IZ)$
;                              + MEANPY(IV)*UN(IX,IY+1,IZ)$
;                              + MEANMY(IV)*UN(IX,IY-1,IZ)$
;                              + MEANPZ(IZ)*UN(IX,IY,IZ+1)$
;                              + MEANMZ(IZ)*UN(IX,IY,IZ-1) )
;        ENDFOR
;         ENDFOR
;          ENDFOR

;            UN = HELP
;----------------------------------------------------------

;      FOR IZ = 1,NZ-2 DO BEGIN
;       FOR IY = 1,NY-2 DO BEGIN
;        FOR IX = 1,NX-2 DO BEGIN
;          IV = IX + (IY-1)*NX
;          FELDX(IX,IY,IZ) =  (1.-0.75)*SNX(IX,IY,IZ)$
;                        + 0.25*(MEANPX(IV)*SNX(IX+1,IY,IZ)$
;                             + MEANMX(IV)*SNX(IX-1,IY,IZ)$
;                              + MEANPY(IV)*SNX(IX,IY+1,IZ)$
;                              + MEANMY(IV)*SNX(IX,IY-1,IZ)$
;                              + MEANPZ(IZ)*SNX(IX,IY,IZ+1)$
;                              + MEANMZ(IZ)*SNX(IX,IY,IZ-1) )
;          FELDY(IX,IY,IZ) =  (1.-0.75)*SNY(IX,IY,IZ)$
;                        + 0.25*(MEANPX(IV)*SNY(IX+1,IY,IZ)$
;                              + MEANMX(IV)*SNY(IX-1,IY,IZ)$
;                              + MEANPY(IV)*SNY(IX,IY+1,IZ)$
;                              + MEANMY(IV)*SNY(IX,IY-1,IZ)$
;                              + MEANPZ(IZ)*SNY(IX,IY,IZ+1)$
;                              + MEANMZ(IZ)*SNY(IX,IY,IZ-1) )
;          FELDZ(IX,IY,IZ) =  (1.-0.75)*SNZ(IX,IY,IZ)$
;                        + 0.25*(MEANPX(IV)*SNZ(IX+1,IY,IZ)$
;                              + MEANMX(IV)*SNZ(IX-1,IY,IZ)$
;                              + MEANPY(IV)*SNZ(IX,IY+1,IZ)$
;                              + MEANMY(IV)*SNZ(IX,IY-1,IZ)$
;                              + MEANPZ(IZ)*SNZ(IX,IY,IZ+1)$
;                              + MEANMZ(IZ)*SNZ(IX,IY,IZ-1) )
;        ENDFOR
;         ENDFOR
;          ENDFOR

;            SNX = FELDX
;            SNY = FELDY
;            SNZ = FELDZ
;----------------------------------------------------------

;      FOR IZ = 1,NZ-2 DO BEGIN
;       FOR IY = 1,NY-2 DO BEGIN
;        FOR IX = 1,NX-2 DO BEGIN
;          IV = IX + (IY-1)*NX
;          FELDX(IX,IY,IZ) =  (1.-0.75)*BX(IX,IY,IZ)$
;                        + 0.25*(MEANPX(IV)*BX(IX+1,IY,IZ)$
;                              + MEANMX(IV)*BX(IX-1,IY,IZ)$
;                              + MEANPY(IV)*BX(IX,IY+1,IZ)$
;                              + MEANMY(IV)*BX(IX,IY-1,IZ)$
;                              + MEANPZ(IZ)*BX(IX,IY,IZ+1)$
;                              + MEANMZ(IZ)*BX(IX,IY,IZ-1) )
;          FELDY(IX,IY,IZ) =  (1.-0.75)*BY(IX,IY,IZ)$
;                        + 0.25*(MEANPX(IV)*BY(IX+1,IY,IZ)$
;                              + MEANMX(IV)*BY(IX-1,IY,IZ)$
;                              + MEANPY(IV)*BY(IX,IY+1,IZ)$
;                              + MEANMY(IV)*BY(IX,IY-1,IZ)$
;                              + MEANPZ(IZ)*BY(IX,IY,IZ+1)$
;                              + MEANMZ(IZ)*BY(IX,IY,IZ-1) )
;          FELDZ(IX,IY,IZ) =  (1.-0.75)*BZ(IX,IY,IZ)$
;                        + 0.25*(MEANPX(IV)*BZ(IX+1,IY,IZ)$
;                              + MEANMX(IV)*BZ(IX-1,IY,IZ)$
;                              + MEANPY(IV)*BZ(IX,IY+1,IZ)$
;                              + MEANMY(IV)*BZ(IX,IY-1,IZ)$
;                              + MEANPZ(IZ)*BZ(IX,IY,IZ+1)$
;                              + MEANMZ(IZ)*BZ(IX,IY,IZ-1) )
;        ENDFOR
;         ENDFOR
;          ENDFOR

;            BX = FELDX
;            BY = FELDY
;            BZ = FELDZ
END
;----------------------------------------------------------
;************************
PRO GRIDNEU
;************************
; ---------------------------------------------------------
;         BERECHNET DIE GITTERPARAMETER, WELCHE DER INTEGRATION
;         ZUGRUNDE LIEGEN.
; ---------------------------------------------------------
@globan3

                 IX = 1l & IY= 1l & IZ = 1l
                 KX = 1.0 & KY = 1.0 & KZ = 1.0
                 EPSX = 1.0 & EPSY = 1.0 & EPSZ = 1.0
                 XANF = 1.0 & YANF = 1.0 & ZANF = 1.0
                 
                 XMINNEU  = FLTARR(3)
                 XMAXNEU  = FLTARR(3)


; XMINNEU(0,1,2) UND XMAXNEU(0,1,2) SIND DIE NEUEN GITTERGRENZEN

      XMINNEU(0)  = xmin 
      XMINNEU(1)  = ymin
      XMINNEU(2)  = zmin
      XMAXNEU(0)  = xmax
      XMAXNEU(1)  = ymax 
      XMAXNEU(2)  = zmax

;Achtung: die naechsten drei muessen ungerade sein!!
;
                 XNEU = FLTARR (NXNEU(0))
                 YNEU = FLTARR (NXNEU(1))
                 ZNEU = FLTARR (NXNEU(2))

;------------------------------------------------
;  BERECHNUNG DER X-KOORDINATE
;------------------------------------------------

      KX =  (XMAXNEU(0)-XMINNEU(0))/(NXNEU(0)-1)
      XANF = XMINNEU(0)
      FOR IX = 1,NXNEU(0)-2 DO XNEU(IX) = XANF + KX*IX
      XNEU(0)     = XMINNEU(0)
      XNEU(NXNEU(0)-1) = XMAXNEU(0)
;
;------------------------------------------------
;  BERECHNUNG DER GITTERPARAMETER DER Y-KOORDINATE
;------------------------------------------------

      KY =  (XMAXNEU(1)-XMINNEU(1))/(NXNEU(1)-1)
      YANF = XMINNEU(1)
      FOR IY = 1,NXNEU(1)-2 DO YNEU(IY) = YANF + KY*IY
      YNEU(0)     = XMINNEU(1)
      YNEU(NXNEU(1)-1) = XMAXNEU(1)
;
;------------------------------------------------
;  BERECHNUNG DER GITTERPARAMETER DER Z-KOORDINATE
;------------------------------------------------

      KZ =  (XMAXNEU(2)-XMINNEU(2))/(NXNEU(2)-1)
      ZANF = XMINNEU(2)
      FOR IZ = 1,NXNEU(2)-2 DO ZNEU(IZ) = ZANF + KZ*IZ
      ZNEU(0)     = XMINNEU(2)
      ZNEU(NXNEU(2)-1) = XMAXNEU(2)


;###################################################################
; jetzt kommt die neubelegung des gitters !

     XDEL = FLTARR(NXNEU(0)) & YDEL = FLTARR(NXNEU(1)) & ZDEL = FLTARR(NXNEU(2))
     XD = FLTARR(NXNEU(0)) & YD = FLTARR(NXNEU(1)) & ZD = FLTARR(NXNEU(2))
     XALT = FLTARR(NX) & YALT = FLTARR(NY) & ZALT = FLTARR(NZ)

     IXALT = LINDGEN(NX) & IYALT = LINDGEN(NY) & IZALT = LINDGEN(NZ)
     JXALT = 1l & JYALT = 1l & JZALT = 1l
     I = 1L & J = 1L & K = 1L

     FNEU = FLTARR(NXNEU(0),NXNEU(1),NXNEU(2))
     HELP = FLTARR(NX,NY,NZ)
     FNEU1 = FLTARR(NXNEU(0),NXNEU(1),NXNEU(2))
     HELP1 = FLTARR(NX,NY,NZ)

;   SUBROUTINE ZUR INTERPOLATION VON 3D FELDERN AUF
;               EIN NEUES GITTER, WOBEI DIE GEBIETS-
;               GRENZEN INNERHALB ODER GLEICH DENEN
;               DES ALTEN GEBIETS SEIN MUESSEN!
; -----------------------------------------------------

;   BESTIMMUNG DES ZU EINEM NEUEN GITTERINDEX I
;               KORRESPONDIERENDEN ALTEN INDEX IXALT
        XALT = X
        YALT = Y
        ZALT = Z
      JXALT = 0
      FOR I = 0, NXNEU(0)-1 DO BEGIN
      JUMP50:
        IF ( XALT(JXALT) LE XNEU(I))  AND  (XALT(JXALT+1) GT XNEU(I) ) THEN BEGIN
          IXALT(I) = JXALT
        ENDIF ELSE BEGIN
          JXALT = JXALT +1
          GOTO, JUMP50
        ENDELSE
      ENDFOR

      JYALT = 0
      FOR I = 0, NXNEU(1)-1 DO BEGIN
      JUMP150:
        IF ( YALT(JYALT) LE YNEU(I))  AND  (YALT(JYALT+1) GT YNEU(I) ) THEN BEGIN
          IYALT(I) = JYALT
        ENDIF ELSE BEGIN
          JYALT = JYALT+1
          GOTO, JUMP150
        ENDELSE
      ENDFOR

      JZALT = 0
      FOR I = 0, NXNEU(2)-1 DO BEGIN
      JUMP250:
        IF ( ZALT(JZALT) LE ZNEU(I))  AND  (ZALT(JZALT+1) GT ZNEU(I) ) THEN BEGIN
          IZALT(I) = JZALT
        ENDIF ELSE BEGIN
          JZALT = JZALT + 1
          GOTO, JUMP250
        ENDELSE
      ENDFOR

;   BSTIMMUNG DER ABSTAENDE IM GITTER

      FOR I = 0, NXNEU(0)-1 DO BEGIN
        XD(I)   = XNEU(I) - XALT(IXALT(I))
        XDEL(I) = XALT(IXALT(I)+1) - XNEU(I)
      ENDFOR
      FOR I = 0, NXNEU(1)-1 DO BEGIN
        YD(I)   = YNEU(I) - YALT(IYALT(I))
        YDEL(I) = YALT(IYALT(I)+1) - YNEU(I)
      ENDFOR
      FOR I = 0, NXNEU(2)-1 DO BEGIN
        ZD(I)   = ZNEU(I) - ZALT(IZALT(I))
        ZDEL(I) = ZALT(IZALT(I)+1) - ZNEU(I)
      ENDFOR

;   jetzt NUR noch die gesamten daten umlegen!!! (weiss der henker wie!)
;   BESTIMMUNG DER NEUEN FELDWERTE
;   ICH bin der henker!
;   setze einfach help = groesse, dann ist fneu diese groesse auf dem neuen gitter!
help = g_feld
case plowahl of 'PF': help1 = g_feld1
else: help1= g_feld
endcase
      FOR K = 0, NXNEU(2)-1 DO BEGIN
        FOR J = 0, NXNEU(1)-1 DO BEGIN
          FOR I = 0, NXNEU(0)-1 DO BEGIN
            FNEU(I,J,K) = ( HELP(IXALT(I),IYALT(J),IZALT(K))$
                              * XDEL(I)*YDEL(J)*ZDEL(K)$
                          + HELP(IXALT(I)+1,IYALT(J),IZALT(K))$
                              * XD(I)*YDEL(J)*ZDEL(K)$
                          + HELP(IXALT(I),IYALT(J)+1,IZALT(K))$
                              * XDEL(I)*YD(J)*ZDEL(K)$
                          + HELP(IXALT(I),IYALT(J),IZALT(K)+1)$
                              * XDEL(I)*YDEL(J)*ZD(K)$
                          + HELP(IXALT(I)+1,IYALT(J)+1,IZALT(K))$
                              * XD(I)*YD(J)*ZDEL(K)$
                          + HELP(IXALT(I)+1,IYALT(J),IZALT(K)+1)$
                              * XD(I)*YDEL(J)*ZD(K)$
                          + HELP(IXALT(I),IYALT(J)+1,IZALT(K)+1)$
                              * XDEL(I)*YD(J)*ZD(K)$
                          + HELP(IXALT(I)+1,IYALT(J)+1,IZALT(K)+1)$
                              * XD(I)*YD(J)*ZD(K)     )$
                          /( XALT(IXALT(I)+1) - XALT(IXALT(I)) )$
                          /( YALT(IYALT(J)+1) - YALT(IYALT(J)) )$
                          /( ZALT(IZALT(K)+1) - ZALT(IZALT(K)) )

            FNEU1(I,J,K) = ( HELP1(IXALT(I),IYALT(J),IZALT(K))$
                              * XDEL(I)*YDEL(J)*ZDEL(K)$
                          + HELP1(IXALT(I)+1,IYALT(J),IZALT(K))$
                              * XD(I)*YDEL(J)*ZDEL(K)$
                          + HELP1(IXALT(I),IYALT(J)+1,IZALT(K))$
                              * XDEL(I)*YD(J)*ZDEL(K)$
                          + HELP1(IXALT(I),IYALT(J),IZALT(K)+1)$
                              * XDEL(I)*YDEL(J)*ZD(K)$
                          + HELP1(IXALT(I)+1,IYALT(J)+1,IZALT(K))$
                              * XD(I)*YD(J)*ZDEL(K)$
                          + HELP1(IXALT(I)+1,IYALT(J),IZALT(K)+1)$
                              * XD(I)*YDEL(J)*ZD(K)$
                          + HELP1(IXALT(I),IYALT(J)+1,IZALT(K)+1)$
                              * XDEL(I)*YD(J)*ZD(K)$
                          + HELP1(IXALT(I)+1,IYALT(J)+1,IZALT(K)+1)$
                              * XD(I)*YD(J)*ZD(K)     )$
                          /( XALT(IXALT(I)+1) - XALT(IXALT(I)) )$
                          /( YALT(IYALT(J)+1) - YALT(IYALT(J)) )$
                          /( ZALT(IZALT(K)+1) - ZALT(IZALT(K)) )
       ENDFOR
         ENDFOR
           ENDFOR

xmin=xminneu(0)
ymin=xminneu(1)
zmin=xminneu(2)
xmax=xmaxneu(0)
ymax=xmaxneu(1)
zmax=xmaxneu(2)
nx=nxneu(0)
ny=nxneu(1)
nz=nxneu(2)
x=xneu
y=yneu
z=zneu
g_feld = fneu
g_feld1 = fneu1



; SPIEGELUNG DES FELDES BEI X=0 : MIRROR(0)=1,-1
;                           Y=0 : MIRROR(1)=1,-1
;                           Z=0 : MIRROR(2)=1,-1
; MIRROR= 1 : SYMMETRISCH
; MIRROR=-1 : ANTISYMMETRISCH
; MIRROR= 0 : NIX

      MIRROR    = INTARR(3)
      MIRROR1   = INTARR(3)
      MIRROR(0) = 0
      MIRROR(1) = 0
      MIRROR(2) = 0
      MIRROR1(0) = 0
      MIRROR1(1) = 0
      MIRROR1(2) = 0

PRINT, 'SOLL GESPIEGELT WERDEN? J,N'
EINGABE='J'
READ, EINGABE
IF EINGABE EQ 'J' THEN BEGIN
  PRINT, 'SPIEGELUNG DES FELDES BEI X=0 : MIRROR(0)=1,-1'
  PRINT, '                          Y=0 : MIRROR(1)=1,-1'
  PRINT, '                          Z=0 : MIRROR(2)=1,-1'
  PRINT, ' MIRROR= 1 : SYMMETRISCH'
  PRINT, ' MIRROR=-1 : ANTISYMMETRISCH'
  PRINT, ' MIRROR= 0 : NIX'
  PRINT, ' EINGABE: MIRROR(0), MIRROR(1), MIRROR(2)'
  READ, MIRROR
  PRINT, ' EINGABE: MIRROR1(0), MIRROR1(1), MIRROR1(2)'
  READ, MIRROR1
ENDIF

IF MIRROR(0) NE 0 THEN BEGIN
 FOR I=0, NXNEU(0)-1,2 DO BEGIN
  XNEU(I/2)           =-X(NXNEU(0)-1-I)
  XNEU(I/2+(NXNEU(0)-1)/2)  = X(I)
 ENDFOR
 FOR K=0,NXNEU(2)-1 DO BEGIN
  FOR J=0,NXNEU(1)-1 DO BEGIN
   FOR I=0, NXNEU(0)-1,2 DO BEGIN
    FNEU(I/2,J,K)           = G_FELD(NXNEU(0)-1-I,J,K)*MIRROR(0)
    FNEU(I/2+(NXNEU(0)-1)/2,J,K)  = G_FELD(I,J,K)
    FNEU1(I/2,J,K)           = G_FELD1(NXNEU(0)-1-I,J,K)*MIRROR1(0)
    FNEU1(I/2+(NXNEU(0)-1)/2,J,K)  = G_FELD1(I,J,K)
   ENDFOR
  ENDFOR
 ENDFOR
 XMIN = -XMAX
ENDIF


IF MIRROR(1) NE 0 THEN BEGIN
 FOR I=0, NXNEU(1)-1,2 DO BEGIN
  YNEU(I/2)           =-Y(NXNEU(1)-1-I)
  YNEU(I/2+(NXNEU(1)-1)/2)= Y(I)
 ENDFOR
 FOR K=0,NXNEU(2)-1 DO BEGIN
  FOR J=0,NXNEU(1)-1,2 DO BEGIN
   FOR I=0, NXNEU(0)-1 DO BEGIN
    FNEU(I,J/2,K)           = G_FELD(I,NXNEU(1)-1-J,K)*MIRROR(1)
    FNEU(I,J/2+(NXNEU(1)-1)/2,K)  = G_FELD(I,J,K)
    FNEU1(I,J/2,K)           = G_FELD1(I,NXNEU(1)-1-J,K)*MIRROR1(1)
    FNEU1(I,J/2+(NXNEU(1)-1)/2,K)  = G_FELD1(I,J,K)
   ENDFOR
  ENDFOR
 ENDFOR
 YMIN = -YMAX
ENDIF


IF MIRROR(2) NE 0 THEN BEGIN
 FOR I=0, NXNEU(2)-1,2 DO BEGIN
  ZNEU(I/2)           =-Z(NXNEU(2)-1-I)
  ZNEU(I/2+(NXNEU(2)-1)/2)= Z(I)
 ENDFOR
 FOR K=0,NXNEU(2)-1,2 DO BEGIN
  FOR J=0,NXNEU(1)-1 DO BEGIN
   FOR I=0, NXNEU(0)-1 DO BEGIN
    FNEU(I,J,K/2)           = G_FELD(I,J,NXNEU(2)-1-K)*MIRROR(2)
    FNEU(I,J,K/2+(NXNEU(2)-1)/2)  = G_FELD(I,J,K)
   FNEU1(I,J,K/2)           = G_FELD1(I,J,NXNEU(2)-1-K)*MIRROR1(2)
    FNEU1(I,J,K/2+(NXNEU(2)-1)/2)  = G_FELD1(I,J,K)
  ENDFOR
 ENDFOR
ENDFOR
 ZMIN = -ZMAX
ENDIF

g_feld = fneu
g_feld1 = fneu1

nx=nxneu(0)
ny=nxneu(1)
nz=nxneu(2)
x=xneu
y=yneu
z=zneu

END
;********************************
pro psaus
;********************************
@bilder_com
!p.multi = [0,1,2]
set_plot, 'PS'
device, /inches,xoffset=0.5,yoffset=0.5,ysize=10, filename='bild.ps'
end
;********************************
pro bi ; bild in postscript-datei plotten
;********************************
@globan3

  set_plot, 'PS'
  device, filename= printfile

end

;********************************
pro screenaus
;********************************
@bilder_com
device, /close
set_plot, 'X'
end
;********************************
pro gitt
;********************************
@globan3
common numdat2d, anzx,anzy,xachse,yachse,fvonxy
common numnamen2d, ueberschrift1, ueberschrift2, ueberschrift3, $
		   xbezeichnung, ybezeichnung, zbezeichnung
if gittschon eq 'j' then begin
print, 'Mein Gedaechtnis!!!!'
print, 'Jetzt habe ich doch schon wieder die Original-Daten vergessen!'
print, 'Wuerdest Du mich bitte noch mal neu starten?'
print, 'BIIIIITTE, es kommt auch bestimmt nicht wieder vor!!!!'
print, 'GROSSES INDIANEREHRENWORT!!!!!!'
goto, jump1
endif
gitt='n'
nw  ='n'
gnw ='n'
plwl1   = 'bb'
plwl2   = 'cc'

NXNEU    = LINDGEN(3)
nxn1=nx
nxn2=ny
nxn3=nz
              
print, 'neues gitter (j/n)?'
read, gitt
case gitt of 'j': begin
                   print, 'xminneu(',xmin,') ,xmaxneu(',xmax,')'
                   print, 'yminneu(',ymin,') ,ymaxneu(',ymax,')'
                   print, 'zminneu(',zmin,') ,zmaxneu(',zmax,')'
                   print, ''
                   print, 'NEUWAHL ? (j/n)
                   read, nw
                       case nw of 'j': begin
                                       print, 'neue Grenzen?'
                                       read, xmin,xmax,ymin,ymax,zmin,zmax
                                       end
                   else: print, ''
                   endcase
                   print, 'nxalt(',nxn1,'), nyalt(',nxn2,'), nzalt(',nxn3,')'
                   print, 'NEUWAHL DER ZAHL DER GITTERPUNKTE ? (j/n)'
                   read, gnw
                   case gnw of 'j': begin
                   print, ''
                   print, 'neue Zahl der Gitterpunkte(jeder wert ungerade)?'
                   read, nxn1,nxn2,nxn3
                   end
                    else: print, ''
                    endcase
                    
                   NXNEU(0)=nxn1   
                   NXNEU(1)=nxn2
                   NXNEU(2)=nxn3
                   
                   GRIDNEU
                   gittschon = 'j'
                  end
else: print,''
endcase                  
jump1:               
print,''
print,'plg-Aufruf mit cp=1 fuer 1-dim Schnitt, cp=2 fuer Hoehenlinien'
print, 'cp=3 fuer Gebirge (Netz), cp=4 fuer Gebirge (shaded)'
print, 'cp=5 fuer Pfeile'                
end
;********************************
PRO PFEILE,U,V,X,Y, Missing = Missing, Length = length, Dots = dots,  $
        Color=color, _EXTRA = extra
;********************************
; offspring of routine VELOVECT
;
; Datei: /usr/local/lib/idl/lib/userlib/velovect.pro
; NAME:
;	VELOVECT
;
; PURPOSE:
;	Produce a two-dimensional velocity field plot.
;
;	A directed arrow is drawn at each point showing the direction and 
;	magnitude of the field.
;               
; CATEGORY:
;	Plotting, two-dimensional.
;
; CALLING SEQUENCE:
;	VELOVECT, U, V [, X, Y]
;
; INPUTS:
;	U:	The X component of the two-dimensional field.  
;		U must be a two-dimensional array.
;
;	V:	The Y component of the two dimensional field.  Y must have
;		the same dimensions as X.  The vector at point (i,j) has a 
;		magnitude of:
;
;			(U(i,j)^2 + V(i,j)^2)^0.5
;
;		and a direction of:
;
;			ATAN2(V(i,j),U(i,j)).
;
; OPTIONAL INPUT PARAMETERS:
; 	X:	Optional abcissae values.  X must be a vector with a length 
;		equal to the first dimension of U and V.
;
;	Y:	Optional ordinate values.  Y must be a vector with a length
;		equal to the first dimension of U and V.
;
; KEYWORD INPUT PARAMETERS:
;      MISSING:	Missing data value.  Vectors with a LENGTH greater
;		than MISSING are ignored.
;
;	LENGTH:	Length factor.  The default of 1.0 makes the longest (U,V)
;		vector the length of a cell.
;
;	DOTS:	Set this keyword to 1 to place a dot at each missing point. 
;		Set this keyword to 0 or omit it to draw nothing for missing
;		points.  Has effect only if MISSING is specified.
;
;	COLOR:	The color index used for the plot.
;
;	Note:   All other keywords are passed directly to the PLOT procedure
;		and may be used to set option such as TITLE, POSITION, 
;		NOERASE, etc.
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Plotting on the selected device is performed.  System
;	variables concerning plotting are changed.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Straightforward.  Unrecognized keywords are passed to the PLOT
;	procedure.  
;
; MODIFICATION HISTORY:
;	DMS, RSI, Oct., 1983.
;	For Sun, DMS, RSI, April, 1989.
;	Added TITLE, Oct, 1990.
;	Added POSITION, NOERASE, COLOR, Feb 91, RES.
;	August, 1993.  Vince Patrick, Adv. Visualization Lab, U. of Maryland,
;		fixed errors in math.
;	August, 1993. DMS, Added _EXTRA keyword inheritance.
;-
;
        on_error,2                      ;Return to caller if an error occurs
        s = size(u)
        t = size(v)
        if s(0) ne 2 then begin 
baduv:   message, 'U and V parameters must be 2D and same size.'
                endif
        if total(abs(s(0:2)-t(0:2))) ne 0 then goto,baduv
;
        if n_params(0) lt 3 then x = findgen(s(1)) else $
                if n_elements(x) ne s(1) then begin
badxy:                  message, 'X and Y arrays have incorrect size.'
                        endif
        if n_params(1) lt 4 then y = findgen(s(2)) else $
                if n_elements(y) ne s(2) then goto,badxy
;
        if n_elements(missing) le 0 then missing = 1.0e30
        if n_elements(length) le 0 then length = 1.0

        mag = sqrt(u^2+v^2)             ;magnitude.
                ;Subscripts of good elements
        nbad = 0                        ;# of missing points
        if n_elements(missing) gt 0 then begin
                good = where(mag lt missing) 
                if keyword_set(dots) then bad = where(mag ge missing, nbad)
        endif else begin
                good = lindgen(n_elements(mag))
        endelse

        ugood = u(good)
        vgood = v(good)
        x0 = min(x)                     ;get scaling
        x1 = max(x)
        y0 = min(y)
        y1 = max(y)
;
;	x_step=(x1-x0)/s(1)	;equidistant grid points
;	y_step=(y1-y0)/s(2)
;
;				;general grid spacing
;
	xstep = fltarr(s(1))
	xstepf = fltarr(s(1),s(2))
	ystep = fltarr(s(2))
	ystepf = fltarr(s(1),s(2))
;
	for i=1,s(1)-2	 do xstep(i) = 0.5*(x(i+1)-x(i-1))
	xstep(0) = x(1) - x(0)
	xstep(s(1)-1) = x(s(1)-1) - x(s(1)-2) 
	for i=0,s(1)-1 do xstepf(i,*) = xstep(i)
;
	for i=1,s(2)-2	 do ystep(i) = 0.5*(y(i+1)-y(i-1))
	ystep(0) = y(1) - y(0)
	ystep(s(2)-1) = y(s(2)-1) - y(s(2)-2) 
	for i=0,s(2)-1 do ystepf(*,i) = ystep(i)
;
	xstgood = xstepf(good)
	ystgood = ystepf(good)
	maxmag = max(sqrt(max((ugood/xstgood)^2+(vgood/ystgood)^2)))
;
;	maxmag=max([max(ugood/x_step),max(vgood/y_step)])
;
;	sina = length * (ugood/maxmag)
;	cosa = length * (vgood/maxmag)
	sina = length * (ugood/maxmag)
	cosa = length * (vgood/maxmag)
;
        if n_elements(title) le 0 then title = ''
        ;--------------  plot to get axes  ---------------
        if n_elements(color) eq 0 then color = !p.color
;
;       x_b0=x0-x_step		;equidistant grid
;	x_b1=x1+x_step
;	y_b0=y0-y_step
;	y_b1=y1+y_step
;
;
	x_b0=x0-xstep(0)	;general grid spacing
	x_b1=x1+xstep(s(1)-1)
	y_b0=y0-ystep(0)
	y_b1=y1+ystep(s(2)-1)
;
        if n_elements(position) eq 0 then begin
          plot,[x_b0,x_b1],[y_b1,y_b0],/nodata,/xst,/yst, $
            color=color, _EXTRA = extra
        endif else begin
          plot,[x_b0,x_b1],[y_b1,y_b0],/nodata,/xst,/yst, $
            color=color, _EXTRA = extra
        endelse
;
        r = .3                          ;len of arrow head
        angle = 22.5 * !dtor            ;Angle of arrowhead
        st = r * sin(angle)             ;sin 22.5 degs * length of head
        ct = r * cos(angle)
;
        for i=0,n_elements(good)-1 do begin     ;Each point
                dx = sina(i)
                x0 = x(good(i) mod s(1))-0.5*dx ;get coords of start & end
                x1 = x0 + dx			;arrows centered on grid
                dy = cosa(i)			;points (compass needle)
                y0 = y(good(i) / s(1))-0.5*dy
                y1 = y0 + dy
;		xd=x_step
;		yd=y_step
		xd = xstgood(i)
		yd = ystgood(i)
                plots,[x0,x1,x1-(ct*dx/xd-st*dy/yd)*xd, $
			x1,x1-(ct*dx/xd+st*dy/yd)*xd], $
                      [y0,y1,y1-(ct*dy/yd+st*dx/xd)*yd, $
			y1,y1-(ct*dy/yd-st*dx/xd)*yd], $
                      color=color
                endfor
        if nbad gt 0 then $             ;Dots for missing?
                oplot, x(bad mod s(1)), y(bad / s(1)), psym=3, color=color
end
;*************************************
;MAIN
;*************************************
@globan3

common numdat2d, anzx,anzy,xachse,yachse,fvonxy
common numnamen2d, ueberschrift1, ueberschrift2, ueberschrift3, $
		   xbezeichnung, ybezeichnung, zbezeichnung
les

;GLATT

feldwahl
gitt


END
           
