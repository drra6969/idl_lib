; Copyright (c) 1991, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	XSURF
;	this routine is adapted from XSURFACE and modified by D.L.
;	to accept x and y axis 
;
; PURPOSE:
;	This routine provides a graphical interface to the SURFACE and
;	SHADE_SURFACE commands.  Different controls are provided to change 
;	the viewing angle and other plot parameters.  The command used to 
;	generate the resulting surface plot is shown in a text window.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;	XSURFACE, Data, X_ax, Y_ax
;
; INPUT PARAMETERS:
;	Data:	The two-dimensional array to display as a wire-mesh or
;		shaded surface.
;	X_ax:   Array with data for the X-axis
;	Y_ax:   Array with data for the Y-axis
;
; KEYWORD PARAMETERS:
;	GROUP:	The widget ID of the widget that calls XSURFACE.  When this
;		keyword is specified, the death of the caller results in the
;		death of XSURFACE.
;       all keywords that are accepted by SURFACE are also accepted here
;
; SIDE EFFECTS:
;	The XMANAGER is initiated if it is not already running.
;
; RESTRICTIONS:
;	XSURFACE does not accept any of the keywords that the IDL command 
;	SURFACE does.
;
; PROCEDURE:
;	Create and register the widget with the XMANAGER and then exit.
;
; MODIFICATION HISTORY:
;	Created from a template written by: Steve Richards, January, 1991.
;	Modified by Dirk Lummerzheim, November, 1992.
;-

;------------------------------------------------------------------------------
;	procedure XSurface_draw
;------------------------------------------------------------------------------

PRO XSurface_draw, _extra=_extra

COMMON orientation, zrot, X_ax, Y_ax, thedata, xrot, skirt, shade, axes, thedraw, $
		xmargin, ymargin, upper, commandid

WSET, thedraw

IF(shade EQ 0) THEN BEGIN
  IF(skirt EQ 0) THEN $
    SURFACE, thedata, X_ax, Y_ax,  $
		XSTYLE = axes, $
		YSTYLE = axes, $
		ZSTYLE = axes, $
		UPPER_ONLY = upper, $
		XMARGIN = xmargin, $
		YMARGIN = ymargin, $
		AZ = zrot, $
		AX = xrot, $
                _extra=_extra $
  ELSE SURFACE, thedata, X_ax, Y_ax,  $
		XSTYLE = axes, $
		YSTYLE = axes, $
		ZSTYLE = axes, $
		UPPER_ONLY = upper, $
		XMARGIN = xmargin, $
		YMARGIN = ymargin, $
		AZ = zrot, $
		AX = xrot, $
		SKIRT = MIN(thedata), $
                _extra=_extra 
ENDIF ELSE BEGIN
  IF(skirt EQ 0) THEN $
    SHADE_SURF, thedata, X_ax, Y_ax,  $
		XSTYLE = axes, $
		YSTYLE = axes, $
		ZSTYLE = axes, $
		UPPER_ONLY = upper, $
		XMARGIN = xmargin, $
		YMARGIN = ymargin, $
		AZ = zrot, $
		AX = xrot , $
                _extra=_extra $
    ELSE SHADE_SURF, thedata, X_ax, Y_ax,  $
		XSTYLE = axes, $
		YSTYLE = axes, $
		ZSTYLE = axes, $
		UPPER_ONLY = upper, $
		XMARGIN = xmargin, $
		YMARGIN = ymargin, $
		AZ = zrot, $
		AX = xrot, $
		SKIRT = MIN(thedata), $
                _extra=_extra 
ENDELSE

IF(shade EQ 0) THEN command = "SURFACE, data" $
ELSE command = "SHADE_SURF, data"
IF(xrot NE 30.0) THEN command = command + STRING(xrot, $
					FORMAT = '(", AX = ",I3.3)')
IF(zrot NE 30.0) THEN command = command + STRING(zrot, $
					FORMAT = '(", AZ = ",I3.3)')
IF(skirt NE 0) THEN command = command + ", /SKIRT"
IF(xmargin(0) NE 10.0) THEN $
	command = command + STRING(xmargin, $
	FORMAT = '(", XMARGIN = [",F4.1,", ",F4.1,"]")')
IF(ymargin(0) NE 4.0) THEN $
	command = command + STRING(ymargin, $
	FORMAT = '(", YMARGIN = [",F4.1,", ",F4.1,"]")')
IF(upper NE 0) THEN command = command + ", /UPPER_ONLY"
IF(axes NE 0) THEN command = command + $
	", XSTYLE = 4, YSTYLE = 4, ZSTYLE = 4"

WIDGET_CONTROL, commandid, SET_VALUE = command

END


;------------------------------------------------------------------------------
;	procedure XSurface_ev
;------------------------------------------------------------------------------

PRO XSurf_ev, event

COMMON orientation, zrot, X_ax, Y_ax, thedata, xrot, skirt, shade, axes, thedraw, $
		xmargin, ymargin, upper, commandid
common c_extra, _extra

WIDGET_CONTROL, event.id, GET_UVALUE = eventval		;find the user value
							;of the widget where
							;the event occured
CASE eventval OF

  "       0": BEGIN
		zrot = (zrot + 15) mod 360
		IF(zrot LT 0) THEN zrot = 360 + zrot
		XSurface_draw, _extra=_extra
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	      END

  "       1": BEGIN
		zrot = (zrot - 15) mod 360
		IF(zrot LT 0) THEN zrot = 360 + zrot
		XSurface_draw, _extra=_extra
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	      END

  "       2": BEGIN
		xrot = (xrot - 15) mod 360
		IF(xrot LT 0) THEN xrot = 360 + xrot
		XSurface_draw, _extra=_extra
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	      END

  "       3": BEGIN
		xrot = (xrot + 15) mod 360
		IF(xrot LT 0) THEN xrot = 360 + xrot
		XSurface_draw, _extra=_extra
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	      END

  "       4": BEGIN	;shrink
		xmargin = xmargin * 1.2
		ymargin = ymargin * 1.2
		XSurface_draw, _extra=_extra
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	      END

  "       5": BEGIN	;grow
		xmargin = xmargin * 0.8
		ymargin = ymargin * 0.8
		XSurface_draw, _extra=_extra
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	      END

  "SKIRTON": IF(event.select EQ 1) THEN BEGIN
		skirt = 1
		XSurface_draw, _extra=_extra
	     ENDIF

  "SKIRTOFF":  IF(event.select EQ 1) THEN BEGIN
		skirt = 0
		XSurface_draw, _extra=_extra
	      ENDIF

  "SHADEOFF":  IF(event.select EQ 1) THEN BEGIN
		shade = 0
		XSurface_draw, _extra=_extra
	      ENDIF

  "SHADEON":  IF(event.select EQ 1) THEN BEGIN
		shade = 1
		XSurface_draw, _extra=_extra
	      ENDIF

  "AXESOFF":  IF(event.select EQ 1) THEN BEGIN
		AXES = 4
		XSurface_draw, _extra=_extra
	      ENDIF

  "AXESON":  IF(event.select EQ 1) THEN BEGIN
		AXES = 0
		XSurface_draw, _extra=_extra
	      ENDIF

  "UPPERON": IF(event.select EQ 1) THEN BEGIN
		upper = 0
		XSurface_draw, _extra=_extra
	      ENDIF

  "UPPEROFF": IF(event.select EQ 1) THEN BEGIN
		upper = 1
		XSurface_draw, _extra=_extra
	      ENDIF

  "XLOADCT": XLoadct, GROUP = event.top

  "XPALETTE": XPalette, GROUP = event.top

  "XMANTOOL": XManagerTool, GROUP = event.top

  "EXIT": WIDGET_CONTROL, event.top, /DESTROY

  ELSE:; MESSAGE, "Event User Value Not Found"

ENDCASE

END ;============= end of XSurf event handling routine task =============



;------------------------------------------------------------------------------
;	procedure XSurf
;------------------------------------------------------------------------------

PRO XSurf, DATA, X, Y, GROUP = GROUP, _extra=_extra

COMMON orientation, zrot, X_ax, Y_ax, thedata, xrot, skirt, shade, axes, thedraw, $
		xmargin, ymargin, upper, commandid
common c_extra, local_extra

if keyword_set(_extra) then local_extra=_extra

IF(XRegistered("XSurf")) THEN RETURN			;only one instance of
							;the XSurf widget
							;is allowed.  If it is
							;already managed, do
							;nothing and return

thesize = SIZE(DATA)
zrot = 30.
xrot = 30.
skirt = 0
shade = 0
axes = 0
xmargin = [10.0, 3.0]
ymargin = [4.0, 2.0]
upper = 0
commandid = 0L

XSurfacebase = WIDGET_BASE(TITLE = "XSurf", $
		/COLUMN)

XPdMenu, [	'"Done"				EXIT',		$
		'"Tools"	{',				$
				'"XLoadct"	XLOADCT',	$
				'"XPalette"	XPALETTE',	$
				'"XManagerTool"	XMANTOOL',	$
				'}'],				$
	 XSurfacebase

thebase = WIDGET_BASE(XSurfacebase, /ROW)

IF (!VERSION.OS EQ 'sunos') THEN $
  XSurfacepalette = WIDGET_BASE(thebase, $
		/COLUMN, $
		/FRAME, $
		/EXCLUSIVE) $
ELSE $
  XSurfacepalette = WIDGET_BASE(thebase, $
		/COLUMN, $
		/FRAME)

controls = [							$
		[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 248B, 255B, 063B, 000B],			$
		[128B, 007B, 000B, 224B, 001B],			$
		[112B, 000B, 000B, 000B, 014B],			$
		[136B, 000B, 016B, 000B, 016B],			$
		[052B, 000B, 048B, 000B, 056B],			$
		[172B, 000B, 080B, 000B, 032B],			$
		[124B, 000B, 144B, 000B, 040B],			$
		[164B, 007B, 016B, 001B, 048B],			$
		[012B, 248B, 031B, 062B, 056B],			$
		[036B, 000B, 000B, 228B, 033B],			$
		[004B, 000B, 000B, 008B, 062B],			$
		[012B, 000B, 000B, 016B, 048B],			$
		[036B, 000B, 000B, 016B, 032B],			$
		[008B, 000B, 000B, 008B, 000B],			$
		[112B, 000B, 000B, 004B, 000B],			$
		[128B, 007B, 000B, 002B, 000B],			$
		[000B, 248B, 031B, 001B, 000B],			$
		[000B, 000B, 144B, 000B, 000B],			$
		[000B, 000B, 080B, 000B, 000B],			$
		[000B, 000B, 048B, 000B, 000B],			$
		[000B, 000B, 016B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
;		dnz.bmdef
		[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 252B, 255B, 031B, 000B],			$
		[128B, 007B, 000B, 224B, 001B],			$
		[112B, 000B, 000B, 000B, 014B],			$
		[008B, 000B, 008B, 000B, 017B],			$
		[028B, 000B, 012B, 000B, 044B],			$
		[004B, 000B, 010B, 000B, 053B],			$
		[020B, 000B, 009B, 000B, 062B],			$
		[012B, 128B, 008B, 224B, 037B],			$
		[028B, 124B, 248B, 031B, 048B],			$
		[132B, 039B, 000B, 000B, 036B],			$
		[124B, 016B, 000B, 000B, 032B],			$
		[012B, 008B, 000B, 000B, 048B],			$
		[004B, 008B, 000B, 000B, 036B],			$
		[000B, 016B, 000B, 000B, 016B],			$
		[000B, 032B, 000B, 000B, 014B],			$
		[000B, 064B, 000B, 224B, 001B],			$
		[000B, 128B, 248B, 031B, 000B],			$
		[000B, 000B, 009B, 000B, 000B],			$
		[000B, 000B, 010B, 000B, 000B],			$
		[000B, 000B, 012B, 000B, 000B],			$
		[000B, 000B, 008B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
		;upz.bm
		[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 128B, 255B, 001B, 000B],			$
		[000B, 064B, 169B, 002B, 000B],			$
		[000B, 160B, 254B, 007B, 000B],			$
		[000B, 160B, 068B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 008B, 016B, 000B, 000B],			$
		[000B, 008B, 016B, 000B, 000B],			$
		[000B, 008B, 144B, 001B, 000B],			$
		[000B, 008B, 112B, 006B, 000B],			$
		[000B, 008B, 016B, 008B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 006B, 096B, 000B],			$
		[000B, 008B, 001B, 128B, 000B],			$
		[000B, 136B, 000B, 000B, 001B],			$
		[000B, 200B, 015B, 240B, 003B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 016B, 004B, 008B, 000B],			$
		[000B, 016B, 004B, 008B, 000B],			$
		[000B, 016B, 005B, 008B, 000B],			$
		[000B, 080B, 004B, 008B, 000B],			$
		[000B, 032B, 147B, 004B, 000B],			$
		[000B, 160B, 042B, 005B, 000B],			$
		[000B, 064B, 149B, 002B, 000B],			$
		[000B, 128B, 255B, 001B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
		;dnx.bm
		[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 128B, 255B, 001B, 000B],			$
		[000B, 064B, 149B, 002B, 000B],			$
		[000B, 160B, 042B, 005B, 000B],			$
		[000B, 032B, 147B, 004B, 000B],			$
		[000B, 080B, 004B, 008B, 000B],			$
		[000B, 016B, 005B, 008B, 000B],			$
		[000B, 016B, 004B, 008B, 000B],			$
		[000B, 016B, 004B, 008B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 200B, 015B, 240B, 003B],			$
		[000B, 136B, 000B, 000B, 001B],			$
		[000B, 008B, 001B, 128B, 000B],			$
		[000B, 008B, 006B, 096B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 016B, 008B, 000B],			$
		[000B, 008B, 112B, 006B, 000B],			$
		[000B, 008B, 144B, 001B, 000B],			$
		[000B, 008B, 016B, 000B, 000B],			$
		[000B, 008B, 016B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 160B, 068B, 000B, 000B],			$
		[000B, 160B, 254B, 007B, 000B],			$
		[000B, 064B, 169B, 002B, 000B],			$
		[000B, 128B, 255B, 001B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
		;shrink.bm
		[						$
		[000B, 000B, 008B, 000B, 000B],			$
		[000B, 000B, 008B, 000B, 000B],			$
		[000B, 000B, 073B, 000B, 000B],			$
		[000B, 000B, 042B, 000B, 000B],			$
		[000B, 000B, 028B, 000B, 000B],			$
		[000B, 000B, 008B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[128B, 255B, 255B, 255B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[132B, 000B, 000B, 000B, 033B],			$
		[136B, 000B, 000B, 000B, 017B],			$
		[144B, 000B, 000B, 000B, 009B],			$
		[191B, 000B, 000B, 000B, 253B],			$
		[144B, 000B, 000B, 000B, 009B],			$
		[136B, 000B, 000B, 000B, 017B],			$
		[132B, 000B, 000B, 000B, 033B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 255B, 255B, 255B, 001B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 008B, 000B, 000B],			$
		[000B, 000B, 028B, 000B, 000B],			$
		[000B, 000B, 042B, 000B, 000B],			$
		[000B, 000B, 073B, 000B, 000B],			$
		[000B, 000B, 008B, 000B, 000B],			$
		[000B, 000B, 008B, 000B, 000B]			$
		],						$
		;grow.bm
		[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[248B, 255B, 255B, 255B, 031B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[008B, 000B, 008B, 000B, 016B],			$
		[008B, 000B, 028B, 000B, 016B],			$
		[008B, 000B, 042B, 000B, 016B],			$
		[008B, 000B, 073B, 000B, 016B],			$
		[008B, 000B, 008B, 000B, 016B],			$
		[008B, 000B, 008B, 000B, 016B],			$
		[008B, 000B, 008B, 000B, 016B],			$
		[008B, 000B, 008B, 000B, 016B],			$
		[008B, 000B, 008B, 000B, 016B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[008B, 001B, 000B, 000B, 016B],			$
		[136B, 000B, 000B, 128B, 016B],			$
		[072B, 000B, 000B, 000B, 017B],			$
		[232B, 063B, 000B, 000B, 018B],			$
		[072B, 000B, 000B, 252B, 023B],			$
		[136B, 000B, 000B, 000B, 018B],			$
		[008B, 001B, 000B, 000B, 017B],			$
		[008B, 000B, 000B, 128B, 016B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[008B, 000B, 016B, 000B, 016B],			$
		[008B, 000B, 016B, 000B, 016B],			$
		[008B, 000B, 016B, 000B, 016B],			$
		[008B, 000B, 016B, 000B, 016B],			$
		[008B, 000B, 146B, 000B, 016B],			$
		[008B, 000B, 084B, 000B, 016B],			$
		[008B, 000B, 056B, 000B, 016B],			$
		[008B, 000B, 016B, 000B, 016B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[248B, 255B, 255B, 255B, 031B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		]						$
	   ]

FOR i = 0,N_ELEMENTS(controls(0,0,*))-1 DO $
  toss = WIDGET_BUTTON(XSurfacepalette, $
		VALUE = controls(*,*,i), $
		UVALUE = STRING(i))

XSurfacedisplay = WIDGET_DRAW(thebase, $
		XSIZE = 675, $
		YSIZE = 600, $
		RETAIN = 2)

XSurfacecontrols = WIDGET_BASE(XSurfacebase, $
		/ROW)

skirtbase = WIDGET_BASE(XSurfacecontrols, $
		/COLUMN, $
		/EXCLUSIVE, $
		/FRAME)

skirtoff = WIDGET_BUTTON(skirtbase, $
		VALUE = "No Skirt", $
		UVALUE = "SKIRTOFF")

skirton = WIDGET_BUTTON(skirtbase, $
		VALUE = "Skirt", $
		UVALUE = "SKIRTON")

shadebase = WIDGET_BASE(XSurfacecontrols, $
		/COLUMN, $
		/EXCLUSIVE, $
		/FRAME)

shadeoff = WIDGET_BUTTON(shadebase, $
		VALUE = "Wire Frame", $
		UVALUE = "SHADEOFF")

shadeon = WIDGET_BUTTON(shadebase, $
		VALUE = "Shaded Surface", $
		UVALUE = "SHADEON")


axesbase = WIDGET_BASE(XSurfacecontrols, $
		/COLUMN, $
		/EXCLUSIVE, $
		/FRAME)

axeson = WIDGET_BUTTON(axesbase, $
		VALUE = "Show Axes", $
		UVALUE = "AXESON")

axesoff = WIDGET_BUTTON(axesbase, $
		VALUE = "Hide Axes", $
		UVALUE = "AXESOFF")

upperbase = WIDGET_BASE(XSurfacecontrols, $
		/COLUMN, $
		/EXCLUSIVE, $
		/FRAME)

upperon = WIDGET_BUTTON(upperbase, $
		VALUE = "Show Top and Bottom", $
		UVALUE = "UPPERON")

upperoff = WIDGET_BUTTON(upperbase, $
		VALUE = "Only Show Top", $
		UVALUE = "UPPEROFF")

commandbase = WIDGET_BASE(XSurfacebase, $
		/FRAME, $
		/COLUMN)

commandlabel = WIDGET_LABEL(commandbase, $
		VALUE = "IDL Commmand To Produce Above Output:")

IF (!VERSION.OS EQ 'sunos') THEN $
  commandid = WIDGET_LABEL(commandbase, $
		VALUE = "SURFACE, data") $
ELSE $
  commandid = WIDGET_TEXT(commandbase, $
		VALUE = "SURVACE, data", $
		/SCROLL, $
		YSIZE = 1)

WIDGET_CONTROL, XSurfacebase, /REALIZE			;create the widgets
							;that is defined

WIDGET_CONTROL, skirtoff, /SET_BUTTON
WIDGET_CONTROL, shadeoff, /SET_BUTTON
WIDGET_CONTROL, axeson, /SET_BUTTON
WIDGET_CONTROL, upperon, /SET_BUTTON
WIDGET_CONTROL, XSurfacedisplay, GET_VALUE = temp & thedraw = temp

IF(N_PARAMS() gt 0) THEN BEGIN
	thedata = DATA
	X_ax = X
	Y_ax = Y
	XSurface_draw, _extra=_extra
END

XManager, "XSurf", XSurfacebase, $			;register the widgets
		EVENT_HANDLER = "XSurf_ev", $		;with the XManager
		GROUP_LEADER = GROUP

END ;================ end of XSurf background task =====================



