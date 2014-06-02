PRO path

A = RANDOMU(seed, 8, 10)           ;Create a 2D array of random numbers.

B = MIN_CURVE_SURF(A)                ;Smooth the dataset before contouring.

CONTOUR, B, PATH_XY=xy, PATH_INFO=info;Compute contour paths.

FOR I = 0, (N_ELEMENTS(info) - 1 ) DO BEGIN

S = [INDGEN(info(I).N), 0]

PLOTS, xy(*,INFO(I).OFFSET + S ), /NORM;Plot the closed paths.

print,'xy',xy(*,INFO(I).OFFSET + S )
ENDFOR
END

