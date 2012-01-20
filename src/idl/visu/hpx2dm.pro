pro hpx2dm, file_in, select_in, $
                     _extra = other_kwds, $
                     gif = gif, $ ; not allowed
                     help = help, $
                     pxsize = pxsize
;+
; NAME:
;      HPX2DM
;
; PURPOSE:
;    Turns a Healpix data set into DomeMaster compliant image
;   (azimuthal equidistant projection of the half-sphere)
;   in PNG or lossless JPEG file
;
; CATEGORY:
;
; CALLING SEQUENCE:
;   Hpx2dm, file [,select ...]
;   + all remaining CARTVIEW's keywords 
;
;
; INPUTS:
; 	File = 
;          by default,           name of a FITS file containing 
;               the healpix map in an extension or in the image field
;          or    name of a variable containing the healpix map
;          if Save is set   :    name of an IDL saveset file containing
;               the healpix map stored under the variable  data
;
; OPTIONAL INPUTS:
;       Select =  if the file read is a multi column BIN table, Select indicates
;                 which column is to be plotted (the default is to plot the
;                 first column containing some signal, as opposed to pixel index)
;               can be either a name : value given in TTYPEi of the FITS file
;                        NOT case sensitive and
;                        can be truncated, 
;                        (only letters, digits and underscore are valid)
;               or an integer        : number i of the column
;                            containing the data, starting with 1
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-

routine = 'HPX2DM'
syntax = [routine+', File [, Select ',$
          '       <+ most of CARTVIEW''s keywords> ]']

if (keyword_set(help)) then begin
    doc_library,routine
    return
endif

if (n_params() eq 0) then begin
    print,syntax,format='(a)'
    print,'Type: '+routine+', /help'
    print,'   for more help.'
    return
endif
;-----------------

; forced : CROP, HALF_SKY, RESO_ARCMIN, WINDOW
mypxsize = defined(pxsize) ? pxsize : 1024
reso_arcmin = 180.d0 * 60.d0 / mypxsize
azeqview, file_in, select_in, $
          crop = 1, $
          half_sky = 1, $
          pxsize = mypxsize, $
          reso_arcmin = reso_arcmin, $
          _strict_extra = other_kwds, $
          window = -1

return
end

