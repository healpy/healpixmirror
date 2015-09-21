; -----------------------------------------------------------------------------
;
;  Copyright (C) 1997-2013  Krzysztof M. Gorski, Eric Hivon, Anthony J. Banday
;
;
;
;
;
;  This file is part of HEALPix.
;
;  HEALPix is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  HEALPix is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with HEALPix; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;  For more information about HEALPix see http://healpix.sourceforge.net
;
; -----------------------------------------------------------------------------
PRO cartview, file_in, select_in, $
              ASINH = asinh, $
              BAD_COLOR = bad_color, $
              BG_COLOR = bg_color, $
              CHARSIZE = charsize, $
              CHARTHICK = charthick, $
              COLT = colt, $
              COORD = coord, $
              CROP = crop, $
              EXECUTE = execute, $
              FACTOR = factor, $
              FITS = fits, $
              FG_COLOR = fg_color, $
              FLIP = flip, $
              GIF = gif, $
              GLSIZE = glsize, $
              GRATICULE = graticule, $
              HBOUND = hbound, $
              HELP = help, $
              HIST_EQUAL = hist_equal, $
              HXSIZE = hxsize, $
              IGLSIZE = iglsize, $
              IGRATICULE = igraticule, $
              JPEG=jpeg, $
              LATEX=latex, $
              LOG = log, $
              MAP_OUT=map_out, $
              MAX = max_set, $
              MIN = min_set, $
              NESTED = nested_online, $
              NOBAR = nobar, $
              NOLABELS = nolabels, $
              NOPOSITION = noposition, $
              OFFSET=offset, $
              ONLINE = online, $
              OUTLINE = outline, $
              PDF = pdf, $
              PFONTS = pfonts, $
              PNG = png, $
              POLARIZATION = polarization, $
              PREVIEW = preview, $
              PS = ps, $
              PXSIZE = pxsize, $
              PYSIZE = pysize, $
              QUADCUBE = quadcube, $
              RESO_ARCMIN = reso_arcmin, $
              RETAIN = retain, $
              ROT = rot, $
              SAVE = save, $
              SILENT = silent, $
              SUBTITLE = subtitle, $
              TITLEPLOT = titleplot, $
              TRANSPARENT = transparent, $
              TRUECOLORS = truecolors, $
              UNITS = units, $
              WINDOW = window, $
              XPOS = xpos, $
              YPOS = ypos, $
              vector_scale = vector_scale
;+
; for extended description see mollview or the paper documentation
;-
defsysv, '!healpix', exists = exists
if (exists ne 1) then init_healpix

@viewcom ; define common
data_plot = 0 ; empty common array
; record original color table and PLOTS settings
record_original_settings, original_settings

loadsky                         ; cgis package routine, define rotation matrices
projection = 'CARTESIAN'
routine = 'cartview'

uroutine = strupcase(routine)
if keyword_set(help) then begin
    doc_library,'mollview'
    return
endif

if keyword_set(gif) then begin
    message_gif, code=routine, error=error_gif
    if (error_gif) then return
endif

if (n_params() lt 1 or n_params() gt 2) then begin
    PRINT, 'Wrong number of arguments in '+uroutine
    print,'Syntax : '
    print, uroutine+', File, [Select, ]'
    print,'              [ASINH=, '
    print,'              BAD_COLOR=, BG_COLOR=, '
    print,'              CHARSIZE=, CHARTHICK=, COLT=, COORD=, CROP=, '
    print,'              EXECUTE=, FACTOR=, FG_COLOR=, FITS=, FLIP=, GIF=, GLSIZE=, GRATICULE=, '
    print,'              HBOUND, HELP=, '
    print,'              HIST_EQUAL=, HXSIZE=,'
    print,'              IGLSIZE=, IGRATICULE=,'
    print,'              JPEG=,'
    print,'              LATEX=, LOG=, '
    print,'              MAX=, MIN=, NESTED=, NOBAR=, NOLABELS=, NOPOSITION= '
;    print,'              NO_DIPOLE, NO_MONOPLE, '
    print,'              OFFSET=, ONLINE=, OUTLINE=,'
    print,'              PDF=, PFONTS=, PNG=,'
    print,'              POLARIZATION=, PREVIEW=, '
    print,'              PS=, PXSIZE=, PYSIZE=, QUADCUBE= ,'
    print,'              RESO_ARCMIN=, RETAIN=, ROT=, '
    print,'              SAVE=, SILENT=, SUBTITLE=, '
    print,'              TITLEPLOT=, TRANSPARENT=, TRUECOLORS= '
    print,'              UNITS=, WINDOW=, XPOS=, YPOS=]'
    print
    print,' Type '+uroutine+', /help '
    print,'   for an extended help'
    return
endif

IF (undefined(file_in)) then begin
    print,routine+': Undefined variable as 1st argument'
    return
endif
; file_in1   = file_in
; if defined(select_in) then select_in1 = select_in else select_in1=1
; if defined(save)      then save1 = save           else save1=0
; if defined(online)    then online1 = online       else online1=0
do_flip = keyword_set(flip)

if (!D.n_colors lt 4) then begin
    print,' : Sorry ... not enough colors ('+strtrim(string(!d.n_colors),2)+') available'
    return
endif

polar_type = 0
if keyword_set(polarization) then polar_type = polarization


loaddata_healpix, $
  file_in, select_in,$
  data, pol_data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat, title_display, sunits, $
  SAVE=save,ONLINE=online,NESTED=nested_online,UNITS=units,COORD=coord,FLIP=flip, $
  ROT=rot,QUADCUBE=quadcube,LOG=log,ERROR=error, $
  POLARIZATION=polarization, FACTOR=factor, OFFSET=offset, SILENT=silent, COMPRESS=1, $
  PIXEL_LIST=pixel_list, TRUECOLORS=truecolors, DATA_TC=data_tc
if error NE 0 then return


data2cart, $
  data, pol_data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat, $
  planmap, Tmax, Tmin, color_bar, dx, planvec, vector_scale, $
  PXSIZE=pxsize, PYSIZE=pysize, ROT=rot, LOG=log, HIST_EQUAL=hist_equal, $
  MAX=max_set, MIN=min_set, $
  RESO_ARCMIN = reso_arcmin, FITS = fits, FLIP=flip, DATA_plot = data_plot, $
  POLARIZATION=polarization, SILENT=silent, PIXEL_LIST=pixel_list, ASINH=asinh, $
  TRUECOLORS=truecolors, DATA_TC=data_tc, MAP_OUT=arg_present(map_out) ? map_out : -1

proj2out, $
  planmap, Tmax, Tmin, color_bar, dx, title_display, $
  sunits, coord_out, do_rot, eul_mat, planvec, vector_scale, $
  CHARSIZE=charsize, COLT=colt, CROP=crop, GIF = gif, GRATICULE = graticule, HXSIZE = hxsize, $
  NOBAR = nobar, NOLABELS = nolabels, NOPOSITION = noposition, PNG = png, PREVIEW = preview, PS = ps, $
  PXSIZE=pxsize, PYSIZE=pysize, ROT = rot, SUBTITLE = subtitle, $
  TITLEPLOT = titleplot, XPOS = xpos, YPOS = ypos, $
  POLARIZATION=polarization, OUTLINE=outline, /CART, FLIP=flip, COORD_IN=coord_in, IGRATICULE=igraticule, $
  HBOUND = hbound, WINDOW = window, TRANSPARENT = transparent, EXECUTE=execute, $
  SILENT=silent, GLSIZE = glsize, IGLSIZE = iglsize, RETAIN=retain, TRUECOLORS=truecolors, CHARTHICK=charthick, $
  JPEG=jpeg, BAD_COLOR=bad_color, BG_COLOR=bg_color, FG_COLOR=fg_color, PDF=pdf, $
  LATEX=latex, PFONTS=pfonts


w_num = !d.window
; restore original color table and PLOTS settings
record_original_settings, original_settings, /restore

RETURN
END

