; -----------------------------------------------------------------------------
;
;  Copyright (C) 1997-2010  Krzysztof M. Gorski, Eric Hivon, Anthony J. Banday
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
;  For more information about HEALPix see http://healpix.jpl.nasa.gov
;
; -----------------------------------------------------------------------------
; ------------------------------------------------------------------------
PRO fast_read_fits_ext, filename, T_sky, silent=silent, pixel=pixel
;
; routine to read fast FITS extension, with one field, and one entry
; per field (ie, TFORM1 = 1x, for backward compatibility)
;
;


	exthdr = headfits(filename, exten=1)

	; identify the format in the row (number of entry per row)
	tform1 = SXPAR(exthdr,'TFORM1')
	nentry = ROUND(FLOAT(tform1))
	if (nentry gt 1) then begin
		stop,'STOP :  can NOT use fast_read_fits_ext'
	endif
	tform1 = STRMID(STRUPCASE(STRTRIM(tform1,2)),0,2) ; first 2 characters of TFORM1
	ndata = SXPAR(exthdr,'NAXIS2')
	ndata = LONG(ndata)

        startrow = 0 ; that is the way readfits works
        numrow   = ndata
        if (n_elements(pixel) eq 1 and pixel[0] ge 0) then begin
            startrow = pixel[0]
            numrow   = ndata - startrow
        endif
        if (n_elements(pixel) eq 2 and pixel[0] ge 0) then begin
            startrow = pixel[0]
            numrow   = pixel[1] - startrow + 1
        endif

	; read the whole first extension (that's the easy part)
	full_data = readfits(filename,/exten, silent=silent, startrow=startrow, numrow=numrow) ; gets a binary table

	; extracts the bytes corresponding to the 1st column
	; and stick them together to convert them in real, double ...
	case tform1 of 
	'1I' :  begin  ; integer (2bytes)
		idltype = 2 
		T_sky = full_data(0:1,*) ; 1st column
		T_sky = FIX(T_sky,0,numrow)
		end
	'1J' :  begin  ; long (4bytes)
		idltype = 3 
		T_sky = full_data(0:3,*) ; 1st column
		T_sky = LONG(T_sky,0,numrow)
		end
	'1E' :  begin ; floating point (4bytes)
		idltype = 4 
		T_sky = full_data(0:3,*) ; 1st column
		T_sky = FLOAT(T_sky,0,numrow) 
		end
	'1D' :  begin  ; DP floating point (8bytes)
		idltype = 5 
		T_sky = full_data(0:7,*) ; 1st column
		T_sky = DOUBLE(T_sky,0,numrow)
		end
	else : 	begin
		stop,'STOP : unknown format in fast_read_fits_ext'
		end
	endcase
	; turns IEEE into host convention
	IEEE_TO_HOST, T_sky, IDLTYPE=idltype

RETURN
END
; ------------------------------------------------------------------------



PRO READ_FITS_MAP, filename, T_sky, hdr, exthdr, SILENT=silent, PIXEL=pixel, NSIDE=nside, ORDERING=ordering, COORDSYS=coordsys

;+
; NAME:
;       READ_FITS_MAP
;
; PURPOSE:
; 	reads the temperature map and possibly the polarisation map in binary table
; 	extension of FITS file filename
; 	if doesn't find an extension, look in the image part
;
;       T_sky is returned as a 2-dim array 
;       1st dim = number of pixels
;       2nd dim = number of maps present in the file
;
; CALLING SEQUENCE:
; 	READ_FITS_MAP, filename, T_sky, [hdr, exthdr, SILENT=, PIXEL=, NSIDE=,
; 	ORDERING=, COORDSYS=]
; 
; INPUTS:
;	filename = String containing the name of the file to be read.
;
; OPTIONAL OUTPUTS:
;	hdr = String array containing the header from the FITS file.
;
;	exthdr = String array containing the header from the extension
;
;       NSIDE = Healpix resolution parameter read from FITS file, set to -1 if
;       not found
;
;       ORDERING = pixel ordering, as read from FITS header, either 'RING' or
;       'NESTED' or ' ' (unkwnown)
;
;       COORDSYS = astrophysical coordinate system used, as read from FITS
;           header (value of keywords COORDSYS or SKYCOORD)
;
;
; OPTIONAL KEYWORDS:
;    SILENT : if set, no message is issued during normal execution
;
;    PIXEL      = pixel number to read from or pixel range to read
;                 (in the order of appearance in the file), starting from 0.
;               if >= 0 scalar        : read from pixel to the end of the file
;               if two elements array : reads from pixel[0] to pixel[1] (included)
;               if absent             : read the whole file
;
; PROCEDURES USED:
;	several (see below)
;
; MODIFICATION HISTORY:
;  October 1997:
;  *strip down edited by E. Hivon from the READ_FITS_MAP.pro 
;  written by A. Banday for COBE data analysis.
;  *added the image reading if no extension, EH		Nov-97
;  *made it faster by using READFITS,/exten		Dec-97
;  addition of /SLOW (=previous implementation using FXBREAD)
;  *made it compatible with multiple entry per row and per column
;  (eg TFORM = '1024E') wich makes the reading faster.  Jan-98
;  *addition of PIXEL                                   Aug-02
;        Jan 2005: added NISDE and ORDERING as outputs
;  May 2005, EH, replaces FINDFILE by FILE_TEST
;  Jan 2008, EH: calls tbfree to remove heap pointer created by TBINFO
;  Jan 2009: calls init_astrolib
;
; requires the THE IDL ASTRONOMY USER'S LIBRARY 
; that can be found at http://idlastro.gsfc.nasa.gov/homepage.html
;
;-

syntax = 'Syntax : READ_FITS_MAP, filename, T_sky [, hdr, exthdr, PIXEL=, SILENT=, NSIDE=, ORDERING=, COORDSYS=]'
syntax2 = '   No File Read, Abort !'
if N_params() LT 2 then begin
    print,syntax
    print,syntax2
    retall
endif

; run astrolib routine to set up non-standard system variables
init_astrolib

; get the primary header information
if (datatype(filename) ne 'STR') then begin
    print,syntax
    print,'Invalid file name'
    message,syntax2
endif

if (not file_test(filename)) then begin
    prim_stc = 0 & xten_stc = 0
    message, 'file '+filename+' not found'
endif

hdr = headfits(filename, errmsg=errmsg)
if (strtrim(errmsg) ne '') then message,errmsg

; checks for the existence of an extension
; if none, reads the image
fits_info,filename, /silent, n_ext=n_ext
if (n_ext eq 0) then begin
    T_sky = readfits(filename,hdr, silent=silent)
    exthdr = ' '
    return
endif

; identify the format in the row (number of entry per row)
;exthdr = headfits(filename, exten=1)
fits_read, filename, junk, exthdr, /header_only, /no_pdu, exten_no=1

nside  = LONG(       SXPAR(exthdr,'NSIDE',count=count  ))
if (count eq 0) then nside = -1
ordering =  strupcase(strtrim(SXPAR(exthdr,'ORDERING', count=count),2))
if (count eq 0) then ordering = ' '
coordsys = sxpar(exthdr,'COORDSYS', count=count)
if (count eq 0) then coordsys = sxpar(exthdr,'SKYCOORD', count=count)
if (count eq 0) then coordsys = sxpar(hdr,'COORDSYS', count=count)
if (count eq 0) then coordsys = sxpar(hdr,'SKYCOORD', count=count)
if (count eq 0) then coordsys = ' ' else coordsys = strtrim(coordsys,2)

tbinfo, exthdr, tab_xhdr
;nentry = ROUND(FLOAT(SXPAR(exthdr,'TFORM1' )))
nentry = max(tab_xhdr.numval)
nmaps  = ROUND(FLOAT(SXPAR(exthdr,'TFIELDS')))
;naxis2 = LONG(       SXPAR(exthdr,'NAXIS2' ))
naxis2 = DOUBLE(       SXPAR(exthdr,'NAXIS2' ))
ndata = nentry * naxis2
n_wpr = LONG(total(tab_xhdr.width*tab_xhdr.numval)) ; number of bytes per row
type = max(tab_xhdr.idltype) ; find out type of output array to fit all data
junk = where(tab_xhdr.idltype eq 7, n_string)
if (n_string ge 1) then begin
    message,'Table in input file contains strings'
endif

; find bytes to read
max_byte = naxis2*n_wpr - 1L
max_pix  = ndata - 1L
firstbyte = 0L
lastbyte  = max_byte
firstpix = 0L
lastpix = max_pix
cut = 0
if (defined(pixel)) then begin
    if (n_elements(pixel) ge 1 and pixel[0] ge 0) then begin
        firstpix  = LONG(pixel[0])
        firstbyte = (firstpix/nentry)*n_wpr
    endif
    if (n_elements(pixel) eq 2) then begin
        if (pixel[1] lt pixel[0]) then begin
            print,pixel
            message,'Invalid pixel range'
        endif
        lastpix   = LONG(pixel[1])
        lastbyte  = (lastpix/nentry)*n_wpr + n_wpr - 1L
    endif
    cut = 1
endif
if (firstpix gt max_pix) then begin
    print,'range : ',firstpix,lastpix,',  available : ',max_pix+1
    message,'Invalid pixel range'
endif
if (lastpix gt max_pix) then begin
    print,'Only ',max_pix+1,' pixels available in '+filename+', will truncate the output array'
    lastpix  = lastpix  < max_pix
    lastbyte = lastbyte < max_byte
endif

; read bytes
if (cut) then begin
    nrows = (lastbyte - firstbyte + 1L)/n_wpr
    npix = (lastpix - firstpix + 1L)
    fits_read, filename, data, exten_no = 1, /no_pdu, first=firstbyte, last=lastbyte
    data = reform(data,n_wpr,nrows,/overwrite)
endif else begin
    npix = ndata
    fits_read, filename, data, exten_no = 1, /no_pdu
endelse

; convert bytes into values
T_sky = make_array(type=type,npix,nmaps,/nozero)
for i=0,nmaps-1 do begin
    nent = tab_xhdr.numval[i]
    fp = firstpix - (firstpix/nent)*nent
    lp = fp + npix - 1
    x = tbget(tab_xhdr, data, i+1)
    T_sky[*,i] = (x)[fp:lp]
endfor
data = 0
tbfree, tab_xhdr

; if (nentry EQ 1 AND nmaps EQ 1) then begin
;     fast_read_fits_ext,filename,T_sky, silent=silent, pixel=pixel_f
; endif else begin

;     ; open the fits file
;     fxbopen,lun,filename,1,exthdr ; sky map is binary extnsion table #1
;     ; ----------------------------------------------

;     nmaps = ROUND(FLOAT(SXPAR(exthdr,'TFIELDS')))
;     ;nmaps_read = N_params() - 1

;     ; read in the required columns
;     fxbread,lun,map_tmp,1, row  ; pixel temperature is column #1

;     ss = size(map_tmp)
;     T_sky = MAKE_ARRAY(npix,nmaps,TYPE= ss(ss(0)+1) )

;     ; if #entry/row > 1 (eg TFORM = '1024x') one gets an array
;     ; turn it to a vector 
;     T_sky[*,0] = map_tmp[s0:s1]
;     ; ----------------------------------------------
;     for icoln = 2, nmaps do begin
;         fxbread,lun,map_tmp,icoln, row

;         ; if #entry/row > 1 (eg TFORM = '1024x') one gets an array
;         ; turn it to a vector 
;         T_sky[*,icoln-1] = map_tmp[s0:s1]
;     endfor

;     ; free the file
;     fxbclose,lun
; endelse

; Exit, stage left ....
return
end

