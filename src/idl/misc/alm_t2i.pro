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
pro alm_t2i, alm_tab, index, alm_list
;+
; alm_t2i, alm_tab, index, alm_list
;
;  turn an alm COMPLEX array  a(l,m) (default)
;
;  into an index list (with index = l*(l+1)+m+1)
;  and a 2D REAL array alm_list
;    where alm_list[*,0] is the real part of alm
;    and   alm_list[*,1] is the imaginary part of alm
;
;
; index and alm_list can then be used by alm2fits
;
; internally Healpix use complex arrays of the form a(l,m)
; (where l is the first index ie, the fastest varying one)
;
;-

syntax = 'ALM_T2I, alm_tab, index, alm_list '
if (n_params() eq 0) then begin
    print,syntax
    return
endif
if (n_params() ne 3) then begin
    print,syntax
    message,'Abort'
endif

lf = 1
if keyword_set(mfirst) then lf = 0

alm = alm_tab

; generates l and m arrays
nm = n_elements(alm[*,0])
nl = n_elements(alm[0,*])
mtab = indgen(nm) # replicate(1,nl)
ltab = replicate(1,nm) # indgen(nl)

; find points where m <= l
k = where(mtab le ltab,nk)

; define index = l*(l+1)+m+1 for those points
index = ((ltab+1L)*ltab + mtab)[k] + 1L

; fill the alm list with the real and imaginary parts
alm_list = fltarr(nk,2)
alm_list[*,0] = float(alm[k])
alm_list[*,1] = imaginary(alm[k])



return
end

