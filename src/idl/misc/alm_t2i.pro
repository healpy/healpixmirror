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
pro alm_t2i, alm_tab, index, alm_list, MFIRST=mfirst
;+
; NAME:
;     alm_t2i
;
; PURPOSE:
;  turn an alm COMPLEX or REAL array of the form 
;    a[l,m] or a[l,m,0:1]           (default)
;     or a[m,l] or [m,l,0:1] (if MFIRST is set)
;
;  into an index list (with index = l*(l+1)+m+1 and abs(m) <= l)
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
; CALLING SEQUENCE:
;     alm_t2i, alm_tab, index, alm_list
; 
; INPUTS:
;     alm_tab: real or complex array, containing all the alm for l
;         in [0,lmax] and m in [0,mmax].
;       if REAL it has 3 dimensions
;       if COMPLEX is has 2 dimensions
;
; KEYWORD PARAMETERS
;    MFIRST: if set the array in a(m,l) instead of a(l,m)
;        where abs(m) <= l
;
; OUTPUTS:
;     index: Integer vector of size nl containing the index the 
;            of alm coefficients, related to {l,m} by the relation
;             i = l^2 + l + m + 1
;
;     alm_list: array of alm coefficients, with dimension (nl, 2)
;            -- corresponding to
;                  nl   = number of {l,m} indices
;                  2 for real and imaginary parts of alm coefficients
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
;     2007-10-04: creation
;     2011-10-21: addition of MFIRST
;
;-

syntax = 'ALM_T2I, alm_tab, index, alm_list, MFIRST= '
if (n_params() eq 0) then begin
    print,syntax
    return
endif
if (n_params() ne 3) then begin
    print,syntax
    message,'Abort'
endif

; generates l and m arrays
sz = size(alm_tab)
if (sz[0] lt 1) then begin
    print,syntax
    print,'alm_tab must have at least 1 dimension'
    message,'Abort.'
endif
if (keyword_set(mfirst)) then begin ; m first index
    nm = sz[1]
    nl = (sz[0] gt 1) ? sz[2] : 1L
    mtab = lindgen(nm) # replicate(1,nl)
    ltab = replicate(1,nm) # lindgen(nl)
endif else begin
    nl = sz[1]
    nm = (sz[0] gt 1) ? sz[2] : 1L
    ltab = lindgen(nl) # replicate(1,nm)
    mtab = replicate(1,nl) # lindgen(nm)
endelse

if (nm gt nl) then begin
    print,syntax
    print,'It seems that Max m ('+strtrim(nm-1,2)+') is larger than Max l ('+strtrim(nl-1,2)+'), '
    print,'while we expect abs(m) <= l'
    message, 'Abort.'
endif

; find points where m <= l
k = where(mtab le ltab,nk)

; define index = l*(l+1)+m+1 for those points
index = ((ltab+1L)*ltab + mtab)[k] + 1L

sample = alm_tab[0,0]*0 ; either (0.,0.) or (0.d0, 0.d0) or (0.) or (0.d0)
if (size(/tname, sample) eq 'COMPLEX' || size(/tname, sample) eq 'DCOMPLEX') then begin
; fill the alm list with the real and imaginary parts
    alm_list = replicate(real_part(sample),nk,2)
    alm_list[0,0] = real_part(alm_tab[k])
    alm_list[0,1] = imaginary(alm_tab[k])
endif else begin
; fill the alm list with the real and imaginary parts
    alm_list = replicate(sample,nk,2)
    alm_list[0,0] = alm_tab[k]         ; alm_tab[*,*,0]
    alm_list[0,1] = alm_tab[k + nm*nl] ; alm_tab[*,*,1]
endelse



return
end

