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
pro query_polygon, nside, vlist, listpix, nlist, help=help, nested=nested, inclusive=inclusive

;+
;=======================================================================
; query_polygon, nside, vlist, listpix, [nlist, HELP=, NESTED=, INCLUSIVE=]
;
; finds pixels that lie within a CONVEX polygon defined by its vertex on the sphere
;
; nside             : IN
; vlist(0:n-1, 0:2) : IN, list of vertices
; listpix           : OUT
; nlist             : OUT
; nested              : IN, OPTIONAL
; inclusive         : IN, OPTIONAL
; help              : IN, OPTIONAL  prints this documentation header and exits
; 
; algorithm:
;   the polygon is divided into triangles
;   vertex 0 belongs to all the triangles
;
; v1.0, EH, Caltech, Dec-2001
; 2008-03-27, check # of params, added /HELP
; 2010-03-12: corrected typo in documentation header
;=======================================================================
;-

routine = 'query_polygon'
syntax = 'QUERY_POLYGON, Nside, Vlist, Listpix, [Nlist, HELP=, NESTED=, INCLUSIVE=]'

if keyword_set(help) then begin
    doc_library,routine
    return
endif

if (n_params() lt 3 or n_params() gt 4) then begin
    print,syntax
    return
endif

npix = nside2npix(nside, error=error)
if (error ne 0) then begin
    message,'invalid Nside '+string(nside)
endif

sz = size(vlist)
rank = sz[0]
n_remain = sz[1]
ndim = sz[2]

if (rank ne 2 or ndim ne 3) then begin
    message,/info,syntax
    message,' vlist (n,3) should be a list of 3D vectors'
endif

if (n_remain lt 3) then begin
    message,/info,syntax
    message,' the polygon should have at least 3 vertices'
endif

vvlist = double(vlist)

if (n_remain eq 3) then goto, convex

ss = replicate(1,n_remain)
; make circular list : n-1, 0, 1, ..., n-1, 0
index = [n_remain-1, lindgen(n_remain), 0]
for i=1, n_remain do begin
    vo   = vect_prod(vvlist[index[i-1],*], vvlist[index[i+1],*])
    hand = total    (vo                  * vvlist[index[i],  *])
    if (hand lt 0.) then ss[i-1] = -1
endfor

kp = where( ss gt 0, nkp) ; vertices with positive handedness
km = where( ss lt 0, nkm) ; vertices with negative handedness

nlow = nkp < nkm
case nlow of
    0: goto, convex ; all vertices have same handn.
    1: begin  ; one is different, find it and put it first
        if (nkp eq 1) then ix = kp[0] else ix = km[0]
        vvlist = shift(vvlist,-ix,0)
    end
    else: begin
        message,/info,' The polygon is not convex and has more that one concave vertex'
        message,/info,' ***** The result is unpredictable *****'
    end
endcase

convex:

; process one triangle at a time
nlist = 0
while (n_remain ge 3) do begin
    query_triangle,nside, $
                   vvlist[0,*], vvlist[n_remain-2,*], vvlist[n_remain-1,*], $
                   list_tr, ntl, nested=nested, inclusive=inclusive

    if ntl gt 0 then begin
        if nlist le 0 then begin
            listpix = list_tr
            nlist = ntl
        endif else begin
            listpix = [listpix,list_tr]
            nlist = nlist + ntl
        endelse
    endif

    n_remain = n_remain - 1
endwhile

; sort final list
listpix = listpix[sort(listpix)]

; remove redondant pixels
listpix = listpix[uniq(listpix)]

nlist = n_elements(listpix)

return
end
