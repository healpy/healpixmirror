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
pro intrs_intrv, d1, d2, di, ni
;=======================================================================
; computes the intersection di 
; of 2 intervals d1 (= [a1,b1]) and d2 (= [a2,b2])
; on the periodic domain ( = [A,B], where A and B are arbitrary)
; ni is the resulting number of intervals (0,1, or 2)
;
; if a1<b1 then d1 = {x | a1 <= x <= b1}
; if a1>b1 then d1 = {x | a1 <= x <= B  U  A <= x <= b1}
;=======================================================================

tr12 = (d1[0] lt d1[1])
tr21 = 1 - tr12
tr34 = (d2[0] lt d2[1])
tr43 = 1 - tr34
tr13 = (d1[0] lt d2[0])
tr31 = 1 - tr13
tr24 = (d1[1] lt d2[1])
tr42 = 1 - tr24
tr14 = (d1[0] lt d2[1])
tr32 = (d2[0] lt d1[1])

ik = -1
dk = replicate(-1.0d9,4)


if ((tr34 and tr31 and tr14)  or  (tr43 and (tr31 or tr14))) then begin
   ik = ik + 1
   dk[ik] = d1[0]  ; a1
endif
if ((tr12 and tr13 and tr32)  or  (tr21 and (tr13 or tr32))) then begin
   ik = ik + 1
   dk[ik] = d2[0]  ; a2
endif
if ((tr34 and tr32 and tr24)  or  (tr43 and (tr32 or tr24))) then begin
   ik = ik + 1
   dk[ik] = d1[1]  ; b1
endif
if ((tr12 and tr14 and tr42)  or  (tr21 and (tr14 or tr42))) then begin
   ik = ik + 1
   dk[ik] =  d2[1]  ; b2
endif

di = replicate(0.0d0,4)
nk = ik+1
case nk of
    0 : begin
        ni = 0
    end
    2 : begin
        ni = 1
        di[0:1] = [ dk[0], dk[1] ] ; [a1,b1] or [a1,b2] or [a2,b1] or [a2,b2]
    end
    4 : begin
        ni = 2
        di[0:3] = [ dk[0], dk[3], dk[1], dk[2] ] ; [a1,b2] U [a2,b1]
    end
    else : begin
        print, ik
        print,dk
        print,d1,d2
        message,"error in intrs_intrv"
    end
endcase

return
end

;===========================================================================

pro query_triangle, nside, v1, v2, v3, listpix, nlist, help=help, nested=nested, inclusive=inclusive
;+
;=======================================================================
;
;    query_triangle, Nside, V1, V2, V3, Listpix, Nlist, HELP=, NESTED=, INCLUSIVE=
;    --------------
;     nside       = resolution parameter (a power of 2)
;     v1, v2, v3  = 3D vector location of the 3 triangle vertices
;     list_pix    = list of pixel lying in the triangle
;     nlist       = number of pixels in the list
;     nested  (OPT), :0 by default, the output list is in RING scheme
;                  if set to 1, the output list is in NESTED scheme
;     inclusive (OPT) , :0 by default, only the pixels whose center 
;                       lie in the triangle are listed on output
;                  if set to 1, all pixels overlapping the triangle are output
;     help (OPT):   prints this documentation header and exits
;
;
;
; v1.0, EH, Caltech, Aug-Sep-2002 : adapted from F90 code
;=======================================================================
;-

routine = 'query_triangle'
syntax = routine+', Nside, V1, V2, V3, Listpix [, Nlist, HELP=, NESTED=, INCLUSIVE=]'

if keyword_set(help) then begin
    doc_library,routine
    return
endif

if (n_params() lt 5 or n_params() gt 6) then begin
    print,syntax
    return
endif


npix = nside2npix(nside)
if (npix lt 0) then begin 
    message,"Invalid Nside = "+string(nside)
endif
lnside = long(nside)

do_inclusive = keyword_set(inclusive)

; normalize vectors
vv = dblarr(3,3)
vv[*,0] = v1 / sqrt(total(v1*v1, /double))
vv[*,1] = v2 / sqrt(total(v2*v2, /double))
vv[*,2] = v3 / sqrt(total(v3*v3, /double))

;;listir=lonarr(4*nside)

dth1 = 1.0d0 / (3.0d0*double(nside)^2)
dth2 = 2.0d0 / (3.0d0*double(nside))


; determ = (vect1 X vect2) . vect3
; determines the left(<0)/right(>0) handedness of the triangle
determ = vv[0,0]*vv[1,1]*vv[2,2] + vv[0,1]*vv[1,2]*vv[2,0] + vv[0,2]*vv[1,0]*vv[2,1] $
       - vv[2,0]*vv[1,1]*vv[0,2] - vv[2,1]*vv[1,2]*vv[0,0] - vv[2,2]*vv[1,0]*vv[0,1]

if (abs(determ) lt 1.d-20) then begin
    message,/info,' ************************************************************'
    message,/info,' The triangle is degenerated (2 of the vertices are antipodal)'
    message,/info,' The query can not be performed '
    message,      ' ************************************************************'
endif
sdet = 1.d0
if (determ lt 0.d0) then sdet = -1.d0 ; = +1 or -1, the sign of determ

; scalar product of vertices vectors
sprod = dblarr(3)
sprod[0] = total(vv[*,1]*vv[*,2])
sprod[1] = total(vv[*,2]*vv[*,0])
sprod[2] = total(vv[*,0]*vv[*,1])

; vector orthogonal to the great circle containing the vertex doublet
vo = dblarr(3,3)
vo[*,0] = vect_prod( vv[*,1], vv[*,2]) 
vo[*,1] = vect_prod( vv[*,2], vv[*,0]) 
vo[*,2] = vect_prod( vv[*,0], vv[*,1]) 

; normalize the orthogonal vector
vo[*,0] = vo[*,0] /  SQRT(total(vo[*,0]^2))
vo[*,1] = vo[*,1] /  SQRT(total(vo[*,1]^2))
vo[*,2] = vo[*,2] /  SQRT(total(vo[*,2]^2))

; test presence of poles in the triangle
zmax = -1.0d0
zmin =  1.0d0
testp = (vo[2,*] * sdet ge 0.0d0) ; north pole in each hemisphere
if (total(testp) eq 3) then begin 
   zmax = 1.0d0 ; north pole in the triangle
endif
if (total(testp) eq 0) then begin
   zmin = -1.0d0 ; south pole in the triangle
endif

; look for northernest and southernest points in the triangle
;   node(1,2) = vector of norm=1, in the plane defined by (1,2) and with z=0
test1a = ((vv[2,2] - sprod[0] * vv[2,1]) ge 0.0d0) ; segment 2-3 : -vector(3) . node(2,3)
test1b = ((vv[2,1] - sprod[0] * vv[2,2]) ge 0.0d0) ;                vector(2) . node(2,3)
test2a = ((vv[2,2] - sprod[1] * vv[2,0]) ge 0.0d0) ; segment 1-3 : -vector(3) . node(1,3)
test2b = ((vv[2,0] - sprod[1] * vv[2,2]) ge 0.0d0) ;                vector(1) . node(1,3)
test3a = ((vv[2,1] - sprod[2] * vv[2,0]) ge 0.0d0) ; segment 1-2 : -vector(2) . node(1,2)
test3b = ((vv[2,0] - sprod[2] * vv[2,1]) ge 0.0d0) ;                vector(1) . node(1,2)

; sin of theta for orthogonal vector
sto = SQRT( (1.0d0-vo[2,*])*(1.0d0+vo[2,*]) )

; for each segment (=side of the triangle) the extrema are either 
; - the 2 vertices 
; - one of the vertices and a point within the segment

; segment 2-3
z1max = vv[2,1]
z1min = vv[2,2]
if ( test1a EQ test1b ) then begin
   zz = sto[0]
   if ((vv[2,1]+vv[2,2]) ge 0.0d0) then z1max =  zz else z1min = -zz
endif

; segment 1-3
z2max = vv[2,2]
z2min = vv[2,0]
if ( test2a EQ test2b ) then begin
   zz = sto[1]
   if ((vv[2,0]+vv[2,2]) ge 0.0d0) then z2max =  zz else z2min = -zz
endif

; segment 1-2
z3max = vv[2,0]
z3min = vv[2,1]
if ( test3a EQ test3b ) then begin
   zz = sto[2]
   if ((vv[2,0]+vv[2,1]) ge 0.0d0) then z3max =  zz else z3min = -zz
endif

zmax = MAX([z1max, z2max, z3max, zmax])
zmin = MIN([z1min, z2min, z3min, zmin])

; if we are inclusive, move the upper point up, and the lower point down, by a half pixel size
offset = 0.0d0
sin_off = 0.0d0
if (do_inclusive) then begin
   offset = !DPI / (4.0d0*nside) ; half pixel size
   sin_off = sin(offset)
   zmax = cos( acos(zmax) - offset) < 1.d0
   zmin = cos( acos(zmin) + offset) > (-1.d0)
endif

; northernest and sourthernest ring number
irmin = ring_num(lnside, zmax)
irmax = ring_num(lnside, zmin)

nlist = 0
;    print*,"irmin, irmax ",irmin,irmax

; -------- loop on the rings -------------------------

tgthi = -1.0d30 * vo[2,*]
phi0i =  replicate(0.0d0, 3)
kk = where(sto gt 1.0d-10, nkk)
if (nkk gt 0) then begin
    tgthi[kk] = -vo[2,kk] / sto[kk] ; -cotan(theta_orth)
    phi0i[kk] = ATAN(vo[1,kk],vo[0,kk])
endif

; the triangle boundaries are geodesics : intersection of the sphere with plans going thru (0,0,0)
; if we are inclusive, the boundaries are the intersecion of the sphere with plans pushed outward
; by sin(offset)
twopi = 2.0d0 * !DPI

dom    = dblarr(2,3)
dom12  = dblarr(4) & dom123a = dom12 & dom123b = dom12
alldom = dblarr(6)

for iz = irmin, irmax do begin
    z = ring2z(lnside, iz)
    ; computes the 3 intervals described by the 3 great circles
    st = SQRT(1.0d0 - z*z)
    tgth = z / st               ; cotan(theta_ring)
;     dc  = tgthi * tgth - sdet * sin_off / (sto * st)
    dc  = tgthi * tgth - sdet * sin_off / ((sto+1.d-30) * st) ; !!! sto is slightly offset to avoid division by 0

    for j=0,2 do begin
        if (dc[j]*sdet le -1.0d0) then begin ; the whole iso-latitude ring is on the right side of the great circle
            dom[0:1, j] = [ 0.0d0, twopi ]
        endif else if (dc[j]*sdet ge 1.0d0) then begin ; all on the wrong side
            dom[0:1, j] = [ -1.000001d0, -1.0d0 ] * (j+1) 
        endif else begin        ; some is good, some is bad
            dom[0:1, j] = (phi0i[j] + ACOS(dc[j]) * sdet * [-1.0d0, 1.0d0] + twopi) MOD twopi
        endelse
    endfor
    ; identify the intersections (0,1,2 or 3) of the 3 intervals
    intrs_intrv, dom[0:1,0], dom[0:1,1], dom12, n12

    if (n12 eq 0) then goto, empty
    if (n12 eq 1) then begin
        intrs_intrv, dom[0:1,2], dom12, dom123a, n123a
        if (n123a eq 0) then goto, empty
        alldom[0:2*n123a-1] = dom123a[0:2*n123a-1]
        ndom = n123a            ; 1 or 2
    endif
    if (n12 eq 2) then begin
        intrs_intrv, dom[0:1,2], dom12[0:1], dom123a, n123a
        intrs_intrv, dom[0:1,2], dom12[2:3], dom123b, n123b
        ndom = n123a + n123b    ; 0, 1, 2 or 3
        if (ndom eq 0)  then goto, empty
        if (n123a ne 0) then alldom[0:     2*n123a-1]  = dom123a[0:2*n123a-1]
        if (n123b ne 0) then alldom[2*n123a:2*ndom-1]  = dom123b[0:2*n123b-1]
        if (ndom gt 3)  then begin
            print,code+"> too many intervals found"
        endif
    endif
    for idom=0,ndom-1 do begin
        a_i = alldom[2*idom]
        b_i = alldom[2*idom+1]
        phi0     = (a_i + b_i) * 0.5d0
        dphiring = (b_i - a_i) * 0.5d0
        if (dphiring lt 0.0d0) then begin
            phi0 = phi0 + !dpi
            dphiring = dphiring + !dpi
        endif

        ;        ------- finds pixels in the triangle on that ring ---------
        listir=in_ring( lnside, iz, phi0, dphiring, nir, nested=nested)

        ;        ----------- merge pixel lists -----------
        if nir gt 0 then begin
            if nlist le 0 then begin
                listpix = listir 
                nlist = nir
            endif else begin
                listpix = [listpix,listir]
                nlist = nlist + nir
            endelse
        endif
    endfor
empty:
endfor ;-----------------------------------------

return
end

