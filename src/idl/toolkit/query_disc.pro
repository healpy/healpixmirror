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
;+
; NAME:
;  query_disc
;
; PURPOSE:
;  finds the Healpix pixels that lie within a radius (radius_in) from the
;  vector (vector_0) in the ring scheme for the resolution Nside
;
; CATEGORY:
;  Healpix pixel toolkit
;
; CALLING SEQUENCE:
;     QUERY_DISC, Nside, Vector0, Radius_In, Listpix, Nlist, Deg=, Nest=, Help=,
;     Inclusive=
;
; INPUTS:
;     Nside : scalar integer : Healpix resolution (power of 2)
;     Vector0 : 3-element vector (float or double) : position of the
;          disc center on the sphere (north pole in [0.,0.,1.])
;          the norm of Vector0 does not have to be one, what is
;          consider is the intersection of the sphere with the line of
;          direction Vector0
;     Radius_in : radius of the circle (in radians, unless Deg is set)
;
; KEYWORD PARAMETERS:
;     Deg : if set, the disc radius is in degrees instead of radians.
;     /Help: if set, prints this documentation header and exits
;
; OUTPUTS:
;     Listpix : list of pixels found within a radius Radius_in from
;          Vector0,
;     Nlist = number of elements of Listpix
;       if no pixel is found (too small a circle) Nlist is 0 and
;       listpix is -1 .
;
; COMMON BLOCKS:
;     none.
;
; RESTRICTIONS:
;     On output Listpix is (supposed to be) ranked in pixel index
;     rather than ranked in distance to the disc center.
;
; SIDE EFFECTS:
;     calls : ring_num, in_ring (included in this file).
;
; PROCEDURE:
;     uses the particular layout of the pixels in parallel rings,
;     to find rapidly the selected pixels.
;     Is much faster than a simple search among all the pixels.
;
; EXAMPLE:
;     query_disc, 256L, [.5,.5,0.], 10., listpix, nlist, /Deg
;       outputs in listpix the Healpix pixel numbers of the pixels
;       with 10 deg of the point on the sphere 
;       having the direction [.5,.5,0.]
;
; MODIFICATION HISTORY:
;     1998??     EH, TAC, 1st f90 version
;     1999-??    EH, Caltech, traduction in IDL
;     1999-12-07 : correction of a bug in the build in subroutine in_ring
;     2002-09    : added inclusive keyword, added nest keyword, renamed
;     query_disc
;     2008-03-30: fixed bug appearing when disc centered on either pole
;                 use 'work' array instead of expanding output array piece by piece
;     2009-04-08: actually returns -1 if nlist = 0
;-

function phi_range_at_z, z, x0, y0, z0, cosang, a, cosphi0

b = cosang - z*z0
c = 1.d0 - z*z
if ((x0 eq 0.d0) and (y0 eq 0.d0)) then begin
    dphi=!PI
    if (b gt 0) then dphi = -1000.d0
endif  else begin
    cosdphi = b/sqrt(a*c)
    if (ABS(cosdphi) le 1.d0) then begin
        dphi = ACOS (cosdphi)   ; in [0,Pi]
    endif else begin
        dphi = !DPI             ; all the pixels at this elevation are in the disc
        if (cosphi0 lt cosdphi) then dphi = -1000.d0 ; out of the disc
    endelse
endelse

return, dphi
end

; =====================================================================

pro query_disc, nside, vector0, radius_in, listpix, nlist, deg = deg, inclusive=inclusive, help=help, nested=nested

code = 'query_disc'

if keyword_set(help) then begin
    doc_library,code
    return
endif

if (n_params() lt 4 or n_params() gt 5) then begin
    print,'SYNTAX = '+code+', Nside, Vector0, Radius_In, Listpix, [Nlist, DEG=, HELP=, INCLUSIVE=, NESTED=]'
    return
endif

prompt = strupcase(code)+'> '
if (n_elements(vector0) ne 3) then begin
    print,prompt+'vector0 should be a 3 element vector'
    return
endif

if (radius_in lt 0.) then begin
    print,prompt+'radius should be > 0 : ',radius_in
    return
endif

if keyword_set(deg) and radius_in gt 180. then begin
    print,prompt+'radius (deg) too big :',radius_in
    return
endif

if (not keyword_set(deg)) and radius_in gt !pi then begin
    print,prompt+'radius (radian) too big :',radius_in
    return
endif

do_inclusive = keyword_set(inclusive)

npix = nside2npix(nside,err=errpix)
if (errpix) then begin
    print,prompt+'invalid Nside:',nside
    return
endif

;radius is in radian
if (keyword_set(deg)) then radius = radius_in*!DtoR else radius = radius_in

halfpi = !DPI*.5d0
lnside = long(nside)
fnside = double(nside)

radius_eff = radius
if (do_inclusive) then begin
;  fudge = !DPI / (4.0d0*nside) ; increase radius by half pixel size
    fudge = acos(2.d0/3.d0) / fnside
    radius_eff = radius + fudge
endif
cosang = cos(radius_eff)

; create work array used to store valid pixels
; (faster than expanding final array)
worksize = long64( (1.d0 - cosang)/2.d0 * 1.2 * npix  + 4*lnside + 50)
worksize = worksize < npix

if (lnside gt 8192) then begin
    work = lon64arr(worksize)
endif else begin
    work = lonarr(worksize)
endelse

; circle center
norm_vect0 = sqrt(total(vector0^2))
x0 = vector0[0]/norm_vect0
y0 = vector0[1]/norm_vect0
z0 = vector0[2]/norm_vect0

phi0=0.
if ((x0 ne 0.d0) or (y0 ne 0.d0)) then phi0 = ATAN(y0, x0)  

cosphi0 = cos(phi0)
a = x0*x0 + y0*y0

; find upper and lower rings
rlat0 = asin(z0) ; lat in RAD
rlat1 = rlat0 + radius_eff
rlat2 = rlat0 - radius_eff
if (rlat1 ge halfpi) then zmax = 1.d0 $
                     else zmax = sin(rlat1)

irmin = ( ring_num(lnside,zmax) - 1L ) > 1L ;start from a higher point, to be safe

if (rlat2 le - halfpi) then zmin = -1.d0 $
                       else zmin = sin(rlat2)
irmax = ( ring_num(lnside,zmin) + 1L) < (4*lnside-1) ;go down to a lower point

; ------ loop on ring number ---------
nlist = 0LL

for iz = irmin, irmax do begin

    z = ring2z(lnside, iz) ; z of current ring

    ; phi range in the disc for each z
    dphi = phi_range_at_z(z, x0, y0, z0, cosang, a, cosphi0); phi range in the disc for each z
    ; concatenate lists of pixels
    listir = in_ring(lnside, iz, phi0, dphi, nir, nested=nested)
    if nir gt 0 then begin
        nnew = nlist + n_elements(listir)
        if (nnew gt worksize) then begin
            ; panic mode: resize work array if not large enough
            message,/info,'Expanding work array'
            work = [work, listir]
        endif else begin
            ; normal mode: fill in work array
            work[nlist] = listir
        endelse
        nlist = nnew
    endif
endfor

if (nlist gt 0) then begin
    listpix = work[0:nlist-1]
endif else begin
    listpix = -1L
endelse

return
end

