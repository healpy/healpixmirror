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
;===============================================================
function modulo, arg1, arg2

n2 = n_elements(arg2)
out = arg1 mod arg2
neg = where(out lt 0, nneg)
if (nneg gt 0) then out[neg] += arg2[neg < (n2-1)]

return, out
end


;===============================================================
pro pixels_in_phi_range2, nside_in, nside_out, irlist, phi0, dphi, pir, npir

nl3 = 3L*nside_in
nl4 = 4L*nside_in
iring  = (nside_in le 8192) ? lindgen(nl4+1) : l64indgen(nl4+1)
npr = iring < (nl4 - iring)
npr <= nside_in
npr *= 4
kshift = ~(iring and 1)
if (nside_in eq 1) then kshift = ~kshift
kshift[0:nside_in] = 1
kshift[nl3:nl4] = 1


count = histogram(dphi/(!dpi-1.0d-8)>(-1)<1, rev=rev, min=-1, max=1)
n_empty = count[0] & n_normal = count[1] & n_full = count[2] & n_non_empty = n_normal+n_full
if (n_empty gt 0)  then empty  = rev[rev[0]:rev[1]-1]
if (n_normal gt 0) then normal = rev[rev[1]:rev[2]-1]
if (n_full gt 0)   then full   = rev[rev[2]:rev[3]-1]
if (n_non_empty gt 0) then non_empty = rev[rev[1]:rev[3]-1]

nrings = n_elements(irlist)
ip_low = lonarr(nrings) ; pixel range on each ring
ip_hi = ip_low

if (n_empty gt 0) then begin
    ip_low[empty] = -9999
    ip_hi[empty] = ip_low[empty]
endif

twopi = 2.d0*!DPI
if (n_full gt 0) then begin
    nr = npr[irlist[full]]
    ip_low[full] = 0
    ip_hi[full] = nr-1
endif

if (n_normal gt 0) then begin
    ir = irlist[normal]
    nr = npr[ir]
    shift = (kshift[ir]) * .5d0
    phi_low = phi0[normal] - dphi[normal] ; in [-Pi,2Pi]
    phi_hi  = phi0[normal] + dphi[normal] ; in [0,3Pi]
    iphi_low = CEIL  (nr * phi_low / twopi - shift)
    iphi_hi  = FLOOR (nr * phi_hi  / twopi - shift)
    ;iphi_low <= iphi_hi
    ip_low[normal] = modulo (iphi_low, nr)     ; in {0,nr-1}
    ip_hi [normal] = modulo (iphi_hi,  nr)
    ;print, phi_low, phi_low*nr / twopi
endif 

if (n_non_empty eq 0) then begin
    pir[0] = -1
    npir = 0
    return
endif

; degrade indices
degrade_ring, nside_in, irlist[non_empty], ip_low[non_empty], nside_out, jr_out1, jp_low
degrade_ring, nside_in, irlist[non_empty], ip_hi [non_empty], nside_out, jr_out2, jp_hi

;print,irlist, (irlist-1)/(nside_in/nside_out)+1, ip_low, ip_hi

;;print,minmax(jr_out1-jr_out2)
; work at low (final) resolution
nl4 = 4L*nside_out
iring  = (nside_out le 8192) ? lindgen(nl4+1) : l64indgen(nl4+1)
npr = iring < (nl4 - iring)
npr <= nside_out
npr *= 4
npc = total(npr, /cumul, /integer)

npir = 0
ramp = lindgen(nl4)

jr_min = min([jr_out1,jr_out2], max=jr_max)
nj = jr_max - jr_min + 1L
h1 = histogram(jr_out1, rev=rev1, min=jr_min, max=jr_max)
h2 = histogram(jr_out2, rev=rev2, min=jr_min, max=jr_max)
for j=0L, nj-1 do begin
    if (h1[j] gt 0 || h2[j] gt 0) then begin
        ir = jr_min + j         ; current ring, in [1, nl4-1]
        nr = npr[ir] ; number of pixels available on this ring

        if (h1[j] gt 0) then begin
            xx = jp_low [rev1[rev1[j]:rev1[j+1]-1]]
            my_low = min(xx,max=junk)
            if (junk-my_low ge (3*nr/4)) then my_low = min(xx - nr * (xx gt nr/2))
        endif
        if (h2[j] gt 0) then begin
            xx = jp_hi  [rev2[rev2[j]:rev2[j+1]-1]]
            my_hi  = max(xx,min=junk)
            if (my_hi-junk ge (3*nr/4))  then my_hi  = max(xx - nr * (xx gt nr/2))
        endif
        if (h1[j] eq 0) then my_low = my_hi
        if (h2[j] eq 0) then my_hi  = my_low

        np = my_hi - my_low ; in [-nr+1, nr-1]
        np = (np ge 0) ? np + 1 : nr + np + 1 ; deal with periodic BC
        np <= nr
        ip = modulo(my_low + ramp[0:np-1], nr)
        ; print, ir, jr_min, jr_max, nr, my_low, my_hi
        pir[npir] = npc[ir-1] + ip ; fill final list
;         np = 2
;         pir[npir] = npc[ir-1] + [my_low, my_hi]
        npir += np
    endif
endfor

if npir eq 0 then pir[0] = -1


return
end


;===============================================================

pro query_disc, nside, vector0, radius_in, listpix, nlist, deg = deg, inclusive=inclusive, help=help, nested=nested, walltime=walltime
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
;     QUERY_DISC, Nside, Vector0, Radius_In, Listpix, Nlist, [Deg=, Nest=, Help=,
;     Inclusive=, Walltime=]
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
;
;     /Help: if set, prints this documentation header and exits
;
;     /INCLUSIVE :0 by default, only the pixels whose center 
;                       lie in the triangle are listed on output
;                  if set to 1, all pixels overlapping the triangle are output
;
;     /NESTED  :0 by default, the output list is in RING scheme
;                  if set to 1, the output list is in NESTED scheme
;
; OUTPUTS:
;     Listpix : list of pixels found within a radius Radius_in from
;          Vector0,
;     Nlist = number of elements of Listpix
;       if no pixel is found (too small a circle) Nlist is 0 and
;       listpix is -1 .
;
; OPTIONAL OUTPUTS:
;     WALLTIME=  contains on output the wall clock time used by the routine [s]   
;
; COMMON BLOCKS:
;     none.
;
; RESTRICTIONS:
;     On output Listpix is (supposed to be) ranked in pixel index
;     rather than ranked in distance to the disc center.
;
; SIDE EFFECTS:
;     calls : ring_num, discphirange_at_z, ring2nest
;          and   modulo, pixels_in_phi_range2 (included in this file)
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
;     2011-04-* : improved performance (less false positive) in /INCLUSIVE mode
;          by working at larger Nside.
;-

tstart = systime(1)
code = 'query_disc'

if keyword_set(help) then begin
    doc_library,code
    return
endif

if (n_params() lt 4 || n_params() gt 5) then begin
    print,'SYNTAX = '+code+', Nside, Vector0, Radius_In, Listpix, [Nlist, DEG=, HELP=, INCLUSIVE=, NESTED=, WALLTIME=]'
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

if (keyword_set(deg) && radius_in gt 180.) then begin
    print,prompt+'radius (deg) too big :',radius_in
    return
endif

if (~keyword_set(deg) && radius_in gt !pi) then begin
    print,prompt+'radius (radian) too big :',radius_in
    return
endif

do_inclusive = keyword_set(inclusive)

npix = nside2npix(nside,err=errpix)
if (errpix) then begin
    print,prompt+'invalid Nside:',nside
    return
endif

defsysv,'!healpix',exists=yes
if (~yes) then init_healpix

;radius is in radian
radius = keyword_set(deg) ? radius_in*!DPI/180.d0 : radius_in

halfpi = !DPI*.5d0
lnside = long(nside)

radius_eff = radius
wnside = lnside              ; Nside used internally
if (do_inclusive) then begin
    wnside = lnside * 8 ; larger internal Nside, used to remove false positives
    wnside <= max(!healpix.nside)
    wnside1 = wnside/2 > lnside ; used to compute effective radius
    fudge = 1.362d0 * !DPI / (4.0d0 * wnside1) ; increase effective radius
    radius_eff = (radius + fudge)  < !DPI
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
z0 = vector0[2]/norm_vect0

; find upper and lower z
rlat0 = asin(z0) ; lat in RAD
rlat1 = rlat0 + radius_eff
rlat2 = rlat0 - radius_eff
zmax = (rlat1 ge halfpi)  ? 1.d0  : sin(rlat1)
zmin = (rlat2 le -halfpi) ? -1.d0 : sin(rlat2)

; fills list of rings and dphi
irmin = ring_num(wnside, zmax, shift=+1)
irmax = ring_num(wnside, zmin, shift=-1)
nz = irmax-irmin+1

izlist = irmin + lindgen(nz)    ; list of rings
zlist  = ring2z(wnside, izlist) ; list of z
dphilist = discphirange_at_z (vector0, radius_eff, zlist, phi0=phi0) ; phi range in each ring
phi0list = replicate(phi0, nz)

; build list of pixels
nlist = 0LL
pixels_in_phi_range2, wnside, lnside, izlist, phi0list, dphilist, work, nlist


if (nlist gt 0) then begin
    if keyword_set(nested) then ring2nest, lnside, work[0:nlist-1], listpix else listpix=work[0:nlist-1]
endif else begin
    listpix = -1L
endelse

walltime = systime(1) - tstart
return
end

