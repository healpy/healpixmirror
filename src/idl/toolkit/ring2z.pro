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
function ring2z, nside, iz
;+
; for a given Nside, returns the z=cos(theta) of a ring index (in [1,4*nside-1])
;
;-

dth1 = 1.0d0 / (3.0d0*double(nside)^2)
dth2 = 2.0d0 / (3.0d0*double(nside))

if (iz le nside-1) then begin   ; north polar cap
    zring = 1.0d0  - double(iz)^2 * dth1
endif else if (iz le 3L*nside) then begin ; tropical band + equat.
    zring = double(2L*nside-iz) * dth2
endif else begin
    zring = - 1.0d0 + double(4L*nside-iz)^2 * dth1
endelse



return, zring
end
