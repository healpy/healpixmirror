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
PRO pix2ang_nest, nside, ipix, theta, phi
;*****************************************************************************
;+
; PIX2ANG_NEST, nside, ipix, theta, phi
;
;       renders Theta and Phi coordinates of the nominal pixel center
;       given the NESTED scheme pixel number Ipix and map resolution parameter Nside
;
; INPUT
;    Nside     : determines the resolution (Npix = 12* Nside^2),
;       should be a power of 2
;	SCALAR
;    Ipix  : pixel number in the NESTED scheme of Healpix pixelisation in [0,Npix-1]
;	can be an ARRAY 
;
; OUTPUT
;    Theta : angle (along meridian = co-latitude), in [0,Pi], theta=0 : north pole,
;	is an ARRAY of same size as Ipix
;    Phi   : angle (along parallel = azimut), in [0,2*Pi]
;	is an ARRAY of same size as Ipix
;
; SUBROUTINE
;    nside2npix
;
; HISTORY
;     Feb 1999,           Eric Hivon,               Caltech
;    Sept 2000,          EH
;           free memory by discarding unused variables
;    June 2003,  EH, replaced STOPs by MESSAGEs
;    Oct 2006, EH, IAP, enabled nside > 8192
;    Aug 2008, EH, IAP: issues warning if ipix is not of integer type
;
;-
;*****************************************************************************
  routine = 'PIX2ANG_NEST'
  if N_params() lt 3 then begin
      message,' syntax: '+routine+', nside, ipix, theta, phi'
  endif

  if (N_ELEMENTS(nside) GT 1) then message,'Nside should be a scalar in '+routine
  npix = nside2npix(nside, error = error)
  if (error ne 0) then message,'Invalid Nside '+string(nside)

  assert_pixindex_type, ipix[0], /warning ; warning if ipix is not integer
  min_pix = MIN(ipix, MAX = max_pix)
  IF (min_pix LT 0) THEN BEGIN
      PRINT,'pixel index : ',min_pix,FORMAT='(A,I18)'
      PRINT,'is out of range : ',0,npix-1,FORMAT='(A,I2,I18)'
      message,'Abort'
  ENDIF
  IF (max_pix GT npix-1) THEN BEGIN
      PRINT,'pixel index : ',max_pix,FORMAT='(A,I18)'
      PRINT,'is out of range : ',0,npix-1,FORMAT='(A,I2,I18)'
      message,'Abort'
  ENDIF

;   coordinate of the lowest corner of each face
  jrll = [2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4] ; in unit of nside
  jpll = [1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7] ; in unit of nside/2


  nl1 = LONG(nside)
  nl3 = 3L*nl1
  if (nl1 gt 8192) then begin
      nl4 = 4LL*nl1
      npface = nl1 * long64(nl1)
      l64 = 1
  endif else begin
      nl4 = 4L*nl1
      npface = nl1 * nl1
      l64 = 0
  endelse
  np = N_ELEMENTS(ipix)
  fn = DOUBLE(nl1)
  fact1 = 1.d0/(3.d0*fn*fn)
  fact2 = 2.d0/(3.d0*fn)
  piover2 = !DPI / 2.d0

;     initiates the array for the pixel number -> (x,y) mapping
  common pix2xy, pix2x, pix2y
  if (n_elements(pix2x) eq 0) then init_pix2xy ; initiate pix2x and pix2y

;     finds the face, and the number in the face
  ipf = ROUND(ipix, L64=l64)  ; make sure pixel number is integer
  face_num = BYTE(ipf/npface)        ; face number in {0,11}
  ipf MOD= npface         ; pixel number in the face {0,npface-1}

;     finds the x,y on the face (starting from the lowest corner)
;     from the pixel number
  scale = 1L
  ix = 0L & iy = 0L
  smax = (nl1 le 8192) ? 2 : 5
  for i=0, smax-1 do begin
      ip_low = ipf and 1023 ; last 10 bits
      ix += scale * pix2x[ip_low]
      iy += scale * pix2y[ip_low]
      scale *= 32L
      ipf /= 1024               ; truncate out last 10 bits
  endfor
  ix += scale * pix2x[ipf]
  iy += scale * pix2y[ipf]
  ip_low = 0 & ipf = 0 ; free memory

;     transforms this in (horizontal, vertical) coordinates
  jrt = ix + iy                 ; 'vertical' in {0,2*(nside-1)}
  jpt = ix - iy                 ; 'horizontal' in {-nside+1,nside-1}
  ix = 0 & iy = 0               ; free memory

;     computes the z coordinate on the sphere
  jr =  long(jrll(face_num)*nl1 - jrt - 1) ; ring number in {1,4*nside-1}
  jrt = 0                       ; free memory

  nr     = LONARR(np, /NOZERO)
  kshift = BYTARR(np, /NOZERO)
  theta  = DBLARR(np, /NOZERO)

  pix_eqt = WHERE( jr GE nl1 AND jr Le nl3, n_eqt) ; equatorial region
  IF (n_eqt GT 0) THEN BEGIN
      nr(pix_eqt)     = nl1   ; equatorial region 
      theta(pix_eqt)  = ACOS(  (2*nl1-jr(pix_eqt))*fact2  )
      kshift(pix_eqt) = (jr(pix_eqt) - nl1) MOD 2
      pix_eqt = 0  ; free memory
  ENDIF

  pix_npl = WHERE( jr LT nl1, n_npl) ; north pole
  IF (n_npl GT 0) THEN BEGIN
      nr(pix_npl)     = jr(pix_npl)
      theta(pix_npl)  = ACOS(  1.d0 - double(nr(pix_npl))^2 * fact1  )
      kshift(pix_npl) = 0
      pix_npl = 0  ; free memory
  ENDIF

  pix_spl = WHERE( jr GT nl3,   n_spl) ; south pole
  if (n_npl + n_spl + n_eqt) NE np THEN message,'error in '+routine
  IF (n_spl GT 0) THEN BEGIN
      nr(pix_spl)     = nl4 - jr(pix_spl)
      theta(pix_spl)  = ACOS(  -1.d0 + double(nr(pix_spl))^2 * fact1  )
      kshift(pix_spl) = 0
      pix_spl = 0 ; free memory
  ENDIF

;     computes the phi coordinate on the sphere, in [0,2Pi]
  jp = (jpll(face_num)*nr + jpt + 1 + kshift)/2 ; 'phi' number in the ring in {1,4*nr}
  jpt = 0  & face_num = 0         ; free memory
  jp = jp - nl4 * (jp GT nl4)
  jp = jp + nl4 * (jp LT 1)

;;  phi   = DBLARR(np, /NOZERO)
  phi = (jp - (kshift+1)*0.5d0) * (piover2 / nr)
  jp = 0 & kshift = 0 & nr = 0
  
  return
end ; pix2ang_nest

