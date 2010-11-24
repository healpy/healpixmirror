; test_chealpix.pro

pro test_chealpix, nside
   if undefined(nside) then nside = 8192L
   npix = 12L*nside*nside
   dpix = npix/10000L
   
   get_lun, lun
   openw, lun, './test_chealpix.dat'

   for ipix = 0L, npix-1L, dpix do begin 
      pix2ang_nest, nside, ipix, theta, phi
      pix2ang_ring, nside, ipix, theta2, phi2
      ring2nest, nside, ipix, jpix
      nest2ring, nside, ipix, jpix2
      printf, lun, ipix, theta, phi, theta2, phi2, jpix, jpix2, $
        format = '(i9, x, f12.8, x, f12.8, x, f12.8, x, f12.8, x, i9, x, i9)'
   endfor 

   for theta = 0.0d0, !dpi, 0.1d0 do begin 
      for phi = 0.0d0, 2.0d0*!dpi, 0.1d0 do begin 
         ang2pix_nest, nside, theta, phi, ipix
         ang2pix_ring, nside, theta, phi, jpix
         printf, lun, theta, phi, ipix, jpix, $
           format = '(f12.8, x, f12.8, x, i9, x, i9)'
      endfor 
   endfor 

   close, lun
   free_lun, lun
end 

