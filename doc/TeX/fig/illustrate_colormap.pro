;pro illustrate_colormap


filek = '/data/WMAP/5yr/maps/wmap_band_iqumap_r9_5yr_K_v3.fits'
mollview,filek,       title='Linear Color Scale'                 ,/silent, ps='fig/wmap_Kband_linear.ps'
mollview,filek,/asinh,title='Sinh!u-1!n Color Scale'            ,/silent, ps='fig/wmap_Kband_asinh.ps'
mollview,filek,/hist, title='Histogram Equalized Color Scale'  ,/silent, ps='fig/wmap_Kband_hist.ps'
mollview,filek,/log,  title='Log Scale'                           ,/silent, ps='fig/wmap_Kband_log.ps'


;return
end
