

outdir = './'

; if undefined(cmb)  then read_fits_map,'/data/Planck/PLA/COM_CompMap_CMB-smica_2048_R1.20.fits',cmb
; planck_colors,1
; mollview,cmb,1,colt=256,/nest,/silent

if undefined(m217) then read_fits_map,'/data/Planck/PLA/HFI_SkyMap_217_2048_R1.10_nominal.fits',m217
asinh=2
planck_colors,2
;mollview,m217,1,colt=256,asinh=asinh,/nest,fact=6.e3,/silent,png=outdir+'planck_colors_217.png',/preview,title='Planck 217GHz'
mollview,m217,1,colt=256,asinh=asinh,/nest,fact=1.e6,/silent,png=outdir+'planck_colors_217.png',/preview,title='Planck @ 217GHz',min=-1.e3,max=1.e7, offset=-133/1.e6,/nobar, pxsize=1000, charsize=2

for i=1,2,1 do begin
    hardfile = outdir+'planck_colors'+strtrim(i,2)
    hard_copy, hardfile, png=png
    planck_colors,i,/show
    hard_copy, /close
endfor


end

