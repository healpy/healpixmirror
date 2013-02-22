
init_healpix
fits2cl,/wmap7,clwmap
nside = 1024
lmax = 2*nside
pxsize=1000
pysize=400
tmpmap = '/tmp/map.fits'
tag = 'banner_1'
idlimg = './'+tag+'_0.png'
;imimg  = './'+tag+'_1.jpg' & color='#070707'
imimg  = './'+tag+'_2.jpg' & color='#FFFFFF'

; isynfast,clwmap,tmpmap,fwhm=7,sim=1,nlmax=lmax,nsmax=nside,silent=1,iseed=-123

; cartview, tmpmap, reso=15, pxsize=pxsize, pysize=pysize, crop=1, png=idlimg, silent=1



command1 = './vignette.sh -i 0 -o 100 -f 20 -c \'+color+' '+idlimg+' '+imimg
command2 = 'display '+imimg
command = command1+' ; '+command2+' & '
print,command
spawn,/sh, command

end



