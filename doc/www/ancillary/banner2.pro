
init_healpix
fits2cl,/planck3,clplanck
nside = 1024
lmax = 2*nside
pxsize=1000
pysize=400
tmpmap = '/tmp/map.fits'
tag = 'banner_2'
idlimg = './'+tag+'_0.png'
;imimg  = './'+tag+'_1.jpg' & color='#070707'
;imimg  = './'+tag+'_2.jpg' & color='#FFFFFF'
imimg  = './'+tag+'_2.png' & color='#FFFFFF'
finimg = './'+tag+'_2.jpg'

;isynfast,clplanck,tmpmap,fwhm=7,sim=1,nlmax=lmax,nsmax=nside,silent=1,iseed=-123

;cartview, tmpmap, reso=15, pxsize=pxsize, pysize=pysize, crop=1, png=idlimg, silent=1,colt='planck1'



command1 = './vignette.sh -i 0 -o 100 -f 20 -c \'+color+' '+idlimg+' '+imimg
command2 = 'gm convert '+imimg+' '+finimg
command3 = 'open '+finimg
command = command1+' ; '+command2+' ; '+command3+' & '
print,command
spawn,/sh, command

end



