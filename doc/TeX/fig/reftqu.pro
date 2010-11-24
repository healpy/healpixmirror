pro reftqu, iau=iau, png=png,ps=ps
;+
; illustrates Healpix and IAU coordinates on the sphere
;
;
;-
tek_color
n=100
lat0=30.
lon0=20.
; lat = 40.
lat = 40.
lon = 50 ;30.
ramp = findgen(n)/(n-1)
xs = 700
frac = 0.99
charsize = 2 & charthick = 4
thick = 6
w= 0.9982d0
norm = 0.3
mat0 = euler_matrix_new(lon0,-lat0,0,/deg,/y)
mat  = euler_matrix_new(0,  lat ,-lon,/deg,/y)
!p.background = 255
!p.color = 0

file_out='ref_hpx'
title = 'HEALPix coordinates'
if keyword_set(iau) then begin
    title = 'IAU coordinates'
    file_out = 'ref_iau'
endif
hard_copy,file_out,ps=ps,png=png,/square
position = [0.05,0.05,0.92,0.92]
!p.position = position

map_set,lat0,lon0,/orth,/iso,/noborder,position=position,charsize=2,title=title

; lines
oplot,180.+ramp*360.,0*ramp,lines=2
for l0=-90.,181,90 do oplot,l0 + 0*ramp,90-180*ramp,lines=1
plots, lon, lat, ps=4

!p.color = 3
if (keyword_set(iau)) then begin
    xs = lon+0*ramp & ys = ramp*lat
    oplot,xs,ys
    xyouts,/data,xs[n*.7]+1,ys[n*.7],'!7d!6',charsize=charsize,charthick=charthick
    xs = ramp*lon & ys = 0+0*ramp
    oplot,xs,ys
    xyouts,/data,xs[n/2],ys[n/2]-5,'!7a!6',charsize=charsize,charthick=charthick
endif else begin
    xs = lon+0*ramp & ys = 90+ramp*(lat-90.)
    oplot,xs,ys
    xyouts,/data,xs[n/2]+1,ys[n/2],'!7h!6',charsize=charsize,charthick=charthick
    xs = ramp*lon & ys = 0+0*ramp
    oplot,xs,ys
    xyouts,/data,xs[n/2],ys[n/2]-5,'!7u!6',charsize=charsize,charthick=charthick
endelse
v1 = mat#[1.d0,0,0] & v1[2] = 0
vn = mat0#v1 & vn = vn/sqrt(total(vn*vn))
!p.color = 0
xyouts,/data,lon0+180,85,'North',charsize=charsize,align=0.5

plot,[-1,1],[-1,1],/nodata,/noerase,xstyle=5,ystyle=5,/iso,position=position
oplot,cos(ramp*2*!dpi)*w,sin(ramp*2*!dpi)*w
xyouts,/data,0.8,0.7,'East',charsize=charsize

oplot,[0,vn[1]],[0,vn[2]],color=3,lines=2

!p.color = 4
x=mat0#[1,0,0]*frac
y=mat0#[0,1,0]*frac
z=mat0#[0,0,1]*frac
arrow,0,0,x[1],x[2],/data,/solid
arrow,0,0,y[1],y[2],/data,/solid
arrow,0,0,z[1],z[2],/data,/solid
ff = 0.9
xyouts,/data,x[1]*ff,x[2]*ff,' !12x!6',charsize=charsize
xyouts,/data,y[1]*ff,y[2]*ff+0.05,'!12y!6',charsize=charsize
xyouts,/data,z[1]*ff^2,z[2]*ff^2,' !12z!6',charsize=charsize

!p.color = 0
hsize = 400.
sign = keyword_set(iau) ? -1 : 1
; e_r
v = mat#[1.d0,0,0]
vv = mat0#v
oplot,[0,vv[1]],[0,vv[2]]
arrow,vv[1],vv[2],vv[1]*(1+sign*norm),vv[2]*(1+sign*norm),thick=thick,/data,hsize=hsize
xyouts,/data,vv[1]*(1+sign*norm),vv[2]*(1+sign*norm),'Z ',charsize=charsize,align=1

; e_phi
z=[0.d0,0,1]
vphi = vect_prod(z,v)
vphi = vphi / sqrt(total(vphi*vphi))
vvphi = (mat0#vphi) * norm
arrow,vv[1],vv[2],vvphi[1]+vv[1],vvphi[2]+vv[2],thick=thick,/data,hsize=hsize
xyouts,/data,vvphi[1]+vv[1],vvphi[2]+vv[2],' Y',charsize=charsize

; e_theta
vth = sign * vect_prod(vphi,v)
vvth = mat0#vth * norm
arrow,vv[1],vv[2],vvth[1]+vv[1],vvth[2]+vv[2],thick=thick,/data,hsize=hsize
xyouts,/data,vvth[1]+vv[1],vvth[2]+vv[2],' X',charsize=charsize

hard_copy,/close

return
end
