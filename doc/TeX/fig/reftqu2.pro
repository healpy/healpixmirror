pro reftqu2, iau=iau, png=png,ps=ps
;+
; illustrates Healpix and IAU coordinates on tangent plan
;
;
;-

tek_color

file_out='ref2_hpx'
title = 'HEALPix coordinates'
if keyword_set(iau) then begin
    title = 'IAU coordinates'
    file_out = 'ref2_iau'
endif
hard_copy,file_out,ps=ps,png=png,/square
position = [0.05,0.05,0.92,0.92]
!p.background = 255
!p.color = 0
!p.position = position
!p.charsize = 2

plot,[-1,1],[-1,1],/nodata,xstyle=5,ystyle=5,/iso,title=title

do_iau = keyword_set(iau)

m = 0.9
if do_iau then begin
    xyouts, -0.05, 0.9*m, 'North',align=1.
endif else begin
    xyouts, -0.05,-0.9*m, 'South',align=1.
endelse

xyouts, m*0.9 ,0.05,'East',align=0.5
if (do_iau) then begin
    arrow,/data,0,0,0,m
    xyouts,0.05,0.9*m,'X',align=0.5
    arrow,/data,0,0,m,0
    xyouts,0.9*m,-0.1,'Y'
endif else begin
    arrow,/data,0,0,0,-m
    xyouts,0.05,-0.9*m,'X',align=0.5
    arrow,/data,0,0,m,0
    xyouts,0.9*m,-0.1,'Y'
endelse

thick = 8
s = sqrt(2)
x = 0.5
y = x * s
p = 0.7 / s

n = 10
ramp = findgen(n)/(n-1)
psi = 30.
psi = psi * !dtor
spsi = sin(psi) & cpsi = cos(psi)
r1 = 0.75
r2 = 0.6

if (do_iau) then begin
    plots,r1*[0,spsi],r1*[0,cpsi]
    xc = r2*sin(ramp*psi)   & yc = r2*cos(ramp*psi)
    plots,xc,yc
    xyouts,xc[n/2],yc[n/2]+0.05,'!7w!6',align=0.5
endif else begin
    plots,r1*[0,spsi],r1*[0,-cpsi]
    xc = r2*sin(ramp*psi)   & yc = -r2*cos(ramp*psi)
    plots,xc,yc
    xyouts,xc[n/2],yc[n/2]-0.1,'!7w!6',align=0.5
endelse


!p.charthick = 3
if (do_iau) then begin
    plots,[x,-x], [x,-x],thick=thick, col=3
    xyouts,p/s,p/s,' +U', col=3

    plots,[x,-x],-[x,-x],thick=thick, col=3, lines=2
    xyouts,p/s,-p/s,'  -U', col=3
endif else begin
    plots,[x,-x], [x,-x],thick=thick, col=3, lines=2
    xyouts,p/s,p/s,'  -U', col=3

    plots,[x,-x],-[x,-x],thick=thick, col=3
    xyouts,p/s,-p/s,' +U', col=3
endelse

plots,[0,0]*y, [1,-1]*y,thick=thick, col=2
xyouts,0.05,-p,'+Q', col=2

plots,[1,-1]*y, [0,0]*y,thick=thick, col=2, lines=2
xyouts,p,-0.1,'-Q', col=2

hard_copy,file_out,/close
!p.charthick = 1
!p.charsize = 1

return
end
