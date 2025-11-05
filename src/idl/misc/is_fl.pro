; -----------------------------------------------------------------------------
;
;  Copyright (C) 1997-2013  Krzysztof M. Gorski, Eric Hivon, Anthony J. Banday
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
;  For more information about HEALPix see http://healpix.sourceforge.net
;
; -----------------------------------------------------------------------------
function is_fl
;+
;   flag = is_fl()
;
;   flag will take value 1 if Fawlty Language (FL) is being run instead of IDL
;   and 0 otherwise
;
;   For more information on FL see
;   http://www.fawlty.uhostall.com/
;
;   2017-06-02
;   2024-10-25 works for old (~<0.97.49) and new (~0.97.54) FL
;-


flag_old = 0
;#fl flag_old++  ; only seen by (old) FL

defsysv,'!FL', exists=flag_new  ; only defined in (new) FL

flag = flag_old or flag_new
return, flag
end
