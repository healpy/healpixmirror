;+
; NAME:
;       epstopdf
;
; PURPOSE:
;       PostScript to PDF conversion
;
; CATEGORY:
;
;
; CALLING SEQUENCE:
;       epstopdf, ps_file [, pdf_file], [DELETE_PS=, HELP=, OPTIONS=, ]
;
; INPUTS:
;      ps_file:  input PostScript file
;
; OPTIONAL INPUTS:
;      pdf_file:  output PDF file
;       if absent, it will be generated from ps_file by replacing
;       (.ps or .eps) suffix with .pdf
;
;
; KEYWORD PARAMETERS:
;      DELETE_PS: if set, the PostScript file will be deleted
;
;      HELP: is set, this documentation header is printed out
;        and the routine leaves
;
;      OPTIONS: string containing options to be passed to epstopdf.pl
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;      calls external script
;      creates and delete files
;
; RESTRICTIONS:
;
; PROCEDURE:
;     calls public domain epstopdf.pl script (which itself calls 'gs') to
;     convert a PS file into PDF.
;     If that fails, D.W. Fanning's  (Coyote) cgps2pdf is called instead.
;
; EXAMPLE:
;     epstopdf, 'file.ps', 'file.pdf'
;
;
; MODIFICATION HISTORY:
;   2015-03-17:  version 1.0
;
;-
pro epstopdf, ps_file, pdf_file, $
              delete_ps = delete_ps, $
              help = help, $
              options = options


routine = 'epstopdf'
syntax = routine+', ps_file [,pdf_file], [delete_ps=, options=, help=]'

if keyword_set(help) then begin
    doc_library,routine
    return
endif

np = n_params()
if (np ne 1 and np ne 2) then begin
    print,syntax
    return
endif

if (np eq 1) then begin
    root_name = cgRootName(ps_file, DIRECTORY=thisDir)
    pdf_file  = Filepath(ROOT_DIR=thisDir, root_name + ".pdf")
endif


; first try epstopdf.pl which produces PDF files with BoundingBox
bin = 'epstopdf.pl'
defsysv, '!healpix',          exists = exists
defsysv, '!healpix.path.src', exists = exists2
if ~exists then init_healpix
bin_path = file_which(bin)
if (bin_path eq '' && exists2) then begin
    bin_path = file_search(!healpix.path.src, bin, count=count)
endif

myoptions = size(/tname,options) eq 'STRING' ? ' '+options[0]+' ' : ' '
if (bin_path ne '') then begin
    command =  bin_path[0]+' '+ps_file[0]+myoptions+' --outfile='+pdf_file[0]
    file_delete, pdf_file[0], /quiet
    spawn, /sh, command, out
    if (file_test(pdf_file[0]) && keyword_set(delete_ps)) then file_delete,ps_file[0]
endif

; in case of failure, use cgps2dpf which produces PDF files
; at paper size (letter, a4, ...)
if (bin_path eq '' || ~file_test(pdf_file[0])) then begin
    message,bin+' not available or not working! Trying cgps2pdf instead.',/info
    cgps2pdf, ps_file, pdf_file, delete_ps = delete_ps
endif


return
end

