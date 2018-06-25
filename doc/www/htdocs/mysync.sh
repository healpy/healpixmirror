#! /bin/csh -f

# web.sourceforge.net:/home/project-web/healpix/htdocs
#-e ssh

rsync -avP -e ssh \
	    --exclude=.svn \
	    --exclude=html --exclude=pdf \
	    --exclude='svn-commit*' \
	    --exclude=tag_manager.txt \
	    --exclude='doc?.pdf' \
	    --exclude=.DS_Store \
	    --exclude='*-BK' --exclude='*-bk' \
	    --exclude=test.php --exclude=mysync.sh \
	    . \
	    web.sourceforge.net:/home/project-web/healpix/htdocs

  
exit

