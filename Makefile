.PHONY: jfac_testing.exe jfac_production.exe \
	redb extendb package production

jfac_testing.exe:
	urweb -output jfac_testing.exe jfac

jfac_production.exe:
	urweb -protocol fastcgi -output jfac_production.exe jfac

redb:
	-dropdb jfac
	createdb jfac
	psql -f jfac.sql jfac

redbprod:
	-sudo -u www-data dropdb jfac
	sudo -u www-data createdb jfac
	sudo -u www-data psql -f jfac.sql jfac

extendb:
	psql -f jfac.sql jfac

LIB=/usr/local/lib

package: jfac_production.exe
	mkdir -p dist
	rm -rf dist/*
	mkdir dist/public_html
	cp index.html dist/public_html/
	cp jfac_production.exe dist/
	cp jfac.sql dist/
	cp -d ${LIB}/liburweb.so* dist/
	cp -d ${LIB}/liburweb_fastcgi.so* dist/
	cp -d ${LIB}/liburweb_ajaxUpload.so* dist/
	cp Makefile.prod dist/Makefile

production: package
	rsync -az dist/* ubuntu@upo.csail.mit.edu:
