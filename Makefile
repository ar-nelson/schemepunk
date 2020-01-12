.PHONY: test-chibi test-gauche test-gerbil test-kawa test-larceny test test-all watch

schemepunk:
	ln -s . schemepunk

test-chibi: schemepunk
	./scripts/test-chibi.sh

test-gauche: schemepunk
	./scripts/test-gauche.sh

test-gerbil: schemepunk
	./scripts/test-gerbil.sh

test-kawa: schemepunk
	./scripts/test-kawa.sh

test-larceny: schemepunk
	./scripts/test-larceny.sh

test: test-gauche

test-all: test-chibi test-gauche test-gerbil test-kawa test-larceny

watch:
	nodemon -e scm,sld --exec 'make test || exit 1'
