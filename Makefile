.PHONY: test-chibi test-chicken test-gauche test-gerbil test-kawa test-larceny test-sagittarius test test-all watch

schemepunk:
	ln -s . schemepunk

test-chibi: schemepunk
	./scripts/test-chibi.sh "$$(./scripts/find-tests.sh)"

test-chicken: schemepunk
	./scripts/test-chicken.sh "$$(./scripts/find-tests.sh)"

test-gauche: schemepunk
	./scripts/test-gauche.sh "$$(./scripts/find-tests.sh)"

test-gerbil: schemepunk
	./scripts/test-gerbil.sh "$$(./scripts/find-tests.sh)"

test-kawa: schemepunk
	./scripts/test-kawa.sh "$$(./scripts/find-tests.sh)"

test-larceny: schemepunk
	./scripts/test-larceny.sh "$$(./scripts/find-tests.sh)"

test-sagittarius: schemepunk
	./scripts/test-sagittarius.sh "$$(./scripts/find-tests.sh)"

test: test-gauche

test-all: test-chibi test-gauche test-gerbil test-kawa test-larceny test-chicken test-sagittarius

watch:
	nodemon -e scm,sld --exec 'make test || exit 1'
