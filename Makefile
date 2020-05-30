.PHONY: test-chibi test-chicken test-gambit test-gauche test-gerbil test-kawa test-larceny test-sagittarius test test-all clean watch

schemepunk:
	mkdir schemepunk
	for module in $$(find . -name '*.sld'); do\
	  mkdir -p "schemepunk/$$(dirname "$$module")" &&\
	  ln "$$module" "schemepunk/$${module#./}";\
	done
	ln -s ../polyfills schemepunk/polyfills
	ln -s ../scripts schemepunk/scripts

test-chibi: schemepunk
	./scripts/test-chibi.sh "$$(./scripts/find-tests.sh)"

test-chicken: schemepunk
	./scripts/test-chicken.sh "$$(./scripts/find-tests.sh)"

test-gambit: schemepunk
	./scripts/test-gambit.sh "$$(./scripts/find-tests.sh)"

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

test-all: test-chibi test-gambit test-gauche test-gerbil test-kawa test-larceny test-chicken test-sagittarius

clean:
	rm -rf schemepunk
	find . -name '*.c' -delete
	find . -name '*.o' -delete
	find . -name '*.o1' -delete
	find . -name '*.o2' -delete
	find . -name '*.slfasm' -delete

watch:
	nodemon -e scm,sld --exec 'make test || exit 1'
