# In case of profiling is needed: add -prof -fprof-auto to the ghc call
# but first make sure you have profiling versions of the library deps installed
ghcparse: ghcparse.hs process.hs stringify.hs
	ghc -O2 -Wall -Wno-unused-imports -rtsopts -threaded \
		-package ghc \
		-package ghc-paths \
		-package filemanip \
		-package bytestring \
		-package monad-parallel \
		-package GenericPretty \
		-package hashable \
		process.hs stringify.hs ghcparse.hs

ghcparse-prof: ghcparse.hs process.hs stringify.hs
	ghc -O2 -rtsopts -prof -fprof-auto \
		-package ghc \
		-package ghc-paths \
		-package filemanip \
		-package bytestring \
		-package monad-parallel \
		-package hashable \
		process.hs stringify.hs ghcparse.hs

deps:
	cabal v2-update
	cabal v2-install --enable-library-profiling ghc-paths --lib
	cabal v2-install --enable-library-profiling monad-parallel --lib
	cabal v2-install --enable-library-profiling filemanip --lib
	cabal v2-install --enable-library-profiling GenericPretty --lib
	cabal v2-install --enable-library-profiling casing --lib
	cabal v2-install --enable-library-profiling hashable --lib

clean:
	rm -f *.o *.hi ghcparse *~
