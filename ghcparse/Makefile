# In case of profiling is needed: add -prof -fprof-auto to the ghc call
# but first make sure you have profiling versions of the library deps installed
ghcparse: ghcparse.hs process.hs stringify.hs
	ghc -O2 -rtsopts -threaded -package ghc -package ghc-paths -package filemanip -package bytestring -package monad-parallel process.hs stringify.hs ghcparse.hs

ghcparse-prof: ghcparse.hs
	ghc -O2 -rtsopts -threaded -prof -fprof-auto -package ghc -package ghc-paths -package filemanip -package monad-parallel ghcparse.hs

deps:
	cabal update
	cabal v2-install --enable-library-profiling ghc-paths --lib
	cabal v2-install --enable-library-profiling monad-parallel --lib
	cabal v2-install --enable-library-profiling filemanip --lib

clean:
	rm -f ghcparse.o ghcparse.hi ghcparse *~
