ghc:
	nix-build -o ghc-frontend-result -A ghc.frontend

ghcjs:
	nix-build -o frontend-result -A ghcjs.frontend

run: ghc
	./ghc-frontend-result/bin/frontend

# only works on mac :)
runjs: ghcjs
	open ./frontend-result/bin/frontend.jsexe/index.html






# seems to be missing dependencies when linking
cabal-ghc:
	nix-shell -A shells.ghc --run "cabal new-build all"

# seems to be missing dependencies when linking
cabal-ghcjs:
	nix-shell -A shells.ghcjs --run "cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all"
