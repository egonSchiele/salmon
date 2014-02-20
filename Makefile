all:
	cabal install && .hsenv/cabal/bin/salmon
repo:
	new_bitbucket_repo salmon
spec:
	cabal install && .hsenv/cabal/bin/salmon-spec
