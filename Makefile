all:
	cabal install && cp .hsenv/cabal/bin/salmon /usr/local/bin/adit/salmon && salmon
repo:
	new_bitbucket_repo salmon
spec:
	cabal install && .hsenv/cabal/bin/salmon-spec
