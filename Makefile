all:
	cabal install && cp .hsenv/cabal/bin/salmon /usr/local/bin/adit/salmon && rm test.rb && salmon test.slm && less test.rb
repo:
	new_bitbucket_repo salmon
spec:
	cabal install && .hsenv/cabal/bin/salmon-spec
