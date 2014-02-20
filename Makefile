all:
	cabal install && cp .hsenv/cabal/bin/salmon /usr/local/bin/adit/salmon && salmon test.rb && less _test.rb
repo:
	new_bitbucket_repo salmon
spec:
	cabal install && .hsenv/cabal/bin/salmon-spec
