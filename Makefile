all:
	cabal install && cp .hsenv/cabal/bin/salmon /usr/local/bin/adit/salmon && salmon test.slm && less test.rb
repo:
	new_bitbucket_repo salmon
spec:
	cabal install && .hsenv/cabal/bin/salmon-spec

run:
	/Users/abhargava/salmon/.stack-work/install/x86_64-osx/lts-3.10/7.10.2/bin/salmon test.slm && less test.rb
