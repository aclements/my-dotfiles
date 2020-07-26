all: install

dist/setup-config: xmonad-aclements.cabal Setup.lhs
	runhaskell Setup.lhs configure --ghc --user --prefix=$$HOME/.cabal

build: dist/setup-config
	runhaskell Setup.lhs build

install: build
	runhaskell Setup.lhs install
	if [[ -e ~/.xmonad/xmonad.hs ]]; then touch ~/.xmonad/xmonad.hs; fi

doc: dist/setup-config
	runhaskell Setup.lhs haddock --hyperlink-source

clean:
	runhaskell Setup.lhs clean

.PHONY: build install doc
