all:
	ghc --make -outputdir build UnifiedScript.hs
clean:
	rm -rf build UnifiedScript
