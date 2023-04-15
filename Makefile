project: clean-all
	ghc -o dbg Main.hs
	make clean
	
clean:
	rm -rf *.hi *.o

clean-all:
	rm -rf *.hi *.o ./dbg