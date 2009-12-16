all:
	ghc --make -O3 main.hs -i$(HOME)/Euterpea/src:$(HOME)/Euterpea/dist/build

clean:
	rm *.o *.hi main

