all: run no-bin

run: build 
	./main

build: program clean

check: program clean no-bin

program: 
	ghc src/main/main.hs
	mv src/main/*.hi target
	mv src/main/*.o target
	mv src/main/main .

no-bin:
	rm main 

clean:
	rm target/*.*
	


