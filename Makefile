ifndef schmlUnderConstruction
$(warning schmlUnderConstruction is not set, you might get runtime errors if there are TODOs)
endif

all: 
	raco make -j 5 tests/main.rkt

clean:
	find src/ -path '*/compiled/*' -delete
	find src/ -type d -name "compiled" -delete
