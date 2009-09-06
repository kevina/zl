all: class-new_abi-fct.so

class-new_abi.zl: class-new_abi.zl.patch
	cp ../class.zl .
	patch class.zl < class-new_abi.zl.patch
	mv class.zl class-new_abi.zl

class-new_abi-fct.so: ../zl class-new_abi.zl
	../zl -C class-new_abi.zl

test:
	./basic_tests

clean:
	rm -f *.out *.zls *.log *.s *.so *~ core *.o

