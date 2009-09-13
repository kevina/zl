all: class-new_abi-fct.so class-this_reg-fct.so

class-new_abi-fct.so: ../zl class-new_abi.zl
	../zl -C class-new_abi.zl

class-this_reg-fct.so: ../zl class-this_reg.zl
	../zl -C class-this_reg.zl

test:
	./basic_tests

clean:
	rm -f *.out *.zls *.log *.s *.so *~ core *.o

