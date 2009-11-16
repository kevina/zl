all: class-new_abi-fct.so class-this_reg-fct.so class-w_abi-fct.so

class-new_abi-fct.so: ../zl class-new_abi.zl
	../zl -C class-new_abi.zl > class-new_abi-fct.log

class-this_reg-fct.so: ../zl class-this_reg.zl
	../zl -C class-this_reg.zl > class-this_reg-fct.log

class-w_abi-fct.so: ../zl class-w_abi.zl class-new_abi-fct.so class-this_reg-fct.so
	../zl -C class-w_abi.zl > class-w_abi-fct.log

test: all
	./basic_tests

clean:
	rm -f *.out *.zls *.log *.s *.so *~ core *.o

