STORAGE_SIZE := 1 10 100 1000 10000 # 1000+ benchmarking gets too slow for my mediocre laptop

def:
	@echo -e "Usage:"
	@echo -e "\tmake build[all|cpp|cppbench|ada]"
	@echo -e "\tmake run[all|cpp|cppbench|ada]"
	@echo -e "\tmake clean"

buildall: buildcpp buildada

runall: runcpp runada


# CPP #####################################

buildcpp: src_cpp/*
	-mkdir -p obj_cpp
	for SS in $(STORAGE_SIZE); do	\
		g++ -Wall -Wextra -pedantic -std=c++20 -g -DMAX_OBJS_PREPROC=$$SS src_cpp/main.cpp -o obj_cpp/main_$$SS;	\
	done

runcpp: buildcpp
	for SS in $(STORAGE_SIZE); do	\
		./obj_cpp/main_$$SS;	\
	done

buildcppbench: src_cpp/*
	-mkdir -p obj_cpp
	for SS in $(STORAGE_SIZE); do	\
		g++ -Wall -Wextra -pedantic -std=c++20 -g -DMAX_OBJS_PREPROC=$$SS src_cpp/bench.cpp -lbenchmark -o obj_cpp/bench_$$SS;	\
	done

runcppbench: buildcppbench
	for SS in $(STORAGE_SIZE); do	\
		./obj_cpp/bench_$$SS;	\
	done


# ADA #####################################

buildada: src_ada/*
	-mkdir -p obj_ada
	gnatmake -P arena.gpr

runada: buildada
	for SS in $(STORAGE_SIZE); do	\
		./obj_ada/main $$SS;	\
	done


# MISC ####################################

clean:
	-rm -rf obj_*

