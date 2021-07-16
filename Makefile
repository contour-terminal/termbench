
all: termbench

termbench: termbench.cpp
	g++ -o $@ $^ -std=c++17 -Wall -O $(CXXFLAGS)

clean:
	rm termbench

.PHONY: clean all
