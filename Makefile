CXX = g++
CXXFLAGS = -Wall -ggdb3 -O0

all: termbench

termbench: termbench.cpp
	$(CXX) -o $@ $^ -std=c++17 $(CXXFLAGS)

clean:
	rm termbench

.PHONY: clean all
