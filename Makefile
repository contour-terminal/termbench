CXXFLAGS := -O

all: termbench

termbench: termbench.cpp
	g++ -o $@ $^ -std=c++17 -Wall $(CXXFLAGS)

clean:
	rm termbench

.PHONY: clean all
