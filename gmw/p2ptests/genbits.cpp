#include<stdlib.h>
#include<iostream>

using namespace std;

int num_bits_per_int = 8;

int range = 130;

int num_ints = 50;

unsigned seed = 32446438;

int main()
{
    srand(seed);

    for(int i = 0; i < num_ints; ++i) {
	int n = rand() % range;
	
	int count_bits = 0;
	do {
	    int modulus = n % 2;
	    cout << modulus << " ";
	    ++count_bits;
	    n = n / 2;
	} while(n > 0);
	if(n > 0) {
	    cout << "some error in inputs?" << endl;
	    exit(1);
	}
	if(count_bits < num_bits_per_int) {
	    int remaining = num_bits_per_int - count_bits;
	    for(int j = 0; j < remaining; ++j) {
		cout << "0 ";
	    }
	}	
    }
    return 0;
}
