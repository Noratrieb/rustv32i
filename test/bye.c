#include<stdio.h>

int main(int argc, char *argv[]) {
	int sum = 0;
	for (int i = 0; i < 200000; i++) {
		sum += i;	
	}
  printf("meow %d\n", sum);
}
