#include<stdio.h>
#include<ctype.h>

int main(int argc, char** argv) {

 int c;
 int count = 0;

 while(( c = getc(stdin)) != EOF) {
   if( ! isspace(c) ) count++;
 }

 printf("%d\n", count);

 return 0;
}
