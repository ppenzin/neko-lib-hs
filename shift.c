#include <stdio.h>

int
main(void) {
  unsigned num;
  scanf("%d", &num);
  printf("0x%x\n", (num << 2));
  return 0;
}
