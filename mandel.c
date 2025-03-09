#include <stdio.h>

char chartab[] = {'.', '.', '.', ',', ',', ',',
		  ':', ':', ':', 'i', 'i', 'i',
		  'w', 'w', 'w', 'W', 'W', 'W',
		  '#', '#', ' '};

#define WIDTH 640
#define HEIGHT 480
#define MAX_ITER 20

int main() {
  int x0 = (-2) << 12;
  int x1 = 1 << 12;
  int y0 = (-1) << 12;
  int y1 = 1 << 12;
  int xstep = (x1 - x0) / WIDTH;
  int ystep = (y1 - y0) / HEIGHT;
  int x2, y2, xy;

  FILE *f = fopen("mand.ppm", "w");
  fprintf(f, "P6 %d %d 255\n", WIDTH, HEIGHT);

  int cy = y0;
  for (int ypos = 0; ypos < HEIGHT; ypos++) {
    int cx = x0;
    for (int xpos = 0; xpos < WIDTH; xpos++) {
      int x = cx;
      int y = cy;
      int n;
      for (n = MAX_ITER; n > 0; n--) {
	y2 = y * y;
	x2 = x * x;
	y2 >>= 12;
	x2 >>= 12;
	if ((x2 + y2) >= 0x4000)
	  break;

	xy = x * y;
        xy /= 4096;

	y = xy + xy + cy;
	x = x2 - y2 + cx;
      }
      //printf("%c", chartab[n]);
      unsigned char c = n * (255 / MAX_ITER);
      for (int i = 0; i < 3; i++) {
	fputc((unsigned char)(n * 12), f);
	//fputc(' ', f);
      }
      cx += xstep;
    }
    //printf("\n");
    cy += ystep;
  }
  return 0;
}
	
