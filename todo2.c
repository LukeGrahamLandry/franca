#define STB_TRUETYPE_IMPLEMENTATION
#include "/Users/luke/Downloads/stb-f0569113c93ad095470c54bf34a17b36646bbbb5/stb_truetype.h"
#include "stdio.h"
char ttf_buffer[1<<25] = {0};

int main(int argc, char **argv)
{
   stbtt_fontinfo font;
   unsigned char *bitmap;
   printf("%d\n", argc);
   int w,h,i,j;
   int c = (argc > 1 ? atoi(argv[1]) : 'a');
    int s = (argc > 2 ? atoi(argv[2]) : 20);
   
   char *path = argc > 3 ? argv[3] : "font.ttf";
   printf("%d %d %d %d %d %s %ld\n", w,h,i,j,c, path, ttf_buffer);
   fread(ttf_buffer, 1, 1<<25, fopen(path, "rb"));

   int off = stbtt_GetFontOffsetForIndex(ttf_buffer,0);
   printf("A %d\n", off);
   stbtt_InitFont(&font, ttf_buffer, off);
   printf("B\n");
   bitmap = stbtt_GetCodepointBitmap(&font, 0,stbtt_ScaleForPixelHeight(&font, s), c, &w, &h, 0,0);
   printf("C\n");
   int putchar(int);
   for (j=0; j < h; ++j) {
      for (i=0; i < w; ++i)
         putchar(" .:ioVM@"[bitmap[j*w+i]>>5]);
      putchar('\n');
   }
   return 0;
}