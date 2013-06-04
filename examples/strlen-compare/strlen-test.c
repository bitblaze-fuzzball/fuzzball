#include <stdio.h>
#include <string.h>
#include <assert.h>

size_t tricky_strlen(const char *str) {
    const char *char_ptr;
    const unsigned long int *longword_ptr;
    unsigned long int longword, magic_bits, himagic, lomagic;

#if 0
    /* Handle the first few characters by reading one character at a time.
       Do this until CHAR_PTR is aligned on a longword boundary.  */
    for (char_ptr = str; ((unsigned long int) char_ptr
			  & (sizeof (longword) - 1)) != 0;
	 ++char_ptr)
	if (*char_ptr == '\0')
	    return char_ptr - str;
#else
    char_ptr = str;
#endif

    longword_ptr = (unsigned long int *) char_ptr;
    
    magic_bits = 0x7efefeffL;
    himagic = 0x80808080L;
    lomagic = 0x01010101L;

    for (;;)
    {
      longword = *longword_ptr++;

      if (((longword - lomagic) & himagic) != 0)
	{
	  /* Which of the bytes was the zero?  If none of them were, it was
	     a misfire; continue the search.  */

	  const char *cp = (const char *) (longword_ptr - 1);

	  if (cp[0] == 0)
	    return cp - str;
	  if (cp[1] == 0)
	    return cp - str + 1;
	  if (cp[2] == 0)
	    return cp - str + 2;
	  if (cp[3] == 0)
	    return cp - str + 3;
	  /*else
	    return 42;*/
	}
    }
}

size_t simple_strlen(const char *s) {
    const char *p;

    for (p = s; *p; ++p);
    return p - s;
}

size_t scasb_strlen(const char *str) {
    int cnt;

    asm("cld\n"                   /* Search forward.  */
	"repnz\n"                 /* Look for a zero byte.  */
	"scasb" /* %0, %1, %3 */ :
	"=c" (cnt) : "D" (str), "0" (-1), "a" (0));
    
    return -2 - cnt;
}

#ifndef SIZE
#define SIZE 10
#endif

int strlen_compare(char *buf) {
    /* buf[SIZE - 1] = '\0'; */
    int simple = simple_strlen(buf);
    int scasb = scasb_strlen(buf);
    int tricky = tricky_strlen(buf);
    return (simple - scasb) | (simple - tricky);
}

char foo[1024];

int main(int argc, char **argv) {
    int r;
    strncpy(foo, argv[1], sizeof(foo));
    r = strlen_compare(foo);
    printf("%d %d %d\n", simple_strlen(foo), scasb_strlen(foo),
	   tricky_strlen(foo));
    assert(!r);
    return 0;
}
