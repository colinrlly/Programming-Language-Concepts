#include "all.h"
/* printbuf.c 924d */
struct block {
    struct block *next;   // never NULL
    int size;  // space in 'chars'
    char chars[];
};

struct Printbuf {
    struct block *lastblock; // pointer to last in circular list
    char *free;              // location of next free space
    char *limit;             // beyond end of current buffer
};
/* printbuf.c 924e */
static void grow(Printbuf buf) {
    assert(buf && buf->lastblock);
    assert(buf->free && buf->limit);
    assert(buf->free == buf->limit);
    assert(buf->limit - &buf->lastblock->chars[0] == buf->lastblock->size);
    unsigned n = buf->lastblock->size;
    n = (n * 13) / 10;   // 30% size increase

    struct block *block = malloc(n + sizeof(*block));
    assert(block);
    block->next = buf->lastblock->next;
    block->size = n;
    buf->lastblock->next = block;
    buf->lastblock = block;
    buf->free = &block->chars[0];
    buf->limit = buf->free + n;
}
/* printbuf.c 925a */
void bufput(Printbuf buf, char c) {
    assert(buf && buf->free && buf->limit);
    if (buf->free == buf->limit) {
        grow(buf);
        assert(buf && buf->free && buf->limit);
        assert(buf->limit > buf->free);
    }
    *buf->free++ = c;
}
/* printbuf.c 925b */
void bufreset(Printbuf buf) {
   assert(buf && buf->lastblock);
   while (buf->lastblock->next != buf->lastblock) {
       struct block *second = buf->lastblock->next->next;
       free(buf->lastblock->next);
       buf->lastblock->next = second;
   }
   buf->free  = &buf->lastblock->chars[0];
   buf->limit = buf->free + buf->lastblock->size;
}
/* printbuf.c 925c */
Printbuf printbuf(void) {
   Printbuf buf = malloc(sizeof(*buf));
   assert(buf);
   int n = 5;   // small as it can be and still grow
   struct block *block = malloc(sizeof(*block) + n);
   assert(block);
   block->next = block;
   block->size = n;
   buf->lastblock = block;
   buf->free = &block->chars[0];
   buf->limit = buf->free + n;
   return buf;
}
/* printbuf.c 925d */
static int nchars(Printbuf buf) {
   assert(buf && buf->lastblock && buf->limit && buf->free);
   int n = buf->limit - buf->free;
   for ( struct block *block = buf->lastblock->next
       ; block != buf->lastblock
       ; block = block->next
       )
       n += block->size;
   return n;
}
/* printbuf.c 926a */
char *bufcopy(Printbuf buf) {
   assert(buf);
   int n = nchars(buf);
   char *s = malloc(n+1);
   char *p = s;
   for ( struct block *block = buf->lastblock->next
       ; block != buf->lastblock
       ; block = block->next
       ) 
   {
       memcpy(p, &block->chars[0], block->size);
       p += block->size;
   }
   char *first = &buf->lastblock->chars[0];
   char *free  = buf->free;
   memcpy(p, first, free - first);
   p += free - first;
   *p++ = '\0';
   assert(p - s == n);
   return s;
}
