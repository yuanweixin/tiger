

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#ifndef UINT64_MAX
#error "tiger only works for 64-bit."
#endif

#ifndef SIZE_MAX
#error "please use a newer version of C compiler that has SIZE_MAX defined"
#endif

_Static_assert(sizeof(void *) <= sizeof(int64_t), "tiger assumes pointers are at most 64-bits");

// Note: size_t is only guaranteed to be at least 16 bits. need to check.
_Static_assert(sizeof(size_t) <= sizeof(int64_t), "tiger assumes size_t is at most 64 bits");

int64_t *
initArray(size_t size, int64_t init)
{
    int64_t i;
    if ((SIZE_MAX - sizeof(int64_t)) / sizeof(int64_t) < size)
    {
        fprintf(stderr, "the requested array size (of int64_t) %lu was too big to allocate", size);
        exit(-1);
    }
    int64_t *a = (int64_t *)malloc(size * sizeof(int64_t) + sizeof(int64_t));
    for (i = 0; i < size; i++)
        a[i] = init;
    a[0] = size;
    return a;
}

int64_t *allocRecord(size_t size)
{
    int64_t i;
    int64_t *p, *a;
    p = a = (int64_t *)malloc(size);
    for (i = 0; i < size; i += sizeof(int64_t))
        *p++ = 0;
    return a;
}

struct string
{
    size_t length;
    unsigned char chars[1];
};

int64_t stringEqual(struct string *s, struct string *t)
{
    int64_t i;
    if (s == t)
        return 1;
    if (s->length != t->length)
        return 0;
    for (i = 0; i < s->length; i++)
        if (s->chars[i] != t->chars[i])
            return 0;
    return 1;
}

void print(struct string *s)
{
    int64_t i;
    unsigned char *p = s->chars;
    for (i = 0; i < s->length; i++, p++)
        putchar(*p);
}

void flush()
{
    fflush(stdout);
}

struct string consts[256];
struct string empty = {0, ""};

extern int64_t tigermain();

int main()
{
    int64_t i;
    for (i = 0; i < 256; i++)
    {
        consts[i].length = 1;
        consts[i].chars[0] = i;
    }
    return (int) tigermain();
}

int64_t ord(struct string *s)
{
    if (s->length == 0)
        return -1;
    else
        return s->chars[0];
}

struct string *chr(int64_t i)
{
    if (i < 0 || i >= 256)
    {
        printf("chr(%lu) out of range\n", i);
        exit(1);
    }
    return consts + i;
}

int64_t size(struct string *s)
{
    return s->length;
}

struct string *substring(struct string *s, int64_t first, int64_t n)
{
    if (first < 0 || first + n > s->length)
    {
        printf("substring([%lu],%lu,%lu) out of range\n", s->length, first, n);
        exit(1);
    }
    if (n == 1)
        return consts + s->chars[first];
    {
        // this malloc is safe because substring is at most same size as input string.
        struct string *t = (struct string *)malloc(sizeof(int64_t) + n);
        int64_t i;
        t->length = n;
        for (i = 0; i < n; i++)
            t->chars[i] = s->chars[first + i];
        return t;
    }
}

struct string *concat(struct string *a, struct string *b)
{
    if (a->length == 0)
        return b;
    else if (b->length == 0)
        return a;
    else
    {
        int64_t i, n = a->length + b->length;

        if ((SIZE_MAX - n < a->length) || (SIZE_MAX - n - a->length) < b->length)
        {
            fprintf(stderr, "the requested size of %lu + %lu is too big to allocate", a->length, b->length);
            exit(-1);
        }

        struct string *t = (struct string *)malloc(sizeof(int64_t) + n);
        t->length = n;
        for (i = 0; i < a->length; i++)
            t->chars[i] = a->chars[i];
        for (i = 0; i < b->length; i++)
            t->chars[i + a->length] = b->chars[i];
        return t;
    }
}

int64_t not(int64_t i)
{
    return !i;
}

// uses capital C to avoid conflicting with stdio function.
// do not dig the pascal case, wtf this is C, should be snake casing.
// but using pascal to be consistent with rest of the file.
struct string *getChar()
{
    int64_t i = getc(stdin);
    if (i == EOF)
        return &empty;
    else
        return consts + i;
}