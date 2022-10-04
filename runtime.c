#include <stdio.h>

int *initArray(int size, int init)
{
    int i;
    int *a = (int *)malloc(size * sizeof(int));
    for (i = 0; i < size; i++)
        a[i] = init;
    return a;
}

struct string
{
    int length;
    unsigned char chars[1];
};

int stringEqual(struct string *s, struct string *t)
{
    int i;
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
    int i;
    unsigned char *p = s->chars;
    for (i = 0; i < s->length; i++, p++)
        putchar(*p);
}

void printi(int num) {
    printf("%d", num);
}

int ord(struct string *s)
{
    if (s->length == 0)
        return -1;
    else
        return s->chars[0];
}

struct string *concat(struct string *a, struct string *b)
{
    if (a->length == 0)
        return b;
    else if (b->length == 0)
        return a;
    else
    {
        int i, n = a->length + b->length;
        struct string *t = (struct string *)malloc(sizeof(int) + n);
        t->length = n;
        for (i = 0; i < a->length; i++)
            t->chars[i] = a->chars[i];
        for (i = 0; i < b->length; i++)
            t->chars[i + a->length] = b->chars[i];
        return t;
    }
}