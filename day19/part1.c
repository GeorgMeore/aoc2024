#include <stdio.h>

#include "test_input.h"

#define NPATTERNS (sizeof(patterns)/sizeof(patterns[0]))
#define NDESIGNS (sizeof(designs)/sizeof(designs[0]))
#define MAXSLEN 128

int strlength(char *s)
{
	int len;
	for (len = 0; *s; s++)
		len++;
	return len;
}

int startswith(char *s, char *p)
{
	for (; *s && *p; s++, p++)
		if (*s != *p)
			return 0;
	return !*p;
}

int mem[MAXSLEN];

int fitcalc(char *design, int j)
{
	if (!design[j])
		return 1;
	if (mem[j] == -1) {
		int i;
		for (i = 0, mem[j] = 0; i < NPATTERNS && !mem[j]; i++)
			if (startswith(design + j, patterns[i]))
				mem[j] = fitcalc(design, j + strlength(patterns[i]));
	}
	return mem[j];
}

int fit(char *design)
{
	int j;
	for (j = 0; design[j]; j++)
		mem[j] = -1;
	return fitcalc(design, 0);
}

int main(void)
{
	int i, count;
	for (i = 0, count = 0; i < NDESIGNS; i++)
		count += fit(designs[i]);
	printf("%d\n", count);
	return 0;
}