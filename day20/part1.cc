#include <stdio.h>

#include "test_input.h"

int costs[N][N] = {0};

void findstart(int& i, int& j)
{
	for (i = 0; i < N; i++)
		for (j = 0; j < N; j++)
			if (track[i][j] == 'S')
				return;
}

void fillcosts(void)
{
	int iprev = -1, jprev = -1, i, j;
	findstart(i, j);
	int cost = 1;
	do {
		int inext = i, jnext = j;
		if (i+1 < N && i+1 != iprev && track[i+1][j] != '#')
			inext = i+1;
		else if (i > 0 && i-1 != iprev && track[i-1][j] != '#')
			inext = i-1;
		else if (j+1 < N && j+1 != jprev && track[i][j+1] != '#')
			jnext = j+1;
		else if (j > 0 && j-1 != jprev && track[i][j-1] != '#')
			jnext = j-1;
		iprev = i, jprev = j;
		i = inext, j = jnext;
		costs[i][j] = cost;
		cost += 1;
	} while (track[i][j] != 'E');
}

// The part2 implementation is much cleaner, but for hysterical raisins...
int countcheats(int i, int j)
{
	if (track[i][j] == '#')
		return 0;
	int count = 0;
	if (track[i+1][j] == '#') {
		if (i+2 < N && costs[i+2][j] - costs[i][j] - 2 >= CUTOFF)
			count += 1;
		if (costs[i+1][j+1] - costs[i][j] - 2 >= CUTOFF)
			count += 1;
		if (costs[i+1][j-1] - costs[i][j] - 2 >= CUTOFF)
			count += 1;
	}
	if (track[i-1][j] == '#') {
		if (i-2 > 0 && costs[i-2][j] - costs[i][j] - 2 >= CUTOFF)
			count += 1;
		if (costs[i-1][j+1] - costs[i][j] - 2 >= CUTOFF)
			count += 1;
		if (costs[i-1][j-1] - costs[i][j] - 2 >= CUTOFF)
			count += 1;
	}
	if (track[i][j+1] == '#')
		if (j+2 < N && costs[i][j+2] - costs[i][j] - 2 >= CUTOFF)
			count += 1;
	if (track[i][j-1] == '#')
		if (j-2 > 0 && costs[i][j-2] - costs[i][j] - 2 >= CUTOFF)
			count += 1;
	return count;
}

int main(void)
{
	fillcosts();
	int total = 0;
	for (int i = 0; i < N; i++)
		for (int j = 0; j < N; j++)
			total += countcheats(i, j);
	printf("%d\n", total);
	return 0;
}
