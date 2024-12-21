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

int countcheats(int i, int j, int cheats)
{
	if (track[i][j] == '#')
		return 0;
	int count = 0;
	for (int di = 0; di <= cheats; di++) {
		for (int dj = 0; dj <= cheats - di; dj++) {
			int d = di + dj;
			if (i+di < N && j+dj < N && costs[i+di][j+dj]-costs[i][j]-d >= CUTOFF)
				count += 1;
			if (i-di >= 0 && j-dj >= 0 && costs[i-di][j-dj]-costs[i][j]-d >= CUTOFF)
				count += 1;
			if (di && dj && i-di >= 0 && j+dj < N && costs[i-di][j+dj]-costs[i][j]-d >= CUTOFF)
				count += 1;
			if (di && dj && i+di < N && j-dj >= 0 && costs[i+di][j-dj]-costs[i][j]-d >= CUTOFF)
				count += 1;
		}
	}
	return count;
}

int main(void)
{
	fillcosts();
	int total = 0;
	for (int i = 0; i < N; i++)
		for (int j = 0; j < N; j++)
			total += countcheats(i, j, 20);
	printf("%d\n", total);
	return 0;
}
