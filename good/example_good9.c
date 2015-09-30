int INF = 100000;

int n = 5;
int [n][n] d;
int i;
int j;

void floyd_warshall(int[][] t) {
	int i;
	int j;
	int k;
	for (i = 0 i < n i++) {
		for (j = 0 j < n j++) {
			for (k = 0 k < n k++) {
				if (t[i][j] > t[i][k] + t[k][j]) {
					t[i][j] = t[i][k] + t[k][j]
				}
			}
		}
	}
}

void wypisz(int[][] t) {
	int i;
	int j;
	for (i = 0 i < n i++) {
		for (j = 0 j < n j++) {
			print t[i][j]
		}
	}
}

for (i = 0 i < n i++)
{
	for (j = 0 j < n j++)
	{
		if (i == j) {
			d[i][j] = 0
		}
		else {
			d[i][j] = INF
		}
	}
}

d[0][1] = 5
d[0][2] = 4
d[0][3] = 8
d[1][0] = -4
d[1][2] = -2
d[1][4] = 5
d[2][3] = 5
d[2][4] = 2
d[3][0] = -1
d[3][1] = 2
d[3][4] = -1
d[4][2] = 4
d[4][3] = 2

floyd_warshall(d)
wypisz(d)

