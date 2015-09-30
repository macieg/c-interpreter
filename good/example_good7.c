int [] f() {
	int [3] t;
	t[0] = 10
	t[1] = 20
	t[2] = 30
	return t
}

int [3][3] a;
a[1] = f()

print a[1][0]
print a[0][1]
print a[1][2]

//10
//0
//30
