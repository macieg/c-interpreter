int x = 1234;

void g(int y) {
	print x
}

void f(int y) {
	int x = 1;
	print x
	g(y)
}

f(12)
