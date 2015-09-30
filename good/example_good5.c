int x = 1234;

void g() {
	print x
}

void f() {
	int x = 1;
	print x
	g()
}

f()
