int x = 1;

void f() {
	print 1
	g()
}

void g() {
	print 2
	if (x < 5) {
		x++
		g()
	}
}

f()
