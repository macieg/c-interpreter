int gcd (int a, int b) {
	print a
	print b
	if (a != 0) {
		return gcd(b%a, a)
	}
	return b
}

int x = 17;

int f() {
	if (x != 0) {
		print x
		x /= 2
		f()
	}
	return 12345
}

void g(int a, int b) {
	print 0
	print a
	print b
	if (a > 0) {
		g(a/2, b+a)
	}
}

print g()

//g(20, 10)
