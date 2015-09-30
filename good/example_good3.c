int x = 1234;

void f() {
	int x = 10;
	x = 1
}

bool checkCond(int x) {
	if (x % 2 == 0) {
		return true
	}
	return false
}

int g(int x) {
		int res = 0;
		while (x > 0) {
			if (checkCond(x)) {
				res += x / 2
			} else if (x % 3 == 1) {
				res++
			} else {
				--res
			}
			x /= 2
		}
		
	return res
}

print x
f()
print x

//1234
//1234

print g(10)

//6
