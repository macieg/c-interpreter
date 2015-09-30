int gcd (int a, int b) {
	if (a != 0) {
		return gcd(b%a, a)
	}
	return b
}

print gcd(10, 5)
print gcd(112, 50)
print gcd(124, 15)

//5
//2
//1
