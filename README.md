ma333856, Maciej Andrearczyk
wymaga użycia bnfc

1. Rozwiązanie uruchamiamy standardowo, wykonując polecenie make.
2. Zaimplementowano interpreter języka imperatywnego bazującego na składni C z paroma małymi wyjątkami
	-po instrukcjach nie jest wymagany średnik,
	-w składni pętli for nie jesy wymagany średnik
3. Najważniejszym plikiem jest Interpreter.hs 
4. Zrealizowane wymagania:
	-funkcje i procedury rekurencyjne w paramatrami przekazywanymi przez wartosc,
	-dwa typy wartosci (int i bool) - na ten moment nie jest sprawdzana poprawnosc typow, wartosci boolowskie rzutpowane są na 0/1
	-arytmetyka
	-while, if, for
	-wbudowana instrukcja print
	-operatory ++ -- += %= itd
	-przesłanianie identyfikatorów ze statycznym wiązaniem
	-jawnie obsłużone dynamiczne błędy wykonania (dzielenie przez 0)
	-dowolne zagnieżdżanie funkcji/procedur z zachowaniem poprawności statycznego wiązanie identyfikatorów
	-statyczne typowanie
	-wielowymiarowe tablice z przypisywaniem, przekazywaniem ich w argumentach funkcji i zwracaniem jako wynik funkcji


bnfc -m -haskell grammar.cf && make && ghc --make Run_interpreter.hs -o interpreter

