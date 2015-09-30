ghc --make Run_interpreter.hs -o interpreter

for i in `seq 1 9`
do
	echo "Test $i"
	./interpreter good/example_good$i.c > out
	diff -Bb ./good/example_good$i.out out && echo "OK" || break
done
