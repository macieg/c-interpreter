for i in static_check/*
do
	echo "TEST $i"
	VAR=`./interpreter $i`
	echo $VAR
	if [ -z "$VAR" ]
	then
		echo "Error"
		break
	else
		echo "Ok"
	fi
done
