// Teste 1
var x = 1, y = 10;
if (x > 1) {
	y;
}

//Teste 2
if (y < 5) {
	x = 4;
} else {
	y = x + y;
}

//Teste 3
while(x < 3) {
	x = x + 1;
}

//Teste 4
while(y < 40) {
	y = y + 1;
	break;
}

//Teste 5
var lista = [2,4,6,8,10];
lista[2];

//Teste 6
lista.length();

//Teste 7
lista.head();

//Teste 8
lista.tail();

//Teste 9
lista.concat(12,14,16);