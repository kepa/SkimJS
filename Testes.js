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
var x;
function func(a, b){
	if(a.len() > b.len()){
		x = a.concat(b);
	} else {
		x = a.tail();
	}
}
func([1,2,3,4,5,6,7], [9,8,7,6,5,4,3,2,1]);
func([9,8,7,6,5,4,3,2,1], [1,2,3,4,5,6,7]);

//Teste 6
function sum(ls){
	var i = 0;
	var val = 0;
	while(i < ls.len()){
		val = val + ls[i];
		i = i + 1;
	}
	return val;
}
sum([2,4,6,8,10,12]);

//Teste 7
var i = 0;
var last;
var ls = [4,8,15,16,23,42];
for(; i < ls.len(); i = i + 1){
	last = ls[i];
}
last;

//Teste 8
var ls = [4,8,15,16,23,42];
var i = ls.len() - 1;
var first;
for(; i >= 0; i = i - 1){
	first = ls[i];
}
first;

//Teste 9
function isPrime(x) {
	var count = 0;
	var i = 1;
	for ( ; i <= x; i = i + 1){
		if (x % i == 0){ 
			count = count + 1;
		}
	}
	if(count == 2){
		return true;
	}
	return false;
}

isPrime(11);

//Teste 10
function equals(a, b) {
	var output = true;
	if(a.len() != b.len()){
		output = false;
	}
	var i = 0;
	while(i < a.len()){
		if(a[i] != b[i]){
			output = false;
		}
		i = i + 1;
	}
	return output;
}

//equals([1,2,3,4,5], [1,2,3,4,5]);
equals([1,2,3,4,5], [1,2,3,4,4]);