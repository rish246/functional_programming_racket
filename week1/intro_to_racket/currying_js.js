// Currying introduction in javascript and how you can use functional programming in js
const filter = func =>  xs => {
	let xs_new = [];
	for(let val of xs) {
		if(func(val)) 
			xs_new.push(val);
	}
	return xs_new;
}


const filterEven = filter((x) => (x % 2 == 0));

const filterOdd = filter((x) => (x%2 != 0));

let myList = [1, 3, 2, 5, 4, 3, 6, 32];

console.log(filterEven(myList));

console.log(filterOdd(myList));
