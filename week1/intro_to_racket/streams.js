function powerOfTwo () {

    function f(x) {
        return [x, () => f(2 * x)];
    }

    return f(2);
}


function evalStream(stream, n) {
    const ans = []

    for(let i=0; i<n; i++) {
        ans.push(stream()[0]);
        stream = stream()[1];
    }

    return ans;
}


let ans = evalStream(powerOfTwo, 5); // should print the first 5 powers of 2
console.log(ans);