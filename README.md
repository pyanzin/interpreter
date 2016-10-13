# interpreter
AST interpreter for JavaScript-like language

```javascript
fold = (z, xs, f) => {
    if (xs) {
        fold(f(z, head(xs)), tail(xs), f);
    }
    else {
        z;
    }
};

xs = [12,3,5,6,8];

sum = fold(0, xs, (z, x) => z + x);
production = fold(1, xs, (z, x) => z * x);

writeLine(sum);
writeLine(production);
```
