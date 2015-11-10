# interpreter
AST interpreter for JavaScript-like language

```javascript
map = function(xs, f) = {
	if (len(xs)) {
		cons(f(head(xs)), map(tail(xs), f));
	} else {
		[];
	}
};

foreach = function(xs, f) = {
	if (xs) {
		f(head(xs));
		foreach(tail(xs), f);
	}
};

xs = [1,2,3,4,5];

ys = map(xs, str);

foreach(ys, writeLine);
```
