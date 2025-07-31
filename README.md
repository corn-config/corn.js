# Corn.JS

Native Typescript implementation of the Corn parser.

This is compliant with the (currently unfinished) v0.11 spec.

>![NOTE]
> Not yet published to NPM.

Usage:

```ts
import { parse } from '<TBD>';

const corn = "{ let $foo = 42 } in { value = $foo }";
const res = parse(corn);

if(res.ok) console.log(res.value); // { value: 42 }
```
