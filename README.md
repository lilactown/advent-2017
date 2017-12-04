# Advent of Code, 2017

I'll be updating this repository with each day's solution in ReasonML.
It's built using BuckleScript; check out the [ReasonML quick start guide](https://reasonml.github.io/guide/javascript/quickstart) for installing the tool chain if you want to build changes to the project.

Solution source files are in `src/*.re`, the compiled JavaScript is in `src/*.bs.js`.

If you want to just try out the solutions, the build files are in the repo; you can run them by starting up a node REPL:

```
~/Code/advent-2017 $ node
> let day4 = require('./src/dayFour.bs')
undefined
> day4.part1('my test input')
```



```

# Build
```
npm run build
```

# Build + Watch

```
npm run watch
```


# Editor
If you use `vscode`, Press `Windows + Shift + B` it will build automatically
