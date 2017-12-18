# Advent of Code, 2017

I'll be updating this repository with each day's solution in ReasonML.
It's built using BuckleScript; check out the [ReasonML quick start guide](https://reasonml.github.io/guide/javascript/quickstart) for installing the tool chain if you want to build changes to the project.

Solution source files are in `src/*.re`. Common utilities and data structures are found in `src/utils/*.re`.

If you want to just try out the solutions, you can build the project (`yarn build`) and run them with the command:
```
yarn solve day4 part1 "my test input"
```

Or, by starting up a node REPL:
```
~/Code/advent-2017 $ node
> let day4 = require('./src/dayFour.bs')
undefined
> day4.part1('my test input')
```

Tests are located in the `src/tests` directory; they are automatically generated using the `Solution` functor and are based on the `cases` binding found in each solution module. The tests run automatically on compiling.

# Editor
If you use `vscode`, Press `Windows + Shift + B` it will build automatically
