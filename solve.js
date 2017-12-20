const dateMap = {
  "14": "Fourteen",
  "15": "Fifteen",
  "16": "Sixteen",
  "17": "Seventeen",
  "18": "Eighteen",
  "19": "Nineteen",
  "20": "Twenty",
  "21": "TwentyOne",
  "22": "TwentyTwo",
  "23": "TwentyThree",
  "24": "TwentyFour",
  "25": "TwentyFive",
  "26": "TwentySix",
  "27": "TwentySeven",
  "28": "TwentyEight",
  "29": "TwentyNine",
  "30": "Thirty",
  "31": "ThirtyOne",
};

let name = process.argv[2];
let longName = `day${dateMap[name]}`;
let srcDir = `./src/day${name}`;
let longNameCapitalize = `Day${dateMap[name]}`;

function processInput(args) {
  if (args[0] === "--int") {
    return parseInt(args[1]);
  } else if (args[0] === "--file") {
    return require(`${srcDir}/${args[1]}`);
  } else {
    return args[0];
  }
}

if (process.argv[3] === "part1") {
  let input = processInput(process.argv.slice(4));
  console.log(require(`${srcDir}/${longName}.bs`).part1(input));
} else if (process.argv[3] === "part2") {
  let input = processInput(process.argv.slice(4));
  console.log(require(`${srcDir}/${longName}.bs`).part2(input));
} else {
  let input = processInput(process.argv.slice(3));
  let { part1, part2 } = require(`${srcDir}/${longName}.bs`);
  console.log(`Part 1: ${part1(input)}`);
  console.log(`Part 2: ${part2(input)}`);
}
