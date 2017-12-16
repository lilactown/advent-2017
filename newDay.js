const sh = require('shelljs');

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
console.log(` ** Creating new solution in ${srcDir}/${longName}.re`);
sh.mkdir(srcDir);
sh.cd(srcDir);
sh.touch(`${longName}.re`);
sh.echo(`module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("", 5)];
  let solve = (input) => 6;
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("", 5)];
  let solve = (input) => 6;
};

let part1 = Part1.solve;

let part2 = Part2.solve;`).to(`${longName}.re`);
console.log(`\n ** Creating tests in tests/${longName}Test.re`)
sh.cd("../tests");
sh.touch(`${longName}Tests.re`);
sh.echo(`module Part1Test = Solution.Make(${longNameCapitalize}.Part1);

module Part2Test = Solution.Make(${longNameCapitalize}.Part2);`).to(`${longName}Tests.re`);
// edit bsconfig

console.log('\n ** Updating bsconfig.json');
const bsconfig = require('./bsconfig.json');
bsconfig.sources.push(`src/day${name}`);

sh.cd('../../');
sh.echo(JSON.stringify(bsconfig, null, 2)).to('./bsconfig.json');
console.log('\n ** 🎉 We\'re done')