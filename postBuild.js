#!/usr/local/bin/node
const file = process.argv[2];

function runTest(file) {
  const m = require(file);
  const fileName = file.split("/").slice(-1)[0].split(".")[0];
  console.log(`Running tests in ${fileName}`);
  if (m.test_part1)
    console.log(`\tPart 1: ${m.test_part1() ? "Pass" : "Fail"}`);

  if (m.test_part2)
    console.log(`\tPart 2: ${m.test_part2() ? "Pass" : "Fail"}`);
}

runTest(file);