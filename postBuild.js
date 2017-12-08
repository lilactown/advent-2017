#!/usr/local/bin/node
const file = process.argv[2];
const solutionUtils = require('./src/solution.bs');

function nullableRequire(path) {
  try {
    return require(path);
  } catch (e) {
    return null;
  }
}

function runTest(file) {
  // const m = require(file);
  const splitByFolders = file.split("/");
  const folder = splitByFolders.slice(-2)[0];
  const moduleName = splitByFolders.slice(-1)[0].split(".")[0];
  const capitalizedName = moduleName[0].toUpperCase() + moduleName.slice(1);
  if (folder !== "tests") { // ignore tests dir
    const srcIndex = splitByFolders.findIndex((v) => v === "src") + 1;
    console.log(splitByFolders.slice(0, srcIndex));
    const testPath = `${splitByFolders.slice(0, srcIndex).join("/")}/tests/${moduleName}Test.bs`;
    const testModule = nullableRequire(testPath);
    if (testModule) {
      console.log(`-----------------------------------------`);
      console.log(`Running tests for ${capitalizedName}...`)
      Object.keys(testModule).forEach(key => {
        // assume `check` fn is first element in the module
        const results = solutionUtils.checkSolution(testModule[key]);
        const pass = results.every((result) => {
          if (solutionUtils.resultToBool(result)) {
            return true;
          }
          const [input, expected, output] = solutionUtils.failureToJs(result);
          console.log(`${key} : Fail
    For case ${JSON.stringify(input)}
    - got ${JSON.stringify(output)}
    - expected ${JSON.stringify(expected)}
-----------------------------------------`)
          throw new Error("Test case failed");
        });
        console.log(`${key} : Pass`);
      });
      console.log(`-----------------------------------------`);
    }
  }
}

runTest(file);