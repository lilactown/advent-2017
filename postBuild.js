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
    const testPath = `${splitByFolders.slice(0, -1).join("/")}/tests/${moduleName}Test.bs`;
    const testModule = nullableRequire(testPath);
    if (testModule) {
      console.log(`Running tests for ${capitalizedName}...`)
      Object.keys(testModule).forEach(key => {
        // assume `check` fn is first element in the module
        const results = solutionUtils.checkSolution(testModule[key]);
        const pass = results.every((result) => {
          if (solutionUtils.resultToBool(result)) {
            return true;
          }
          const [input, expected, output] = solutionUtils.failureToJs(result);
          throw `${key} :
  For case ${JSON.stringify(input)}
  - got ${JSON.stringify(output)}
  - expected ${JSON.stringify(expected)}`;
        });
        console.log(`${key} : ${pass ? "Pass" : "Fail"} `);
      });
    }
  }
}

runTest(file);