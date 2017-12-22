const solutionUtils = require('./src/utils/solution.bs');

function nullableRequire(path) {
  try {
    return require(path);
  } catch (e) {
    return null;
  }
}

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

function runTest(file) {
  // const m = require(file);
  const moduleName = longName;
  const capitalizedName = moduleName[0].toUpperCase() + moduleName.slice(1);
    const testPath = `./src/tests/${moduleName}Test.bs`;
    const testModule = require(testPath);
    console.log(testModule);
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

runTest(longName);