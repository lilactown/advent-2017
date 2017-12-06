#!/usr/local/bin/node
const file = process.argv[2];

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
    if (testModule){
      console.log(`Running tests for ${capitalizedName}...`)
      Object.keys(testModule).forEach(key => {
        const result = testModule[key][0]();
        console.log(`${key} : ${result ? "Pass" : "Fail"}`);
        if (!result) {
          throw `${key} did not pass`;
        }
      });
    }
  }
}

runTest(file);