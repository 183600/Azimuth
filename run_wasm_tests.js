const fs = require('fs');
const path = require('path');

async function runWasmTest(wasmPath, testName) {
  try {
    const wasmBuffer = fs.readFileSync(wasmPath);
    const wasmModule = await WebAssembly.compile(wasmBuffer);
    const instance = await WebAssembly.instantiate(wasmModule);
    
    console.log(`Running ${testName}...`);
    
    // 检查是否有导出的函数
    const exports = instance.exports;
    const exportedFunctions = Object.keys(exports).filter(key => typeof exports[key] === 'function');
    
    console.log(`Exported functions: ${exportedFunctions.join(', ')}`);
    
    // 尝试运行一些基本函数
    if (exports.add) {
      console.log(`add(2, 3) = ${exports.add(2, 3)}`);
    }
    if (exports.multiply) {
      console.log(`multiply(2, 3) = ${exports.multiply(2, 3)}`);
    }
    
    return true;
  } catch (error) {
    console.error(`Error running ${testName}:`, error);
    return false;
  }
}

async function main() {
  const projectRoot = '/home/runner/work/Azimuth/Azimuth';
  
  // 测试 azimuth 包
  const azimuthWasm = path.join(projectRoot, 'src/azimuth/azimuth.wasm');
  if (fs.existsSync(azimuthWasm)) {
    await runWasmTest(azimuthWasm, 'azimuth');
  } else {
    console.log('azimuth.wasm not found');
  }
  
  // 测试 clean_test 包
  const cleanTestWasm = path.join(projectRoot, 'clean_test/clean_test.wasm');
  if (fs.existsSync(cleanTestWasm)) {
    await runWasmTest(cleanTestWasm, 'clean_test');
  } else {
    console.log('clean_test.wasm not found');
  }
  
  // 测试测试文件
  const azimuthTestWasm = path.join(projectRoot, 'src/azimuth/test/simple_test.wasm');
  if (fs.existsSync(azimuthTestWasm)) {
    await runWasmTest(azimuthTestWasm, 'azimuth_test');
  } else {
    console.log('azimuth_test.wasm not found');
  }
}

main().catch(console.error);