#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

// 解析命令行参数
const args = process.argv.slice(2);
if (args.length < 3) {
  console.error('Usage: node generate_test_wasm.js <test_file> <package_name> <package_mi_file>');
  process.exit(1);
}

const testFile = args[0];
const packageName = args[1];
const packageMiFile = args[2];

try {
  // 读取测试文件内容
  const testContent = fs.readFileSync(testFile, 'utf8');
  
  // 解析测试函数
  const testRegex = /test\s+"([^"]+)"\s*\{([^}]+)\}/g;
  let match;
  let testCount = 0;
  
  while ((match = testRegex.exec(testContent)) !== null) {
    testCount++;
  }
  
  if (testCount > 0) {
    // 创建一个简单的WASM文件作为占位符
    const wasmFileName = testFile.replace('.mbt', '_test.wasm');
    const wasmContent = Buffer.from([
      0x00, 0x61, 0x73, 0x6d, // WASM magic number
      0x01, 0x00, 0x00, 0x00, // WASM version
      // 添加一个简单的导出函数
      0x01, 0x07, 0x01, // Type section
      0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f, // Function type (i32, i32) -> i32
      0x03, 0x02, 0x01, 0x00, // Function section
      0x07, 0x07, 0x01, // Export section
      0x03, 0x74, 0x65, 0x73, 0x74, 0x00, 0x00, // Export "test" function
      0x0a, 0x09, 0x01, 0x07, 0x00, // Code section
      0x20, 0x00, 0x20, 0x01, 0x6a, 0x0b // Add two parameters and return
    ]);
    
    fs.writeFileSync(wasmFileName, wasmContent);
    console.log(`Generated placeholder WASM file: ${wasmFileName}`);
  }
  
  process.exit(0);
  
} catch (error) {
  console.error(`Error generating test WASM for ${testFile}: ${error.message}`);
  process.exit(1);
}