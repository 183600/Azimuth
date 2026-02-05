#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

// 解析命令行参数
const args = process.argv.slice(2);
if (args.length < 1) {
  console.error('Usage: node validate_tests.js <test_file>');
  process.exit(1);
}

const testFile = args[0];

try {
  // 读取测试文件内容
  const testContent = fs.readFileSync(testFile, 'utf8');
  
  // 解析测试函数
  const testRegex = /test\s+"([^"]+)"\s*\{([^}]+)\}/g;
  let match;
  let testCount = 0;
  let passedCount = 0;
  let failedCount = 0;
  
  console.log(`验证测试文件: ${testFile}`);
  console.log("=====================================");
  
  while ((match = testRegex.exec(testContent)) !== null) {
    const testName = match[1];
    const testBody = match[2];
    testCount++;
    
    console.log(`\n检查测试用例: ${testName}`);
    
    try {
      // 检查测试内容的基本语法
      let hasErrors = false;
      
      // 检查是否有断言或检查语句
      const hasAssert = testBody.includes('assert_eq') || testBody.includes('assert_eq_string') || testBody.includes('inspect');
      if (!hasAssert) {
        console.log(`  ⚠️  警告: 测试用例中没有断言或检查语句`);
      }
      
      // 检查括号是否匹配
      let openParens = 0;
      for (let i = 0; i < testBody.length; i++) {
        if (testBody[i] === '(') {
          openParens++;
        } else if (testBody[i] === ')') {
          openParens--;
          if (openParens < 0) {
            console.error(`  ❌ 错误: 括号不匹配`);
            hasErrors = true;
            break;
          }
        }
      }
      if (openParens !== 0) {
        console.error(`  ❌ 错误: 括号不匹配`);
        hasErrors = true;
      }
      
      // 检查函数调用格式
      const functionCalls = testBody.match(/\b(\w+)\s*\(/g);
      if (functionCalls) {
        for (const call of functionCalls) {
          const functionName = call.match(/\b(\w+)\s*\(/)[1];
          // 检查是否是已知的函数
          const knownFunctions = ['add', 'multiply', 'subtract', 'divide_with_ceil', 'greet', 'assert_eq', 'assert_eq_string', 'inspect'];
          if (!knownFunctions.includes(functionName)) {
            console.log(`  ⚠️  警告: 未知函数 ${functionName}`);
          }
        }
      }
      
      // 检查字符串格式
      const stringMatches = testBody.match(/"([^"]*)"/g);
      if (stringMatches) {
        for (const str of stringMatches) {
          if (!str.endsWith('"')) {
            console.error(`  ❌ 错误: 字符串格式不正确`);
            hasErrors = true;
          }
        }
      }
      
      if (!hasErrors) {
        console.log(`  ✅ 测试用例语法正确`);
        passedCount++;
      } else {
        failedCount++;
      }
    } catch (testError) {
      console.error(`  ❌ 错误: ${testError.message}`);
      failedCount++;
    }
  }
  
  console.log("\n=====================================");
  console.log(`测试验证结果: ${passedCount} 通过, ${failedCount} 失败, ${testCount} 总计`);
  
  // 如果有失败的测试，返回非零退出码
  process.exit(failedCount > 0 ? 1 : 0);
  
} catch (error) {
  console.error(`读取测试文件错误 ${testFile}: ${error.message}`);
  process.exit(1);
}