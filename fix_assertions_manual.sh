#!/bin/bash

# 精确修复所有测试文件中的断言问题
echo "Precisely fixing assertion issues in all test files..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 修复 azimuth 测试文件
echo "Fixing azimuth test files..."
cd "$AZIMUTH_TEST_PATH"

# 手动修复 lib_test.mbt 作为示例
cat > lib_test.mbt << 'EOF'
test "lib_add_comprehensive" {
  // 基础功能测试
  let result1 = @azimuth.add(7, 3)
  if result1 != 10 { @test.fail("Expected 10 but got " + result1.to_string()) }
  
  let result2 = @azimuth.add(5, -5)
  if result2 != 0 { @test.fail("Expected 0 but got " + result2.to_string()) }
  
  let result3 = @azimuth.add(-7, -3)
  if result3 != -10 { @test.fail("Expected -10 but got " + result3.to_string()) }
  
  // 零值测试
  let result4 = @azimuth.add(0, 7)
  if result4 != 7 { @test.fail("Expected 7 but got " + result4.to_string()) }
  
  let result5 = @azimuth.add(-3, 0)
  if result5 != -3 { @test.fail("Expected -3 but got " + result5.to_string()) }
  
  let result6 = @azimuth.add(0, 0)
  if result6 != 0 { @test.fail("Expected 0 but got " + result6.to_string()) }
}

///|
test "lib_multiply_comprehensive" {
  // 基础功能测试
  let result1 = @azimuth.multiply(7, 3)
  if result1 != 21 { @test.fail("Expected 21 but got " + result1.to_string()) }
  
  let result2 = @azimuth.multiply(7, -3)
  if result2 != -21 { @test.fail("Expected -21 but got " + result2.to_string()) }
  
  let result3 = @azimuth.multiply(-7, -3)
  if result3 != 21 { @test.fail("Expected 21 but got " + result3.to_string()) }
  
  // 特殊值测试
  let result4 = @azimuth.multiply(0, 7)
  if result4 != 0 { @test.fail("Expected 0 but got " + result4.to_string()) }
  
  let result5 = @azimuth.multiply(1, 7)
  if result5 != 7 { @test.fail("Expected 7 but got " + result5.to_string()) }
  
  let result6 = @azimuth.multiply(-1, 7)
  if result6 != -7 { @test.fail("Expected -7 but got " + result6.to_string()) }
}

///|
test "lib_greet_comprehensive" {
  // 基础功能测试
  let result1 = @azimuth.greet("Azimuth")
  if result1 != "Hello, Azimuth!" { @test.fail("Expected \"Hello, Azimuth!\" but got \"" + result1 + "\"") }
  
  let result2 = @azimuth.greet("")
  if result2 != "Hello, !" { @test.fail("Expected \"Hello, !\" but got \"" + result2 + "\"") }
  
  // 特殊字符测试
  let result3 = @azimuth.greet("123")
  if result3 != "Hello, 123!" { @test.fail("Expected \"Hello, 123!\" but got \"" + result3 + "\"") }
  
  let result4 = @azimuth.greet("test_case")
  if result4 != "Hello, test_case!" { @test.fail("Expected \"Hello, test_case!\" but got \"" + result4 + "\"") }
}
EOF

echo "Fixed lib_test.mbt manually"

# 修复 clean_test 测试文件
echo "Fixing clean_test test files..."
cd "$CLEAN_TEST_PATH"

# 手动修复 lib_test.mbt 作为示例
cat > lib_test.mbt << 'EOF'
test "basic_math_operations" {
  let a = 10
  let b = 20
  
  let sum = @clean_test.add(a, b)
  if sum != 30 { @test.fail("Expected 30 but got " + sum.to_string()) }
  
  let product = @clean_test.multiply(a, b)
  if product != 200 { @test.fail("Expected 200 but got " + product.to_string()) }
}

// 测试用例 2: 字符串处理
///|
test "string_processing" {
  let name1 = "Alice"
  let name2 = "Bob"
  
  let greeting1 = @clean_test.greet(name1)
  if greeting1 != "Hello, Alice!" { @test.fail("Expected \"Hello, Alice!\" but got \"" + greeting1 + "\"") }
  
  let greeting2 = @clean_test.greet(name2)
  if greeting2 != "Hello, Bob!" { @test.fail("Expected \"Hello, Bob!\" but got \"" + greeting2 + "\"") }
}

// 测试用例 3: 边界值测试
///|
test "boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  let result1 = @clean_test.add(max_val, 0)
  if result1 != max_val { @test.fail("Expected " + max_val.to_string() + " but got " + result1.to_string()) }
  
  let result2 = @clean_test.add(min_val, 0)
  if result2 != min_val { @test.fail("Expected " + min_val.to_string() + " but got " + result2.to_string()) }
}

// 测试用例 4: 负数运算
///|
test "negative_operations" {
  let a = -10
  let b = -20
  
  let sum = @clean_test.add(a, b)
  if sum != -30 { @test.fail("Expected -30 but got " + sum.to_string()) }
  
  let product = @clean_test.multiply(a, b)
  if product != 200 { @test.fail("Expected 200 but got " + product.to_string()) }
}

// 测试用例 5: 复合运算
///|
test "compound_operations" {
  let base = 5
  
  let result1 = @clean_test.multiply(base, 2)
  if result1 != 10 { @test.fail("Expected 10 but got " + result1.to_string()) }
  
  let result2 = @clean_test.add(result1, 10)
  if result2 != 20 { @test.fail("Expected 20 but got " + result2.to_string()) }
  
  let result3 = @clean_test.multiply(result2, 3)
  if result3 != 60 { @test.fail("Expected 60 but got " + result3.to_string()) }
}
EOF

echo "Fixed clean_test/lib_test.mbt manually"
echo "Manual fixes completed!"