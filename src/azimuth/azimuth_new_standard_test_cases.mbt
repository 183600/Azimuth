// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法
// 测试 lib.mbt 中的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  let result1 = add(1, 2)
  let result2 = add(-1, 1)
  let result3 = add(0, 5)
  let result4 = add(5, 0)
  let result5 = add(-3, -2)
  
  // 验证结果
  assert_eq(3, result1)
  assert_eq(0, result2)
  assert_eq(5, result3)
  assert_eq(5, result4)
  assert_eq(-5, result5)
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  let result1 = add(max_val, 1)
  let result2 = add(max_val/2, max_val/2 + 1)
  
  // 测试负数溢出
  let result3 = add(min_val, -1)
  let result4 = add(min_val/2, min_val/2 - 1)
  
  // 测试最小值特殊情况
  let result5 = add(min_val, 0)
  let result6 = add(0, min_val)
  let result7 = add(min_val, 1)
  let result8 = add(1, min_val)
  
  // 验证溢出保护
  assert_eq(max_val, result1)
  assert_eq(max_val, result2)
  assert_eq(min_val, result3)
  assert_eq(min_val, result4)
  assert_eq(min_val, result5)
  assert_eq(min_val, result6)
  assert_eq(min_val + 1, result7)
  assert_eq(min_val + 1, result8)
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  let result1 = multiply(2, 3)
  let result2 = multiply(-2, 3)
  let result3 = multiply(2, -3)
  let result4 = multiply(-2, -3)
  let result5 = multiply(0, 5)
  let result6 = multiply(5, 0)
  
  // 验证结果
  assert_eq(6, result1)
  assert_eq(-6, result2)
  assert_eq(-6, result3)
  assert_eq(6, result4)
  assert_eq(0, result5)
  assert_eq(0, result6)
}

test "multiply_edge_cases" {
  // 测试乘法边界情况
  let result1 = multiply(1, 5)
  let result2 = multiply(5, 1)
  let result3 = multiply(-1, 5)
  let result4 = multiply(5, -1)
  let result5 = multiply(-1, -5)
  
  // 验证结果
  assert_eq(5, result1)
  assert_eq(5, result2)
  assert_eq(-5, result3)
  assert_eq(-5, result4)
  assert_eq(5, result5)
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  let result1 = multiply(max_val, 2)
  let result2 = multiply(46341, 46341)  // sqrt(max_val) + 1
  
  // 测试负数溢出
  let result3 = multiply(min_val, 2)
  let result4 = multiply(min_val, -2)
  
  // 测试最小值特殊情况
  let result5 = multiply(min_val, 1)
  let result6 = multiply(1, min_val)
  let result7 = multiply(min_val, -1)
  let result8 = multiply(-1, min_val)
  
  // 验证溢出保护
  assert_eq(max_val, result1)
  assert_eq(max_val, result2)
  assert_eq(min_val, result3)
  assert_eq(max_val, result4)
  assert_eq(min_val, result5)
  assert_eq(min_val, result6)
  assert_eq(min_val, result7)
  assert_eq(min_val, result8)
}

test "greet_functionality" {
  // 测试 greet 函数
  let result1 = greet("World")
  let result2 = greet("MoonBit")
  let result3 = greet("")
  let result4 = greet("123")
  
  // 验证字符串拼接
  assert_eq_string("Hello, World!", result1)
  assert_eq_string("Hello, MoonBit!", result2)
  assert_eq_string("Hello, !", result3)
  assert_eq_string("Hello, 123!", result4)
}

test "assert_functions" {
  // 测试断言函数
  let int_val = 42
  let string_val = "test"
  let bool_true = true
  let bool_false = false
  
  // 测试成功断言（不会抛出异常）
  assert_eq(42, int_val)
  assert_eq_string("test", string_val)
  assert_true(bool_true)
  assert_false(bool_false)
}

test "complex_operations" {
  // 测试复合操作
  let a = 10
  let b = 20
  let c = -5
  
  // 复合加法运算
  let sum1 = add(a, b)
  let sum2 = add(sum1, c)
  let sum3 = add(add(a, b), add(c, a))
  
  // 复合乘法运算
  let prod1 = multiply(a, b)
  let prod2 = multiply(prod1, 2)
  let prod3 = multiply(multiply(a, 2), multiply(b, 3))
  
  // 混合运算
  let mixed = add(multiply(a, 2), multiply(b, 3))
  
  // 验证结果
  assert_eq(30, sum1)
  assert_eq(25, sum2)
  assert_eq(35, sum3)
  assert_eq(200, prod1)
  assert_eq(400, prod2)
  assert_eq(360, prod3)
  assert_eq(80, mixed)
  
  // 测试字符串与数字结合
  let greeting = greet("Result")
  let final_result = add(a, multiply(b, c))
}