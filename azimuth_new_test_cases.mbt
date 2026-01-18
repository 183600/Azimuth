// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能的多样化场景

// 导入必要的模块
use azimuth

test "add_function_basic_cases" {
  // 测试 add 函数的基本用例
  @assertion.assert_eq(5, azimuth.add(2, 3))?
  @assertion.assert_eq(0, azimuth.add(0, 0))?
  @assertion.assert_eq(-1, azimuth.add(2, -3))?
  @assertion.assert_eq(100, azimuth.add(50, 50))?
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界用例
  @assertion.assert_eq(2147483647, azimuth.add(2147483647, 0))?
  @assertion.assert_eq(-2147483648, azimuth.add(-2147483648, 0))?
  @assertion.assert_eq(1, azimuth.add(2147483647, -2147483646))?
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本用例
  @assertion.assert_eq(6, azimuth.multiply(2, 3))?
  @assertion.assert_eq(0, azimuth.multiply(5, 0))?
  @assertion.assert_eq(-15, azimuth.multiply(3, -5))?
  @assertion.assert_eq(100, azimuth.multiply(10, 10))?
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界用例
  @assertion.assert_eq(2147483647, azimuth.multiply(2147483647, 1))?
  @assertion.assert_eq(-2147483648, azimuth.multiply(-2147483648, 1))?
  @assertion.assert_eq(0, azimuth.multiply(0, 2147483647))?
}

test "greet_function_basic_cases" {
  // 测试 greet 函数的基本用例
  @assertion.assert_eq_string("Hello, Alice!", azimuth.greet("Alice"))?
  @assertion.assert_eq_string("Hello, Bob!", azimuth.greet("Bob"))?
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
}

test "greet_function_special_characters" {
  // 测试 greet 函数的特殊字符处理
  @assertion.assert_eq_string("Hello, John Doe!", azimuth.greet("John Doe"))?
  @assertion.assert_eq_string("Hello, 用户123!", azimuth.greet("用户123"))?
  @assertion.assert_eq_string("Hello, @#$%!", azimuth.greet("@#$%"))?
}

test "mathematical_combinations" {
  // 测试数学函数组合使用
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let result1 = azimuth.multiply(azimuth.add(a, b), c)
  @assertion.assert_eq(45, result1)?
  
  // a * b + c
  let result2 = azimuth.add(azimuth.multiply(a, b), c)
  @assertion.assert_eq(53, result2)?
}

test "real_world_scenarios" {
  // 测试实际应用场景
  let price_per_item = 25
  let quantity = 4
  let tax_rate = 8
  let shipping = 10
  
  // 计算商品总价
  let subtotal = azimuth.multiply(price_per_item, quantity)
  @assertion.assert_eq(100, subtotal)?
  
  // 计算税费
  let tax = azimuth.multiply(subtotal, tax_rate) / 100
  @assertion.assert_eq(8, tax)?
  
  // 计算最终总价
  let total = azimuth.add(azimuth.add(subtotal, tax), shipping)
  @assertion.assert_eq(118, total)?
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  // 测试大数相加是否保持稳定
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_sum = azimuth.add(large_num1, large_num2)
  @assertion.assert_eq(3000000, large_sum)?
  
  // 测试负数乘法
  let negative_result = azimuth.multiply(-100, 5)
  @assertion.assert_eq(-500, negative_result)?
}

test "performance_patterns" {
  // 测试性能模式
  let base = 7
  
  // 重复加法 vs 乘法
  let repeated_add = azimuth.add(azimuth.add(azimuth.add(base, base), base), base)
  let multiply_result = azimuth.multiply(base, 4)
  @assertion.assert_eq(repeated_add, multiply_result)?
  
  // 嵌套运算
  let nested1 = azimuth.multiply(azimuth.add(2, 3), azimuth.add(4, 5))
  @assertion.assert_eq(35, nested1)?
}