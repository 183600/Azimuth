// Azimuth 项目综合测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

// 导入主模块中的函数
use azimuth

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(10, add(10, 0))
  assert_eq(-7, add(-3, -4))
}

test "add_function_overflow_protection" {
  // 测试 add 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  
  // 测试混合情况
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本情况
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
}

test "multiply_function_special_values" {
  // 测试 multiply 函数的特殊值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0的乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试与1的乘法
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 测试与-1的乘法
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(-max_val, multiply(max_val, -1))
}

test "multiply_function_overflow_protection" {
  // 测试 multiply 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, multiply(46341, 46341))
  
  // 测试负数溢出
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 测试最小值乘法
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -2))
}

test "greet_function_standard_cases" {
  // 测试 greet 函数的标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, Test!", greet("Test"))
}

test "greet_function_edge_cases" {
  // 测试 greet 函数的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let a = 10
  let b = 20
  let c = 5
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(150, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(205, result2)
  
  // (a + c) * (b - c)
  let result3 = multiply(add(a, c), add(b, -c))
  assert_eq(225, result3)
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  // 计算购物总价：单价 * 数量
  let unit_price = 25
  let quantity = 8
  let total_price = multiply(unit_price, quantity)
  assert_eq(200, total_price)
  
  // 计算总分数：各科分数相加
  let math_score = 85
  let english_score = 92
  let science_score = 78
  let total_score = add(add(math_score, english_score), science_score)
  assert_eq(255, total_score)
  
  // 计算折扣后价格：原价 - 折扣金额
  let original_price = 500
  let discount_percentage = 10
  let discount_amount = multiply(original_price, discount_percentage) / 100
  let discounted_price = add(original_price, -discount_amount)
  assert_eq(450, discounted_price)
}