// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(0, 10))
  assert_eq(10, add(10, 0))
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
  
  // 测试最小值特殊情况
  assert_eq(min_val, add(min_val, 0))
  assert_eq(-2147483647, add(min_val, 1))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
}

test "multiply_function_overflow_protection" {
  // 测试 multiply 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试乘法溢出
  assert_eq(max_val, multiply(46341, 46341))  // sqrt(max_val)的平方
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 测试最小值乘法特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算矩形周长
  let length = 10
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(30, perimeter)
  
  // 场景2：计算折扣价格
  let price = 100
  let discount = 20
  let final_price = add(price, -discount)
  assert_eq(80, final_price)
  
  // 场景3：计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let average = add(add(score1, score2), score3) / 3
  assert_eq(90, average)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "edge_case_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 连续运算的边界情况
  let result1 = add(add(max_val, -10), 5)
  assert_eq(2147483642, result1)
  
  let result2 = multiply(multiply(min_val, 1), 1)
  assert_eq(min_val, result2)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let cart_total = add(total1, total2)
  assert_eq(697, cart_total)
  
  // 场景2：温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 场景3：简单贷款计算
  let principal = 10000
  let rate = 5
  let years = 3
  let total = multiply(principal, add(1, multiply(rate, years)))
  assert_eq(160000, total)
}

test "assert_functions_functionality" {
  // 测试断言函数的功能
  // 测试 assert_true 和 assert_false
  assert_true(true)
  assert_false(false)
  assert_true(5 > 3)
  assert_false(5 < 3)
  
  // 测试 assert_eq
  assert_eq(42, 42)
  assert_eq(-10, -10)
  assert_eq(0, 0)
  
  // 测试 assert_eq_string
  assert_eq_string("hello", "hello")
  assert_eq_string("", "")
  assert_eq_string("Hello, World!", greet("World"))
}