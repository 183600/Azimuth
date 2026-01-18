// Azimuth 项目额外综合测试用例
// 使用标准 MoonBit 测试语法，覆盖项目主要功能

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(10, -10))
}

test "add_function_overflow_protection" {
  // 测试 add 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试最大值与最小值相加
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_function_overflow_protection" {
  // 测试 multiply 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试乘法溢出
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 测试最小值与-1相乘的特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试边界值乘法
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced  "))
}

test "assert_functions_functionality" {
  // 测试断言函数的功能
  // 测试 assert_true
  assert_true(true)
  assert_true(5 > 3)
  assert_true(0 == 0)
  
  // 测试 assert_false
  assert_false(false)
  assert_false(5 < 3)
  assert_false(1 == 0)
  
  // 测试 assert_eq
  assert_eq(42, 42)
  assert_eq(0, 0)
  assert_eq(-5, -5)
  
  // 测试 assert_eq_string
  assert_eq_string("hello", "hello")
  assert_eq_string("", "")
  assert_eq_string("测试", "测试")
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算矩形的面积和周长
  let length = 8
  let width = 5
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(40, area)
  assert_eq(26, perimeter)
  
  // 场景2：计算简单的利息
  let principal = 1000
  let rate = 5
  let years = 3
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(16000, total)
}

test "edge_case_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 组合1：最大值与各种值运算
  assert_eq(max_val, add(max_val, zero))
  assert_eq(max_val, multiply(max_val, one))
  assert_eq(min_val, multiply(max_val, neg_one))
  
  // 组合2：最小值与各种值运算
  assert_eq(min_val, add(min_val, zero))
  assert_eq(min_val, multiply(min_val, one))
  assert_eq(max_val, multiply(min_val, neg_one))
  
  // 组合3：零的运算特性
  assert_eq(zero, multiply(zero, max_val))
  assert_eq(zero, multiply(zero, min_val))
  assert_eq(max_val, add(zero, max_val))
  assert_eq(min_val, add(zero, min_val))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 7
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let subtotal = add(total1, total2)
  let final_total = add(subtotal, shipping)
  
  assert_eq(717, final_total)
  
  // 场景2：温度转换（摄氏度转华氏度，简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 场景3：计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let average = add(add(score1, score2), score3) / 3
  assert_eq(90, average)
}

test "divide_with_ceil_function" {
  // 测试 divide_with_ceil 函数
  // 正常除法（无余数）
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(3, divide_with_ceil(9, 3))
  
  // 需要向上取整的除法
  assert_eq(6, divide_with_ceil(11, 2))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(8, 3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 负数情况
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-3, divide_with_ceil(-11, 4))
}