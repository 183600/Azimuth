// 综合测试套件 - 标准MoonBit测试用例
// 测试核心功能：add, multiply, greet, divide_with_ceil

test "add_function_basic_functionality" {
  // 测试add函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(50, 50))
}

test "add_function_overflow_protection" {
  // 测试add函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试multiply函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(20, multiply(-4, -5))
  assert_eq(0, multiply(0, 100))
}

test "multiply_function_edge_cases" {
  // 测试multiply函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试乘以1和-1
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_function_comprehensive" {
  // 测试greet函数的各种情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "divide_with_ceil_function" {
  // 测试divide_with_ceil函数
  // 正常除法（无需向上取整）
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(3, divide_with_ceil(9, 3))
  
  // 需要向上取整的情况
  assert_eq(6, divide_with_ceil(11, 2))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(8, 3))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 计算面积
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 计算周长
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(26, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：简单利息计算
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  // 计算简单利息：本金 × 利率 × 时间
  let interest = multiply(principal, multiply(rate, time))
  // 计算总金额：本金 + 利息
  let total = add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a × (b + c) = a × b + a × c
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "error_handling_and_boundary_conditions" {
  // 测试错误处理和边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试边界值运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试特殊除法情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
}