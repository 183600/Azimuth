// 核心功能测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(50, 50))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(20, multiply(-4, -5))
  assert_eq(0, multiply(0, 123))
}

test "greet_function_basic_functionality" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, !", greet(""))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  
  // 测试与0相加
  assert_eq(42, add(42, 0))
  assert_eq(-42, add(0, -42))
  assert_eq(0, add(0, 0))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  
  // 测试与1和-1相乘
  assert_eq(42, multiply(42, 1))
  assert_eq(-42, multiply(42, -1))
  assert_eq(0, multiply(0, 123))
  
  // 测试最小值与-1相乘的特殊情况
  assert_eq(min_val, multiply(min_val, -1))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形的面积和周长
  let length = 10
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(50, area)
  assert_eq(30, perimeter)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced  "))
  assert_eq_string("Hello, 新年快乐!", greet("新年快乐"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let tax_rate = 8
  
  let subtotal = multiply(item_price, quantity)
  let tax = multiply(subtotal, tax_rate) / 100
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(674, total)
}

test "performance_and_stress_tests" {
  // 测试性能和压力情况
  // 大数计算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  assert_eq(3000000, add(large_num1, large_num2))
  assert_eq(2000000000000, multiply(large_num1, large_num2))
  
  // 连续计算
  let result = add(multiply(100, 100), multiply(200, 200))
  assert_eq(50000, result)
}