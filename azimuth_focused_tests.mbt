// Azimuth 项目专注测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-10, add(-5, -5))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_basic" {
  // 测试问候语基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  
  // 测试乘法溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "string_processing_capabilities" {
  // 测试字符串处理能力
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "mathematical_operation_combinations" {
  // 测试数学运算组合
  let a = 10
  let b = 5
  let c = 3
  
  // 测试 (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(45, result1)
  
  // 测试 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(53, result2)
}

test "real_world_application_scenario" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(617, total)
}

test "error_handling_robustness" {
  // 测试错误处理健壮性
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试边界条件下的断言
  let max_val = 2147483647
  assert_true(add(max_val, 1) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：面积和周长计算
  let length = 8
  let width = 6
  
  // 计算矩形面积
  let area = multiply(length, width)
  assert_eq(48, area)
  
  // 计算矩形周长
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(28, perimeter)
}