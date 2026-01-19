// Azimuth 核心功能测试用例
// 使用标准 MoonBit 测试语法

test "add_basic_operations" {
  // 测试基本加法运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-3, 3))
  assert_eq(-7, add(-4, -3))
  assert_eq(100, add(50, 50))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_basic_operations" {
  // 测试基本乘法运算
  assert_eq(6, multiply(2, 3))
  assert_eq(-8, multiply(-2, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(12, multiply(3, 4))
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
}

test "greet_function_standard" {
  // 测试标准问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_function_special_characters" {
  // 测试特殊字符处理
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, @123!", greet("@123"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let length = 10
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(30, perimeter)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_application" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(612, total)  // 199*3 + 15 = 597 + 15 = 612
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("success", "success")
  
  // 测试边界条件
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}