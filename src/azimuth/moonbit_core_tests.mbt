// MoonBit 核心功能测试用例
// 测试 azimuth 包的核心功能：add、multiply 和 greet 函数

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(0, 12345))
}

test "multiply_special_values" {
  // 测试特殊值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_standard_cases" {
  // 测试标准的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形周长
  let length = 8
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(26, perimeter)
}

test "mathematical_properties" {
  // 测试数学性质：交换律
  let a = 12
  let b = 18
  
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "practical_application" {
  // 测试实际应用：购物车计算
  let price = 199
  let quantity = 3
  let shipping = 15
  let total = add(multiply(price, quantity), shipping)
  assert_eq(612, total)
}

test "error_handling_boundary" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(add(max_val, 100) == max_val)
  assert_true(add(min_val, -100) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
}