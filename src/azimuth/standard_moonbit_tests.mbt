// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-1, -1))
  assert_eq(100, add(50, 50))
}

test "add_with_negative_numbers" {
  // 测试包含负数的加法
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-15, 5))
  assert_eq(10, add(15, -5))
  assert_eq(-100, add(-50, -50))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(min_val, add(min_val, 0))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(1, multiply(1, 1))
  assert_eq(-6, multiply(-2, 3))
}

test "multiply_with_negative_numbers" {
  // 测试包含负数的乘法
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(0, multiply(0, -5))
  assert_eq(15, multiply(-3, -5))
}

test "multiply_boundary_values" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "greet_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, emoji🌍!", greet("emoji🌍"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算矩形周长：2 × (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(30, perimeter)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 8
  let height = 6
  let area = multiply(base, height) / 2
  assert_eq(24, area)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算：商品单价 × 数量 + 运费
  let price = 199
  let quantity = 3
  let shipping = 20
  let total = add(multiply(price, quantity), shipping)
  assert_eq(617, total)
  
  // 温度转换：摄氏度 × 2 + 30（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}