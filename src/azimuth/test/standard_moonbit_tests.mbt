// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-3, add(-8, 5))
}

test "add_function_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(0, multiply(0, 123))
}

test "multiply_function_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(min_val, 0))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_function_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
}

test "complex_calculations" {
  // 测试复合计算
  // 计算 (a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = add(a, b)  // 8
  let product = multiply(sum_ab, c)  // 32
  let result = add(product, -d)  // 22
  
  assert_eq(22, result)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 11
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 计算矩形面积：长 × 宽
  let length = 8
  let width = 6
  let area = multiply(length, width)
  assert_eq(48, area)
}