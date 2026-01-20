// 新增的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_cases" {
  // 基本加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(50, 50))
}

test "add_function_edge_cases" {
  // 边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 零值测试
  assert_eq(42, add(0, 42))
  assert_eq(-42, add(-42, 0))
  assert_eq(0, add(0, 0))
}

test "multiply_function_basic_cases" {
  // 基本乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(0, multiply(0, 123))
}

test "multiply_function_edge_cases" {
  // 边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 特殊值测试
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(0, multiply(5, 0))
  
  // 溢出保护测试
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_various_inputs" {
  // 字符串拼接测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 张三!", greet("张三"))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 计算矩形面积：长 × 宽
  let length = 8
  let width = 6
  let area = multiply(length, width)
  assert_eq(48, area)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(28, perimeter)
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a × (b + c) = a × b + a × c
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_applications" {
  // 实际应用测试
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let total = add(multiply(item_price, quantity), shipping)
  assert_eq(612, total)
  
  // 温度转换（简化版）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "boundary_combinations" {
  // 边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 极值与普通值运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}