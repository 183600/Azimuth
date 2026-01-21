// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic_tests" {
  // 基本加法测试
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(0, azimuth::add(-2, 2))
  azimuth::assert_eq(-5, azimuth::add(-2, -3))
  azimuth::assert_eq(100, azimuth::add(50, 50))
}

test "add_function_edge_cases" {
  // 边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  azimuth::assert_eq(max_val, azimuth::add(max_val, 1))
  azimuth::assert_eq(min_val, azimuth::add(min_val, -1))
  
  // 测试零值
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(5, azimuth::add(0, 5))
  azimuth::assert_eq(-5, azimuth::add(-5, 0))
}

test "multiply_function_basic_tests" {
  // 基本乘法测试
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
  azimuth::assert_eq(-6, azimuth::multiply(2, -3))
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(1, azimuth::multiply(1, 100))
}

test "multiply_function_special_cases" {
  // 特殊情况测试
  let min_val = -2147483648
  
  // 测试溢出保护
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, -1))
  
  // 测试单位元
  azimuth::assert_eq(5, azimuth::multiply(5, 1))
  azimuth::assert_eq(-5, azimuth::multiply(5, -1))
}

test "greet_function_tests" {
  // 问候函数测试
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
}

test "complex_calculation_tests" {
  // 复杂计算测试
  // 矩形面积计算
  let length = 10
  let width = 5
  let area = azimuth::multiply(length, width)
  azimuth::assert_eq(50, area)
  
  // 周长计算
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  azimuth::assert_eq(30, perimeter)
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 分配律
  azimuth::assert_eq(azimuth::multiply(a, azimuth::add(b, 3)), azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, 3)))
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 购物车计算
  let item_price = 199
  let quantity = 2
  let shipping = 15
  
  let subtotal = azimuth::multiply(item_price, quantity)
  let total = azimuth::add(subtotal, shipping)
  azimuth::assert_eq(413, total)
  
  // 温度转换（简化版）
  let celsius = 20
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  azimuth::assert_eq(70, fahrenheit)
}

test "boundary_value_combinations" {
  // 边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  azimuth::assert_eq(-1, azimuth::add(max_val, min_val))
  azimuth::assert_eq(0, azimuth::multiply(max_val, 0))
  azimuth::assert_eq(0, azimuth::multiply(min_val, 0))
  
  // 边界值附近的运算
  azimuth::assert_eq(2147483646, azimuth::add(max_val, -1))
  azimuth::assert_eq(-2147483647, azimuth::add(min_val, 1))
}

test "error_handling_and_assertions" {
  // 错误处理和断言测试
  azimuth::assert_true(true)
  azimuth::assert_false(false)
  azimuth::assert_eq(42, 42)
  azimuth::assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  azimuth::assert_true(azimuth::add(max_val, 100) == max_val)
  azimuth::assert_true(azimuth::multiply(max_val, 2) == max_val)
}