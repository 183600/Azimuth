// 简洁的标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

// 导入要测试的模块
use azimuth

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(azimuth.add(10, 25), 35)
  assert_eq(azimuth.multiply(4, 7), 28)
  assert_eq(azimuth.divide_with_ceil(10, 3), 4)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(azimuth.add(-5, 3), -2)
  assert_eq(azimuth.multiply(-4, 6), -24)
  assert_eq(azimuth.multiply(-3, -5), 15)
}

test "zero_handling" {
  // 测试零值处理
  assert_eq(azimuth.add(0, 42), 42)
  assert_eq(azimuth.multiply(15, 0), 0)
  assert_eq(azimuth.multiply(0, 0), 0)
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(azimuth.add(max_val, 1), max_val)
  assert_eq(azimuth.add(min_val, -1), min_val)
  assert_eq(azimuth.multiply(max_val, 2), max_val)
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string(azimuth.greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(azimuth.greet(""), "Hello, !")
  assert_eq_string(azimuth.greet("测试"), "Hello, 测试!")
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 分配律简化测试
  assert_eq(azimuth.multiply(a, azimuth.add(b, 2)), azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, 2)))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算矩形周长：2 × (长 + 宽)
  let length = 15
  let width = 8
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  assert_eq(46, perimeter)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 12
  let height = 9
  let area = azimuth.multiply(base, height) / 2
  assert_eq(54, area)
}

test "real_world_application" {
  // 测试实际应用场景
  // 购物车计算：商品价格 × 数量 + 运费
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = azimuth.multiply(item_price, quantity)
  let total = azimuth.add(subtotal, shipping)
  assert_eq(617, total)
}

test "assertion_functions" {
  // 测试断言函数
  azimuth.assert_true(true)
  azimuth.assert_false(false)
  azimuth.assert_eq(100, 100)
  azimuth.assert_eq_string("test", "test")
}

test "edge_case_combinations" {
  // 测试边界情况组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 混合运算
  assert_eq(azimuth.add(azimuth.multiply(2, 3), azimuth.multiply(4, 5)), 26)
  assert_eq(azimuth.multiply(azimuth.add(10, 20), azimuth.add(5, 5)), 300)
  
  // 边界值与普通值运算
  assert_eq(azimuth.add(max_val, -max_val), 0)
  assert_eq(azimuth.multiply(min_val, 0), 0)
}