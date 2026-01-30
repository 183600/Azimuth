// 简单的 Azimuth 测试用例
// 包含基本的测试用例，使用标准 MoonBit 测试语法

test "simple_add_test" {
  // 简单的加法测试
  assert_eq(5, azimuth.add(2, 3))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(-5, azimuth.add(-2, -3))
}

test "simple_multiply_test" {
  // 简单的乘法测试
  assert_eq(6, azimuth.multiply(2, 3))
  assert_eq(0, azimuth.multiply(0, 5))
  assert_eq(-6, azimuth.multiply(-2, 3))
}

test "simple_greet_test" {
  // 简单的问候测试
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
}

test "simple_divide_with_ceil_test" {
  // 简单的向上取整除法测试
  assert_eq(4, azimuth.divide_with_ceil(10, 3))
  assert_eq(3, azimuth.divide_with_ceil(9, 3))
  assert_eq(0, azimuth.divide_with_ceil(5, 0))
}

test "simple_subtract_test" {
  // 简单的减法测试
  assert_eq(2, azimuth.subtract(5, 3))
  assert_eq(-2, azimuth.subtract(3, 5))
  assert_eq(0, azimuth.subtract(5, 5))
}

test "mathematical_properties_test" {
  // 数学性质测试
  let x = 10
  let y = 5
  
  // 加法交换律
  assert_eq(azimuth.add(x, y), azimuth.add(y, x))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(x, y), azimuth.multiply(y, x))
}

test "edge_cases_test" {
  // 边界情况测试
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 5))
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
}

test "practical_scenario_test" {
  // 实际场景测试：包装计算
  let items = 25
  let box_capacity = 6
  let boxes_needed = azimuth.divide_with_ceil(items, box_capacity)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = azimuth.multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
}

test "complex_calculation_test" {
  // 复杂计算测试
  let a = azimuth.add(5, 3)        // 8
  let b = azimuth.multiply(a, 2)   // 16
  let c = azimuth.divide_with_ceil(b, 5)  // 4
  let result = azimuth.add(c, 1)    // 5
  
  assert_eq(5, result)
}

test "unicode_support_test" {
  // Unicode 支持测试
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  assert_eq_string("Hello, café!", azimuth.greet("café"))
}