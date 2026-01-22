// 简单的 azimuth 测试用例

// 导入 azimuth 库
use src.azimuth

test "basic_addition_test" {
  // 基本加法测试
  assert_eq(10, azimuth.add(3, 7))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(-5, azimuth.add(-2, -3))
}

test "basic_multiplication_test" {
  // 基本乘法测试
  assert_eq(15, azimuth.multiply(3, 5))
  assert_eq(0, azimuth.multiply(0, 10))
  assert_eq(-6, azimuth.multiply(-2, 3))
}

test "basic_greet_test" {
  // 基本问候测试
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
}

test "basic_divide_with_ceil_test" {
  // 基本向上取整除法测试
  assert_eq(4, azimuth.divide_with_ceil(10, 3))
  assert_eq(3, azimuth.divide_with_ceil(9, 3))
  assert_eq(0, azimuth.divide_with_ceil(5, 0))
}

test "practical_packaging_test" {
  // 实际应用：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = azimuth.divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "financial_calculation_test" {
  // 金融计算测试
  let principal = 1000
  let rate = 8
  let interest = azimuth.divide_with_ceil(azimuth.multiply(principal, rate), 100)
  let total = azimuth.add(principal, interest)
  assert_eq(80, interest)
  assert_eq(1080, total)
}

test "time_calculation_test" {
  // 时间计算测试
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  assert_eq(6, days_needed)  // ceil(45/8) = 6
}

test "complex_expression_test" {
  // 复杂表达式测试
  let result = azimuth.add(azimuth.multiply(2, 3), azimuth.multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "negative_number_test" {
  // 负数测试
  assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  assert_eq(4, azimuth.divide_with_ceil(-10, -3))
  assert_eq(-6, azimuth.add(-2, -4))
}

test "unicode_greeting_test" {
  // Unicode 问候测试
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
}