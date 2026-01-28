// 简单的高质量测试用例
// 验证基本功能

test "simple_high_quality_test_1" {
  // 基本算术测试
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "simple_high_quality_test_2" {
  // 字符串测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "simple_high_quality_test_3" {
  // 组合运算测试
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "simple_high_quality_test_4" {
  // 边界情况测试
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "simple_high_quality_test_5" {
  // 负数测试
  assert_eq(-5, add(-2, -3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "simple_high_quality_test_6" {
  // 大数测试
  assert_eq(2000, add(1000, 1000))
  assert_eq(10000, multiply(100, 100))
}

test "simple_high_quality_test_7" {
  // 实际应用测试 - 包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "simple_high_quality_test_8" {
  // 实际应用测试 - 预算计算
  let income = 5000
  let expenses = 3500
  let remaining = add(income, -expenses)
  assert_eq(1500, remaining)
}

test "simple_high_quality_test_9" {
  // 数学性质测试 - 交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "simple_high_quality_test_10" {
  // 复杂表达式测试
  let x = 10
  let y = 15
  let z = 20
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(499, result)  // (10+15)*20 - ceil(10/15) = 25*20 - 1 = 500 - 1 = 499
}