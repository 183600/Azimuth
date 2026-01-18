// Azimuth 项目新增综合测试用例
// 使用标准 MoonBit 测试语法，补充现有测试覆盖

test "add_zero_handling" {
  // 测试加法的零值处理
  assert_eq(42, add(0, 42))
  assert_eq(42, add(42, 0))
  assert_eq(0, add(0, 0))
  assert_eq(-17, add(0, -17))
  assert_eq(-17, add(-17, 0))
}

test "add_large_numbers" {
  // 测试大数加法
  assert_eq(2000000000, add(1000000000, 1000000000))
  assert_eq(-2000000000, add(-1000000000, -1000000000))
  assert_eq(0, add(1500000000, -1500000000))
}

test "multiply_by_zero_and_one" {
  // 测试乘法的零值和单位值处理
  assert_eq(0, multiply(12345, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(0, 0))
  assert_eq(12345, multiply(12345, 1))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-12345, 1))
  assert_eq(-12345, multiply(1, -12345))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(0, multiply(-3, 0))
}

test "multiply_edge_cases" {
  // 测试乘法边界情况
  let min_val = -2147483648
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(2, min_val))
  assert_eq(0, multiply(min_val, 0))
}

test "greet_empty_and_special" {
  // 测试问候函数的空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, !", greet(" "))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quote\"!", greet("\"quote\""))
}

test "greet_unicode_and_emojis" {
  // 测试 Unicode 和 Emoji 字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🌟✨!", greet("🌟✨"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂数学序列计算
  // 计算 1 + 2 + 3 + 4 + 5
  let sum1 = add(1, add(2, add(3, add(4, 5))))
  assert_eq(15, sum1)
  
  // 计算 2*3 + 4*5
  let term1 = multiply(2, 3)
  let term2 = multiply(4, 5)
  let result = add(term1, term2)
  assert_eq(26, result)
}

test "nested_calculations" {
  // 测试嵌套计算
  // (2 + 3) * (4 + 5)
  let sum1 = add(2, 3)
  let sum2 = add(4, 5)
  let product = multiply(sum1, sum2)
  assert_eq(45, product)
  
  // 10 + (20 * 3) + (40 / 2) - 使用整数运算模拟
  let mult = multiply(20, 3)
  let total = add(10, mult)
  assert_eq(70, total)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算矩形面积和周长
  let length = 8
  let width = 6
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(48, area)
  assert_eq(28, perimeter)
  
  // 计算折扣价格：原价100，折扣20%，最终价格
  let original_price = 100
  let discount_rate = 20  // 20%
  let discount_amount = multiply(original_price, discount_rate) / 100
  let final_price = add(original_price, -discount_amount)
  assert_eq(80, final_price)
}