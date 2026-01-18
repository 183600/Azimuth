// 额外的标准 MoonBit 测试用例
// 覆盖 azimuth 模块的核心功能

test "add_function_comprehensive" {
  // 全面测试 add 函数
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 100))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -100))
}

test "multiply_function_comprehensive" {
  // 全面测试 multiply 函数
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(-30, @azimuth.multiply(6, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 2))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 2))
}

test "greet_function_edge_cases" {
  // 测试 greet 函数的边界情况
  @azimuth.assert_eq_string("Hello, 单个字符!", @azimuth.greet("单个字符"))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, 1234567890!", @azimuth.greet("1234567890"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 42
  let b = -17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试不同数值组合
  @azimuth.assert_eq(@azimuth.add(100, -50), @azimuth.add(-50, 100))
  @azimuth.assert_eq(@azimuth.multiply(12, 8), @azimuth.multiply(8, 12))
}

test "calculation_sequence" {
  // 测试计算序列
  let x = 10
  let y = 20
  let z = 5
  
  // (x + y) * z
  let result1 = @azimuth.multiply(@azimuth.add(x, y), z)
  @azimuth.assert_eq(150, result1)
  
  // x * y + z
  let result2 = @azimuth.add(@azimuth.multiply(x, y), z)
  @azimuth.assert_eq(205, result2)
  
  // (x - y) * z (使用 add 实现减法)
  let result3 = @azimuth.multiply(@azimuth.add(x, -y), z)
  @azimuth.assert_eq(-50, result3)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)
  let total = @azimuth.add(subtotal1, subtotal2)
  
  @azimuth.assert_eq(199 * 2 + 99 * 3, total)
  
  // 计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 78
  let sum = @azimuth.add(@azimuth.add(score1, score2), score3)
  let average = sum / 3
  
  @azimuth.assert_eq((85 + 90 + 78) / 3, average)
}

test "boundary_condition_stress" {
  // 边界条件压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值的各种运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值的各种运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 极值之间的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_string_combinations" {
  // 复杂字符串组合测试
  // 测试包含各种字符的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, Mixed123中文!", @azimuth.greet("Mixed123中文"))
  @azimuth.assert_eq_string("Hello, Special#@$%!", @azimuth.greet("Special#@$%"))
}