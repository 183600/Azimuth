// 额外的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_negative_to_positive" {
  // 测试负数加正数的结果
  @azimuth.assert_eq(0, @azimuth.add(-100, 100))
  @azimuth.assert_eq(50, @azimuth.add(-50, 100))
  @azimuth.assert_eq(-50, @azimuth.add(-100, 50))
}

test "multiply_by_negative_two" {
  // 测试乘以-2的情况
  @azimuth.assert_eq(-20, @azimuth.multiply(10, -2))
  @azimuth.assert_eq(20, @azimuth.multiply(-10, -2))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -2))
}

test "greet_with_spaces" {
  // 测试包含各种空格的字符串
  @azimuth.assert_eq_string("Hello,   Multiple   Spaces!", @azimuth.greet("  Multiple   Spaces"))
  @azimuth.assert_eq_string("Hello, Leading Space!", @azimuth.greet(" Leading Space"))
  @azimuth.assert_eq_string("Hello, Trailing Space !", @azimuth.greet("Trailing Space "))
}

test "add_sequence_check" {
  // 测试序列加法的一致性
  let a = 15
  let b = 25
  let c = 35
  
  // (a + b) + c 应该等于 a + (b + c)
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
}

test "multiply_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "boundary_zero_operations" {
  // 测试与零相关的边界操作
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 零与极值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 零与极值的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "greet_with_special_unicode" {
  // 测试特殊Unicode字符
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, 🌟✨!", @azimuth.greet("🌟✨"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
}

test "add_near_boundary_values" {
  // 测试接近边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近最大值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 2))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 101))
  
  // 接近最小值的加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -2))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 100, -101))
}

test "multiply_large_numbers_safety" {
  // 测试大数乘法的安全性
  let sqrt_max = 46340  // sqrt(2147483647) 的整数部分
  
  // 安全的乘法
  @azimuth.assert_eq(2147395600, @azimuth.multiply(sqrt_max, sqrt_max))
  
  // 可能溢出的乘法
  @azimuth.assert_eq(2147483647, @azimuth.multiply(sqrt_max + 1, sqrt_max + 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-sqrt_max - 1, sqrt_max + 1))
}

test "complex_arithmetic_expression" {
  // 测试复杂算术表达式：(a + b) * (c - d) + e * f
  let a = 12
  let b = 8
  let c = 20
  let d = 5
  let e = 3
  let f = 7
  
  // 分步计算
  let sum_ab = @azimuth.add(a, b)  // 20
  let diff_cd = @azimuth.add(c, -d)  // 15
  let product1 = @azimuth.multiply(sum_ab, diff_cd)  // 300
  let product2 = @azimuth.multiply(e, f)  // 21
  let result = @azimuth.add(product1, product2)  // 321
  
  @azimuth.assert_eq(321, result)
}