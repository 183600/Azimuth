// 用户新增的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法

test "add_negative_numbers" {
  // 测试负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "add_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  @azimuth.assert_eq(0, @azimuth.add(1500000000, -1500000000))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(-3, 7))
  @azimuth.assert_eq(21, @azimuth.multiply(3, -7))
}

test "multiply_large_numbers" {
  // 测试大数相乘
  @azimuth.assert_eq(1000000000, @azimuth.multiply(10000, 100000))
  @azimuth.assert_eq(-1000000000, @azimuth.multiply(-10000, 100000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, John@Doe!", @azimuth.greet("John@Doe"))
  @azimuth.assert_eq_string("Hello, 用户#123!", @azimuth.greet("用户#123"))
  @azimuth.assert_eq_string("Hello, test_case!", @azimuth.greet("test_case"))
}

test "complex_mathematical_expression" {
  // 测试复杂数学表达式：(a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(38, result)
}

test "sequential_operations" {
  // 测试连续运算：a + b + c + d
  let a = 5
  let b = 10
  let c = 15
  let d = 20
  
  let sum_ab = @azimuth.add(a, b)
  let sum_abc = @azimuth.add(sum_ab, c)
  let sum_abcd = @azimuth.add(sum_abc, d)
  
  @azimuth.assert_eq(50, sum_abcd)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "string_edge_cases" {
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", @azimuth.greet("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  @azimuth.assert_eq_string("Hello, 1234567890!", @azimuth.greet("1234567890"))
}

test "real_world_calculation" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(697, cart_total)
}