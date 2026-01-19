// Azimuth 增强测试用例
// 使用标准的 MoonBit 测试语法，覆盖更多边界情况和功能组合

test "add_zero_handling" {
  // 测试加法中的零值处理
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-3, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483645, 1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483646, -1))
  @azimuth.assert_eq(2147483647, @azimuth.add(1073741823, 1073741824))
}

test "multiply_zero_and_one" {
  // 测试乘法中的零和一处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(-5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(1, -5))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "multiply_boundary_values" {
  // 测试乘法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(2147483647, @azimuth.multiply(1, max_val))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(1, min_val))
  @azimuth.assert_eq(-2147483647, @azimuth.multiply(-1, max_val))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-1, min_val))
}

test "greet_unicode_characters" {
  // 测试包含Unicode字符的问候
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
}

test "greet_edge_cases" {
  // 测试问候功能的边界情况
  @azimuth.assert_eq_string("Hello,     !", @azimuth.greet("    "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
}

test "complex_mathematical_operations" {
  // 测试复杂数学运算
  let x = 100
  let y = -50
  let z = 25
  
  // 计算 (x + y) * z
  let sum = @azimuth.add(x, y)
  let product = @azimuth.multiply(sum, z)
  @azimuth.assert_eq(-1250, product)
  
  // 计算 x * (y + z)
  let sum2 = @azimuth.add(y, z)
  let product2 = @azimuth.multiply(x, sum2)
  @azimuth.assert_eq(-2500, product2)
  
  // 计算 (x * y) + (x * z)
  let product3 = @azimuth.multiply(x, y)
  let product4 = @azimuth.multiply(x, z)
  let final_sum = @azimuth.add(product3, product4)
  @azimuth.assert_eq(-2500, final_sum)
}

test "extreme_overflow_scenarios" {
  // 测试极端溢出场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 多次加法可能导致溢出
  let result1 = @azimuth.add(max_val - 1, 2)
  @azimuth.assert_eq(max_val, result1)
  
  let result2 = @azimuth.add(min_val + 1, -2)
  @azimuth.assert_eq(min_val, result2)
  
  // 大数乘法
  let result3 = @azimuth.multiply(65536, 32768)
  @azimuth.assert_eq(max_val, result3)
}