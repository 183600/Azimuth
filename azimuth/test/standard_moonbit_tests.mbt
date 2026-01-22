// 标准 MoonBit 测试用例
// 覆盖 azimuth 包的核心功能

// 测试1: 基础加法功能
test "add_basic_functionality" {
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
}

// 测试2: 基础乘法功能
test "multiply_basic_functionality" {
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

// 测试3: 问候语功能
test "greet_functionality" {
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

// 测试4: 向上取整除法 - 正数
test "divide_with_ceil_positive" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
}

// 测试5: 向上取整除法 - 边界情况
test "divide_with_ceil_edge_cases" {
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 2))
}

// 测试6: 数学运算结合
test "combined_mathematical_operations" {
  let step1 = @azimuth.add(5, 3)
  let step2 = @azimuth.multiply(step1, 4)
  let step3 = @azimuth.divide_with_ceil(step2, 3)
  @azimuth.assert_eq(11, step3)  // ((5+3)*4)/3 = 32/3 = 11 (ceil)
}

// 测试7: 实际应用场景 - 包装计算
test "packaging_calculation" {
  let items = 23
  let capacity = 5
  let packages = @azimuth.divide_with_ceil(items, capacity)
  @azimuth.assert_eq(5, packages)  // ceil(23/5) = 5
}

// 测试8: 实际应用场景 - 价格计算
test "price_calculation" {
  let unit_price = 15
  let quantity = 4
  let discount = 5
  let total = @azimuth.multiply(unit_price, quantity)
  let final_price = @azimuth.add(total, -discount)
  @azimuth.assert_eq(55, final_price)  // 15*4-5 = 55
}

// 测试9: 字符串处理 - 特殊字符
test "greet_special_characters" {
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

// 测试10: 复杂数学序列
test "complex_calculation_sequence" {
  let a = @azimuth.multiply(3, 4)
  let b = @azimuth.add(a, 5)
  let c = @azimuth.divide_with_ceil(b, 2)
  let d = @azimuth.multiply(c, 3)
  @azimuth.assert_eq(24, d)  // ceil(((3*4)+5)/2)*3 = ceil(17/2)*3 = 9*3 = 27
}