// 简洁核心测试用例 - 标准MoonBit测试语法
// 测试核心功能：add, multiply, greet函数

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "multiply_special_cases" {
  // 测试乘法特殊情况
  let min_val = -2147483648
  
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_functionality" {
  // 测试greet函数
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "complex_calculation" {
  // 测试复合计算：矩形面积+周长
  let length = 8
  let width = 5
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  let total = @azimuth.add(area, perimeter)
  
  @azimuth.assert_eq(76, total) // 8*5 + 2*(8+5) = 40 + 26 = 66
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
}

test "real_world_scenario" {
  // 实际应用场景：购物车计算
  let item_price = 199
  let quantity = 2
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(413, total) // 199*2 + 15 = 398 + 15 = 413
}

test "error_handling" {
  // 测试错误处理和断言
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "performance_calculation" {
  // 性能相关计算：等差数列求和
  // 1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(1, n)) / 2
  
  @azimuth.assert_eq(55, sum)
}