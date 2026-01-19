// 标准 MoonBit 测试用例
// 覆盖 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
  @azimuth.assert_eq(10, @azimuth.add(-5, 15))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 测试负溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  
  // 测试负溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  
  // 测试特殊值
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "greet_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(13, 3))
  
  // 测试负数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-12, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-13, 3))
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法单位元
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  
  // 乘法单位元
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
}

test "complex_calculations" {
  // 测试复杂计算
  // (2 + 3) * 4 = 20
  let temp = @azimuth.add(2, 3)
  let result = @azimuth.multiply(temp, 4)
  @azimuth.assert_eq(20, result)
  
  // 5 * (6 + 7) = 65
  let temp2 = @azimuth.add(6, 7)
  let result2 = @azimuth.multiply(5, temp2)
  @azimuth.assert_eq(65, result2)
}

test "assert_functions" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(10, 10)
  @azimuth.assert_eq_string("test", "test")
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算：商品价格总和
  let item1 = 199
  let item2 = 299
  let item3 = 399
  let total = @azimuth.add(@azimuth.add(item1, item2), item3)
  @azimuth.assert_eq(897, total)
  
  // 折扣计算：原价8折
  let original_price = 1000
  let discounted_price = @azimuth.multiply(original_price, 8) / 10
  @azimuth.assert_eq(800, discounted_price)
}