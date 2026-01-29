// 标准 MoonBit 测试文件
// 简洁但全面的测试用例，覆盖 azimuth 库的核心功能

test "add_function_core" {
  // 测试加法核心功能
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
}

test "multiply_function_core" {
  // 测试乘法核心功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 7))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "greet_function_core" {
  // 测试问候核心功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative" {
  // 测试负数向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-4, 2))
}

test "mathematical_laws" {
  // 测试数学定律
  let x = 6
  let y = 9
  
  // 交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 零元素
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
}

test "combined_calculations" {
  // 测试组合计算
  let result = @azimuth.add(@azimuth.multiply(3, 4), @azimuth.multiply(2, 5))
  @azimuth.assert_eq(22, result)  // 3*4 + 2*5 = 12 + 10 = 22
}

test "real_world_scenario" {
  // 测试实际应用场景：计算购物车总价
  let price = 45
  let quantity = 4
  let shipping = 12
  
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(180, subtotal)
  @azimuth.assert_eq(192, total)
}

test "packaging_efficiency" {
  // 测试包装效率计算
  let items = 37
  let capacity = 8
  let packages = @azimuth.divide_with_ceil(items, capacity)
  
  @azimuth.assert_eq(5, packages)  // ceil(37/8) = 5
}

test "unicode_support" {
  // 测试 Unicode 支持
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
}