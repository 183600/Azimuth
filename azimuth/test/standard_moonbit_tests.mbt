// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
test "add_function_basic_cases" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
}

test "multiply_function_basic_cases" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "greet_function_basic_cases" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic_cases" {
  // 测试基本的向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  @azimuth.assert_eq(@azimuth.add(7, 13), @azimuth.add(13, 7))
  @azimuth.assert_eq(@azimuth.multiply(7, 13), @azimuth.multiply(13, 7))
  @azimuth.assert_eq(@azimuth.add(-5, 10), @azimuth.add(10, -5))
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(-x, @azimuth.multiply(x, -1))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let result1 = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(16, result1)
  
  // a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(17, result2)
}

test "unicode_and_special_characters" {
  // 测试Unicode字符和特殊字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：计算装盒问题
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)
  
  // 场景2：计算总价
  let unit_price = 15
  let quantity = 4
  let total_price = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(60, total_price)
  
  // 场景3：个性化问候
  let customer_name = "张三"
  let greeting = @azimuth.greet(customer_name)
  @azimuth.assert_eq_string("Hello, 张三!", greeting)
}

test "edge_cases_and_boundary_conditions" {
  // 测试边界条件和边缘情况
  // 大数测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
}