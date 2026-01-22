// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 测试 add、multiply、greet 等核心函数

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
}

test "multiply_special_cases" {
  // 测试乘法的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "greet_functionality" {
  // 测试问候函数
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, user@123!", @azimuth.greet("user@123"))
  @azimuth.assert_eq_string("Hello,  spaces  !", @azimuth.greet("  spaces  "))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(75, result)  // (10 + 20) * 3 - 15 = 75
}

test "real_world_application" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 2
  let shipping = 15
  let discount = 20
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let after_discount = @azimuth.add(subtotal, -discount)
  let total = @azimuth.add(after_discount, shipping)
  
  @azimuth.assert_eq(383, total)  // 199 * 2 - 20 + 15 = 383
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
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 5)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 5)))
}

test "error_handling_boundaries" {
  // 测试错误处理和边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(@azimuth.add(max_val, 100) == max_val)
  assert_true(@azimuth.add(min_val, -100) == min_val)
  assert_true(@azimuth.multiply(max_val, 2) == max_val)
  assert_true(@azimuth.multiply(min_val, 2) == min_val)
}