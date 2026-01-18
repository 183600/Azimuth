// 标准 MoonBit 测试用例 - 第17版
// 测试项目的核心功能：add, multiply, greet 和断言函数

test "add_positive_numbers" {
  // 测试正数相加
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "add_boundary_values" {
  // 测试边界值相加
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_basic_operations" {
  // 测试基本乘法运算
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_function_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, User@123!", @azimuth.greet("User@123"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(22, result)
}

test "assert_functions_verification" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "real_world_application" {
  // 测试实际应用场景：计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let tax_rate = 10
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(676, total)
}