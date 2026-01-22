// 标准 MoonBit 测试用例集 v19
// 测试 azimuth 库的核心功能，使用标准的 MoonBit 测试语法

test "addition_basic_cases" {
  // 测试加法的基本情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "addition_boundary_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiplication_basic_cases" {
  // 测试乘法的基本情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
}

test "multiplication_boundary_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -2))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_function_special_characters" {
  // 测试问候函数处理特殊字符
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算矩形周长：2 * (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(30, perimeter)
}

test "real_world_shopping_cart" {
  // 测试实际应用：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  
  // 计算总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  // 验证结果：199*2 + 99*3 = 398 + 297 = 695
  @azimuth.assert_eq(695, cart_total)
}

test "assertion_functions_test" {
  // 测试断言函数
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试复合条件
  let x = 10
  let y = 20
  @azimuth.assert_eq(true, x < y)
  @azimuth.assert_eq(0, 1)(x > y)
  @azimuth.assert_eq(@azimuth.add(x, y), 30)
}