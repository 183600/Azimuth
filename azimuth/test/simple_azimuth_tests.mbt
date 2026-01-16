// 简单的 MoonBit 测试用例
// 测试 azimuth 模块的基本功能

test "add_basic" {
  // 测试基本加法
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-1, -1))
}

test "multiply_basic" {
  // 测试基本乘法
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
}

test "greet_basic" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "assertions" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "math_expressions" {
  // 测试数学表达式
  let a = 10
  let b = 20
  
  let sum_ab = @azimuth.add(a, b)
  @azimuth.assert_eq(30, sum_ab)
  
  let product_ab = @azimuth.multiply(a, b)
  @azimuth.assert_eq(200, product_ab)
}

test "real_world_scenario" {
  // 测试实际应用场景
  let price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(612, total)
}

test "string_cases" {
  // 测试字符串情况
  @azimuth.assert_eq_string("Hello, John!", @azimuth.greet("John"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
}

test "negative_numbers" {
  // 测试负数运算
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(5, @azimuth.multiply(-1, -5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
}

test "sequential_operations" {
  // 测试连续运算
  let result1 = @azimuth.add(@azimuth.add(1, 2), 3)
  @azimuth.assert_eq(6, result1)
  
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(24, result2)
}