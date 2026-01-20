// 核心 MoonBit 测试用例 - 验证 azimuth 包的基本功能
// 这组测试用例专注于核心数学运算和字符串处理功能

test "basic_addition_operations" {
  // 测试基本加法运算
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(100, @azimuth.add(1, 99))
}

test "basic_multiplication_operations" {
  // 测试基本乘法运算
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "combined_operations" {
  // 测试组合运算
  let a = 6
  let b = 7
  let c = 8
  
  // (a + b) * c
  let sum_ab = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum_ab, c)
  @azimuth.assert_eq(104, result)
}

test "edge_cases" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "assert_functions_work" {
  // 测试断言函数正常工作
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "math_properties" {
  // 测试数学性质
  let x = 5
  let y = 12
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
}

test "real_world_calculation" {
  // 实际应用计算：购物车总价
  let price1 = 25
  let qty1 = 3
  let price2 = 15
  let qty2 = 2
  
  let total1 = @azimuth.multiply(price1, qty1)
  let total2 = @azimuth.multiply(price2, qty2)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(105, cart_total)
}