// 用户自定义的 MoonBit 测试用例
// 包含9个实用的测试用例，使用标准 MoonBit 测试语法

test "basic_addition_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(64, @azimuth.multiply(8, 8))
}

test "greet_function_variations" {
  // 测试问候函数的各种情况
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 6
  let b = 9
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
}

test "edge_case_numbers" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 溢出保护测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 4
  
  let item1_total = @azimuth.multiply(item1_price, item1_qty)
  let item2_total = @azimuth.multiply(item2_price, item2_qty)
  let cart_total = @azimuth.add(item1_total, item2_total)
  
  @azimuth.assert_eq(135, cart_total)  // 25×3 + 15×4 = 75 + 60 = 135
}

test "simple_calculator_expressions" {
  // 测试简单计算器表达式
  let x = 12
  let y = 8
  
  // (x + y) * 2
  let sum = @azimuth.add(x, y)
  let result = @azimuth.multiply(sum, 2)
  @azimuth.assert_eq(40, result)
  
  // x * 3 + y * 2
  let x_part = @azimuth.multiply(x, 3)
  let y_part = @azimuth.multiply(y, 2)
  let total = @azimuth.add(x_part, y_part)
  @azimuth.assert_eq(52, total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", @azimuth.greet("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  @azimuth.assert_eq_string("Hello, test123!", @azimuth.greet("test123"))
  @azimuth.assert_eq_string("Hello, special@chars!", @azimuth.greet("special@chars"))
}

test "assertion_functions_usage" {
  // 测试断言函数的使用
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("hello", "hello")
  
  // 结合项目函数使用
  let add_result = @azimuth.add(7, 8)
  @azimuth.assert_eq(15, add_result)
  @azimuth.assert_true(add_result > 10)
  
  let greet_result = @azimuth.greet("Test")
  @azimuth.assert_eq_string("Hello, Test!", greet_result)
}