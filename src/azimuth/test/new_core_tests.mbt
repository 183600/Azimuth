// 标准 MoonBit 核心测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_boundary_values" {
  // 测试加法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "multiply_boundary_values" {
  // 测试乘法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_function_special_chars" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(900, result)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(230, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 15
  let y = 25
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, 5)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 5)))
}

test "assert_functions_verification" {
  // 测试断言函数
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试比较操作
  assert_true(5 > 3)
  assert_true(10 >= 10)
  assert_false(5 < 3)
  assert_false(10 <= 9)
}

test "real_world_application_scenario" {
  // 实际应用场景：计算购物车总价
  let item1_price = 100
  let item1_quantity = 2
  let item2_price = 50
  let item2_quantity = 3
  let shipping_fee = 15
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(325, final_total)
}