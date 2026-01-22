// 用户指定的额外 MoonBit 测试用例
// 测试核心功能的各种场景和边界情况

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
}

test "boundary_value_multiplication" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算 (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum = @azimuth.add(a, b)  // 30
  let product = @azimuth.multiply(sum, c)  // 90
  let result = @azimuth.add(product, -d)  // 75
  
  @azimuth.assert_eq(75, result)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 8
  let y = 12
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, 5)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 5)))
}

test "real_world_application" {
  // 测试实际应用：计算购物车总价
  let item1_price = 50
  let item1_quantity = 2
  let item2_price = 30
  let item2_quantity = 3
  let shipping = 10
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 100
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 90
  let subtotal = @azimuth.add(total1, total2)  // 190
  let final_total = @azimuth.add(subtotal, shipping)  // 200
  
  @azimuth.assert_eq(200, final_total)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(@azimuth.add(max_val, max_val, true) == max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, min_val, true) == min_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, max_val, true) == max_val)
  
  // 测试断言函数
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, aéiou!", @azimuth.greet("aéiou"))
}