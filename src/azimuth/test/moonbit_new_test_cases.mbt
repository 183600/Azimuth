// MoonBit 新增测试用例
// 使用标准的 MoonBit 测试语法
// 测试基本数学运算和字符串处理功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, @azimuth.add(2, 3))
  assert_eq(0, @azimuth.add(-3, 3))
  assert_eq(-5, @azimuth.add(-2, -3))
  assert_eq(10, @azimuth.add(0, 10))
  assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, @azimuth.multiply(2, 3))
  assert_eq(-6, @azimuth.multiply(2, -3))
  assert_eq(0, @azimuth.multiply(5, 0))
  assert_eq(1, @azimuth.multiply(1, 7))
  assert_eq(25, @azimuth.multiply(5, 5))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", @azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", @azimuth.greet(""))
  assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -12
  let d = 8
  
  assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  assert_eq(max_val, @azimuth.add(max_val, 1))
  assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值测试
  assert_eq(min_val, @azimuth.add(min_val, -1))
  assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值组合
  assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum = @azimuth.add(a, b)  // 30
  let product = @azimuth.multiply(sum, c)  // 90
  let result = @azimuth.add(product, -d)  // 75
  
  assert_eq(75, result)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 空字符串
  assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单个字符
  assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  // 特殊字符
  assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}

test "real_world_application" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 398
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 299
  let subtotal = @azimuth.add(total1, total2)  // 697
  
  // 加上运费
  let final_total = @azimuth.add(subtotal, shipping)  // 717
  
  assert_eq(717, final_total)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列前5项的和：1 + 3 + 5 + 7 + 9 = 25
  let term1 = 1
  let term2 = 3
  let term3 = 5
  let term4 = 7
  let term5 = 9
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  assert_eq(25, sum)
  
  // 使用公式验证：n * (首项 + 末项) / 2
  let n = 5
  let formula_sum = @azimuth.multiply(n, @azimuth.add(term1, term5)) / 2
  assert_eq(25, formula_sum)
}

test "error_handling_validation" {
  // 测试错误处理验证
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(@azimuth.add(max_val, 100) == max_val)
  assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 测试边界情况
  let min_val = -2147483648
  assert_true(@azimuth.add(min_val, -100) == min_val)
  assert_true(@azimuth.multiply(min_val, -1) == min_val)
}