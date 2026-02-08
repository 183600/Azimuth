// 标准 MoonBit 测试用例
// 为 Azimuth 项目创建的高质量测试套件

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(@azimuth.add(10, 15), 25)
  assert_eq(@azimuth.multiply(7, 8), 56)
  assert_eq(@azimuth.subtract(20, 12), 8)
  assert_eq(@azimuth.divide_with_ceil(17, 5), 4)
  assert_eq(@azimuth.divide_with_ceil(20, 5), 4)
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(@azimuth.add(0, 0), 0)
  assert_eq(@azimuth.multiply(0, 100), 0)
  assert_eq(@azimuth.subtract(0, 0), 0)
  assert_eq(@azimuth.divide_with_ceil(0, 5), 0)
  assert_eq(@azimuth.divide_with_ceil(10, 0), 0)
  assert_eq(@azimuth.divide_with_ceil(0, 0), 0)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(@azimuth.add(-5, 10), 5)
  assert_eq(@azimuth.add(-8, -3), -11)
  assert_eq(@azimuth.multiply(-4, 6), -24)
  assert_eq(@azimuth.multiply(-3, -7), 21)
  assert_eq(@azimuth.subtract(-10, 5), -15)
  assert_eq(@azimuth.divide_with_ceil(-15, 4), -3)
}

test "string_processing_functions" {
  // 字符串处理函数测试
  assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(@azimuth.greet(""), "Hello, !")
  assert_eq_string(@azimuth.greet("123"), "Hello, 123!")
  assert_eq_string(@azimuth.greet("测试"), "Hello, 测试!")
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 13
  let y = 17
  
  // 加法交换律
  assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法单位元
  assert_eq(@azimuth.add(x, 0), x)
  
  // 乘法单位元
  assert_eq(@azimuth.multiply(x, 1), x)
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  let base = 100
  let rate = 15
  let years = 3
  
  // 复利计算模拟
  let year1 = @azimuth.add(base, @azimuth.divide_with_ceil(@azimuth.multiply(base, rate), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate), 100))
  
  assert_eq(year1, 115)
  assert_eq(year2, 133)
  assert_eq(year3, 153)
}

test "edge_case_divisions" {
  // 边界除法情况测试
  assert_eq(@azimuth.divide_with_ceil(1, 2), 1)
  assert_eq(@azimuth.divide_with_ceil(99, 100), 1)
  assert_eq(@azimuth.divide_with_ceil(100, 99), 2)
  assert_eq(@azimuth.divide_with_ceil(-10, 3), -3)
  assert_eq(@azimuth.divide_with_ceil(10, -3), 3)
  assert_eq(@azimuth.divide_with_ceil(-10, -3), 4)
}

test "large_number_operations" {
  // 大数运算测试
  let large_num = 100000
  assert_eq(@azimuth.add(large_num, large_num), 200000)
  assert_eq(@azimuth.multiply(1000, 1000), 1000000)
  assert_eq(@azimuth.subtract(large_num, 50000), 50000)
  assert_eq(@azimuth.divide_with_ceil(large_num, 3), 33334)
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算测试
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let tax_rate = 10
  
  // 计算小计
  let item1_subtotal = @azimuth.multiply(item1_price, item1_qty)
  let item2_subtotal = @azimuth.multiply(item2_price, item2_qty)
  let subtotal = @azimuth.add(item1_subtotal, item2_subtotal)
  
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = @azimuth.add(subtotal, tax)
  
  assert_eq(item1_subtotal, 75)
  assert_eq(item2_subtotal, 30)
  assert_eq(subtotal, 105)
  assert_eq(tax, 11)
  assert_eq(total, 116)
}

test "algorithm_simulation" {
  // 算法模拟测试 - 斐波那契数列
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f1, f0)
  let f3 = @azimuth.add(f2, f1)
  let f4 = @azimuth.add(f3, f2)
  let f5 = @azimuth.add(f4, f3)
  
  assert_eq(f2, 1)
  assert_eq(f3, 2)
  assert_eq(f4, 3)
  assert_eq(f5, 5)
}