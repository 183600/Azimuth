// 标准MoonBit测试用例 - 10个高质量测试用例
// 测试azimuth库的核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  @azimuth.assert_eq(@azimuth.add(2, 3), 5)
  @azimuth.assert_eq(@azimuth.add(-2, 3), 1)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.add(100, 200), 300)
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  @azimuth.assert_eq(@azimuth.multiply(3, 4), 12)
  @azimuth.assert_eq(@azimuth.multiply(-2, 5), -10)
  @azimuth.assert_eq(@azimuth.multiply(0, 100), 0)
  @azimuth.assert_eq(@azimuth.multiply(7, 8), 56)
}

test "basic_arithmetic_subtraction" {
  // 测试基本减法运算
  @azimuth.assert_eq(@azimuth.subtract(10, 3), 7)
  @azimuth.assert_eq(@azimuth.subtract(5, 8), -3)
  @azimuth.assert_eq(@azimuth.subtract(0, 0), 0)
  @azimuth.assert_eq(@azimuth.subtract(100, 50), 50)
}

test "division_with_ceil_basic" {
  // 测试向上取整除法的基本情况
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, 3), 4)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(9, 3), 3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(1, 2), 1)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(0, 5), 0)
}

test "division_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, 0), 0)  // 除数为0的情况
  @azimuth.assert_eq(@azimuth.divide_with_ceil(0, 0), 0)   // 两个0的情况
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-10, 3), -3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, -3), 3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-10, -3), 4)
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  @azimuth.assert_eq_string(@azimuth.greet("World"), "Hello, World!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet("测试"), "Hello, 测试!")
}

test "complex_calculation_scenario" {
  // 测试复合计算场景
  let a = 10
  let b = 5
  let c = 2
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(result1, 30)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(result2, 52)
  
  // (a - b) / c (向上取整)
  let result3 = @azimuth.divide_with_ceil(@azimuth.subtract(a, b), c)
  @azimuth.assert_eq(result3, 3)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法单位元
  @azimuth.assert_eq(@azimuth.add(x, 0), x)
  
  // 乘法单位元
  @azimuth.assert_eq(@azimuth.multiply(x, 1), x)
}

test "boundary_value_testing" {
  // 测试边界值
  // 测试零值
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, 5), 0)
  @azimuth.assert_eq(@azimuth.subtract(0, 0), 0)
  
  // 测试单位值
  @azimuth.assert_eq(@azimuth.add(1, 0), 1)
  @azimuth.assert_eq(@azimuth.multiply(1, 5), 5)
  @azimuth.assert_eq(@azimuth.subtract(1, 0), 1)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(5, 1), 5)
  
  // 测试负一
  @azimuth.assert_eq(@azimuth.add(-1, 1), 0)
  @azimuth.assert_eq(@azimuth.multiply(-1, 5), -5)
  @azimuth.assert_eq(@azimuth.subtract(0, -1), 1)
}

test "practical_application_simulation" {
  // 模拟实际应用场景：购物车计算
  let item1_price = 25
  let item2_price = 30
  let item3_price = 45
  let discount = 10
  let tax_rate = 5  // 5%
  
  // 计算小计
  let subtotal = @azimuth.add(@azimuth.add(item1_price, item2_price), item3_price)
  
  // 应用折扣
  let discounted_total = @azimuth.subtract(subtotal, discount)
  
  // 计算税费（向上取整）
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(discounted_total, tax_rate), 100)
  
  // 计算最终总额
  let final_total = @azimuth.add(discounted_total, tax)
  
  @azimuth.assert_eq(subtotal, 100)
  @azimuth.assert_eq(discounted_total, 90)
  @azimuth.assert_eq(tax, 5)
  @azimuth.assert_eq(final_total, 95)
}