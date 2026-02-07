// 标准 MoonBit 测试用例
// 简洁版本 - 包含基本功能测试

test "basic_addition_functionality" {
  // 基本加法功能测试
  @azimuth.assert_eq(@azimuth.add(2, 3), 5)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.add(-5, 5), 0)
  @azimuth.assert_eq(@azimuth.add(100, 200), 300)
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  @azimuth.assert_eq(@azimuth.multiply(3, 4), 12)
  @azimuth.assert_eq(@azimuth.multiply(0, 5), 0)
  @azimuth.assert_eq(@azimuth.multiply(-2, 3), -6)
  @azimuth.assert_eq(@azimuth.multiply(-2, -3), 6)
}

test "subtract_function_basic_tests" {
  // 基本减法功能测试
  @azimuth.assert_eq(@azimuth.subtract(10, 5), 5)
  @azimuth.assert_eq(@azimuth.subtract(5, 10), -5)
  @azimuth.assert_eq(@azimuth.subtract(0, 0), 0)
  @azimuth.assert_eq(@azimuth.subtract(15, 5), 10)
}

test "divide_with_ceil_basic_functionality" {
  // 向上取整除法基本功能测试
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, 3), 4)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(9, 3), 3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(1, 2), 1)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(0, 5), 0)
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, 0), 0)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(0, 0), 0)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-10, 3), -3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, -3), 3)
}

test "greet_function_standard_tests" {
  // 问候函数标准测试
  @azimuth.assert_eq_string(@azimuth.greet("World"), "Hello, World!")
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet("测试"), "Hello, 测试!")
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 7
  let b = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 减法逆运算验证
  @azimuth.assert_eq(@azimuth.add(@azimuth.subtract(a, b), b), a)
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let base = 10
  let multiplier = 3
  let offset = 5
  let divisor = 4
  
  // 复合计算：(base * multiplier + offset) / divisor
  let step1 = @azimuth.multiply(base, multiplier)
  let step2 = @azimuth.add(step1, offset)
  let result = @azimuth.divide_with_ceil(step2, divisor)
  
  @azimuth.assert_eq(step1, 30)
  @azimuth.assert_eq(step2, 35)
  @azimuth.assert_eq(result, 9)
}

test "boundary_value_tests" {
  // 边界值测试
  // 零值测试
  @azimuth.assert_eq(@azimuth.add(0, 5), 5)
  @azimuth.assert_eq(@azimuth.multiply(0, 5), 0)
  @azimuth.assert_eq(@azimuth.subtract(5, 0), 5)
  
  // 单位值测试
  @azimuth.assert_eq(@azimuth.add(1, 0), 1)
  @azimuth.assert_eq(@azimuth.multiply(1, 5), 5)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(5, 1), 5)
}

test "real_world_application_test" {
  // 真实应用场景测试 - 购物车计算
  let item_price = 25
  let quantity = 3
  let tax_rate = 10
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = @azimuth.add(subtotal, tax_amount)
  
  @azimuth.assert_eq(subtotal, 75)
  @azimuth.assert_eq(tax_amount, 8)
  @azimuth.assert_eq(total, 83)
}