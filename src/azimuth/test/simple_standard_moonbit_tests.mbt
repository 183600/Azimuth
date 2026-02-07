// 标准 MoonBit 测试用例
// 简洁版本 - 包含基本功能测试

test "basic_addition_functionality" {
  // 基本加法功能测试
  let result1 = @azimuth.add(2, 3)
  if (result1 != 5) { @builtin.abort("Test failed") }
  
  let result2 = @azimuth.add(0, 0)
  if (result2 != 0) { @builtin.abort("Test failed") }
  
  let result3 = @azimuth.add(-5, 5)
  if (result3 != 0) { @builtin.abort("Test failed") }
  
  let result4 = @azimuth.add(100, 200)
  if (result4 != 300) { @builtin.abort("Test failed") }
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  let result1 = @azimuth.multiply(3, 4)
  if (result1 != 12) { @builtin.abort("Test failed") }
  
  let result2 = @azimuth.multiply(0, 5)
  if (result2 != 0) { @builtin.abort("Test failed") }
  
  let result3 = @azimuth.multiply(-2, 3)
  if (result3 != -6) { @builtin.abort("Test failed") }
  
  let result4 = @azimuth.multiply(-2, -3)
  if (result4 != 6) { @builtin.abort("Test failed") }
}

test "subtract_function_basic_tests" {
  // 基本减法功能测试
  let result1 = @azimuth.subtract(10, 5)
  if (result1 != 5) { @builtin.abort("Test failed") }
  
  let result2 = @azimuth.subtract(5, 10)
  if (result2 != -5) { @builtin.abort("Test failed") }
  
  let result3 = @azimuth.subtract(0, 0)
  if (result3 != 0) { @builtin.abort("Test failed") }
  
  let result4 = @azimuth.subtract(15, 5)
  if (result4 != 10) { @builtin.abort("Test failed") }
}

test "divide_with_ceil_basic_functionality" {
  // 向上取整除法基本功能测试
  let result1 = @azimuth.divide_with_ceil(10, 3)
  if (result1 != 4) { @builtin.abort("Test failed") }
  
  let result2 = @azimuth.divide_with_ceil(9, 3)
  if (result2 != 3) { @builtin.abort("Test failed") }
  
  let result3 = @azimuth.divide_with_ceil(1, 2)
  if (result3 != 1) { @builtin.abort("Test failed") }
  
  let result4 = @azimuth.divide_with_ceil(0, 5)
  if (result4 != 0) { @builtin.abort("Test failed") }
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  let result1 = @azimuth.divide_with_ceil(10, 0)
  if (result1 != 0) { @builtin.abort("Test failed") }
  
  let result2 = @azimuth.divide_with_ceil(0, 0)
  if (result2 != 0) { @builtin.abort("Test failed") }
  
  let result3 = @azimuth.divide_with_ceil(-10, 3)
  if (result3 != -3) { @builtin.abort("Test failed") }
  
  let result4 = @azimuth.divide_with_ceil(10, -3)
  if (result4 != 3) { @builtin.abort("Test failed") }
}

test "greet_function_standard_tests" {
  // 问候函数标准测试
  let result1 = @azimuth.greet("World")
  if (result1 != "Hello, World!") { @builtin.abort("Test failed") }
  
  let result2 = @azimuth.greet("MoonBit")
  if (result2 != "Hello, MoonBit!") { @builtin.abort("Test failed") }
  
  let result3 = @azimuth.greet("")
  if (result3 != "Hello, !") { @builtin.abort("Test failed") }
  
  let result4 = @azimuth.greet("测试")
  if (result4 != "Hello, 测试!") { @builtin.abort("Test failed") }
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 7
  let b = 3
  
  // 加法交换律
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  if (result1 != result2) { @builtin.abort("Test failed") }
  
  // 乘法交换律
  let result3 = @azimuth.multiply(a, b)
  let result4 = @azimuth.multiply(b, a)
  if (result3 != result4) { @builtin.abort("Test failed") }
  
  // 减法逆运算验证
  let result5 = @azimuth.subtract(a, b)
  let result6 = @azimuth.add(result5, b)
  if (result6 != a) { @builtin.abort("Test failed") }
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let base = 10
  let multiplier = 3
  let offset = 5
  let divisor = 4
  
  // 复合计算：(base * multiplier + offset) / divisor
  let step1 = @azimuth.multiply(base, multiplier)
  if (step1 != 30) { @builtin.abort("Test failed") }
  
  let step2 = @azimuth.add(step1, offset)
  if (step2 != 35) { @builtin.abort("Test failed") }
  
  let result = @azimuth.divide_with_ceil(step2, divisor)
  if (result != 9) { @builtin.abort("Test failed") }
}

test "boundary_value_tests" {
  // 边界值测试
  // 零值测试
  let result1 = @azimuth.add(0, 5)
  if (result1 != 5) { @builtin.abort("Test failed") }
  
  let result2 = @azimuth.multiply(0, 5)
  if (result2 != 0) { @builtin.abort("Test failed") }
  
  let result3 = @azimuth.subtract(5, 0)
  if (result3 != 5) { @builtin.abort("Test failed") }
  
  // 单位值测试
  let result4 = @azimuth.add(1, 0)
  if (result4 != 1) { @builtin.abort("Test failed") }
  
  let result5 = @azimuth.multiply(1, 5)
  if (result5 != 5) { @builtin.abort("Test failed") }
  
  let result6 = @azimuth.divide_with_ceil(5, 1)
  if (result6 != 5) { @builtin.abort("Test failed") }
}

test "real_world_application_test" {
  // 真实应用场景测试 - 购物车计算
  let item_price = 25
  let quantity = 3
  let tax_rate = 10
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  if (subtotal != 75) { @builtin.abort("Test failed") }
  
  // 计算税费（向上取整）
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  if (tax_amount != 8) { @builtin.abort("Test failed") }
  
  // 计算总价
  let total = @azimuth.add(subtotal, tax_amount)
  if (total != 83) { @builtin.abort("Test failed") }
}