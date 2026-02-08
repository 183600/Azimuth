// 高质量 MoonBit 测试用例
// 包含10个专业级测试用例，覆盖核心功能和边界情况

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(add(5, 3), 8)
  assert_eq(multiply(4, 6), 24)
  assert_eq(subtract(10, 7), 3)
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
}

test "string_operations" {
  // 字符串操作测试
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
}

test "division_edge_cases" {
  // 除法边界情况测试
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(0, 1), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(add(-5, 3), -2)
  assert_eq(add(5, -3), 2)
  assert_eq(add(-5, -3), -8)
  assert_eq(multiply(-4, 6), -24)
  assert_eq(multiply(4, -6), -24)
  assert_eq(multiply(-4, -6), 24)
  assert_eq(subtract(-10, 7), -17)
  assert_eq(subtract(10, -7), 17)
  assert_eq(subtract(-10, -7), -3)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试零值
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 测试单位值
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 测试负一
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 5), -5)
  assert_eq(subtract(0, -1), 1)
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 7
  let y = 3
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
  
  // 减法的逆运算
  assert_eq(add(subtract(x, y), y), x)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial_value = 100
  let factor1 = 3
  let factor2 = 2
  let offset = 15
  let divisor = 4
  
  // 计算流水线
  let step1 = multiply(initial_value, factor1)  // 100 * 3 = 300
  let step2 = add(step1, offset)                // 300 + 15 = 315
  let step3 = subtract(step2, factor2)          // 315 - 2 = 313
  let step4 = divide_with_ceil(step3, divisor)  // ceil(313/4) = 79
  
  // 验证每一步
  assert_eq(step1, 300)
  assert_eq(step2, 315)
  assert_eq(step3, 313)
  assert_eq(step4, 79)
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(add(1000000, 1000000), 2000000)
  assert_eq(multiply(10000, 10000), 100000000)
  assert_eq(subtract(1000000, 500000), 500000)
  assert_eq(divide_with_ceil(1000000, 3), 333334)
}

test "precision_operations" {
  // 精度运算测试
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(2, 3), 1)
  assert_eq(divide_with_ceil(3, 4), 1)
  assert_eq(divide_with_ceil(100, 99), 2)
  assert_eq(divide_with_ceil(99, 100), 1)
}

test "real_world_scenario" {
  // 真实世界场景测试 - 购物车计算
  // 商品价格和数量
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  
  // 计算各商品小计
  let item1_subtotal = multiply(item1_price, item1_quantity)  // 25 * 3 = 75
  let item2_subtotal = multiply(item2_price, item2_quantity)  // 15 * 2 = 30
  
  // 计算商品总计
  let items_total = add(item1_subtotal, item2_subtotal)  // 75 + 30 = 105
  
  // 计算税费（假设税率10%，向上取整）
  let tax_rate = 10
  let tax_amount = divide_with_ceil(multiply(items_total, tax_rate), 100)  // ceil(105 * 10 / 100) = 11
  
  // 计算总价
  let grand_total = add(items_total, tax_amount)  // 105 + 11 = 116
  
  // 验证计算结果
  assert_eq(item1_subtotal, 75)
  assert_eq(item2_subtotal, 30)
  assert_eq(items_total, 105)
  assert_eq(tax_amount, 11)
  assert_eq(grand_total, 116)
}