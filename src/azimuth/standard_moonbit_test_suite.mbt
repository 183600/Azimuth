// 标准 MoonBit 测试套件
// 包含 10 个高质量测试用例，覆盖 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  inspect(add(5, 3), content="8")
  inspect(multiply(4, 6), content="24")
  inspect(subtract(10, 7), content="3")
  inspect(divide_with_ceil(10, 3), content="4")
  inspect(divide_with_ceil(9, 3), content="3")
}

test "negative_number_operations" {
  // 负数运算测试
  inspect(add(-5, 3), content="-2")
  inspect(add(5, -3), content="2")
  inspect(multiply(-4, 6), content="-24")
  inspect(multiply(4, -6), content="-24")
  inspect(multiply(-4, -6), content="24")
  inspect(subtract(-10, 7), content="-17")
  inspect(subtract(10, -7), content="17")
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  azimuth::assert_eq(azimuth::divide_with_ceil(0, 5), 0)
  azimuth::assert_eq(azimuth::divide_with_ceil(0, 1), 0)
  azimuth::assert_eq(azimuth::divide_with_ceil(0, 0), 0)
  azimuth::assert_eq(azimuth::divide_with_ceil(10, 0), 0)
  azimuth::assert_eq(azimuth::divide_with_ceil(-10, 3), -3)
  azimuth::assert_eq(azimuth::divide_with_ceil(10, -3), 3)
  azimuth::assert_eq(azimuth::divide_with_ceil(-10, -3), 4)
}

test "string_greeting_function" {
  // 字符串问候函数测试
  azimuth::assert_eq_string(azimuth::greet("World"), "Hello, World!")
  azimuth::assert_eq_string(azimuth::greet("MoonBit"), "Hello, MoonBit!")
  azimuth::assert_eq_string(azimuth::greet(""), "Hello, !")
  azimuth::assert_eq_string(azimuth::greet("中文"), "Hello, 中文!")
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial = 100
  let step1 = azimuth::multiply(initial, 3)  // 300
  let step2 = azimuth::add(step1, 15)        // 315
  let step3 = azimuth::subtract(step2, 5)    // 310
  let step4 = azimuth::divide_with_ceil(step3, 4)  // 78
  
  azimuth::assert_eq(step1, 300)
  azimuth::assert_eq(step2, 315)
  azimuth::assert_eq(step3, 310)
  azimuth::assert_eq(step4, 78)
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 17
  let y = 23
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(x, y), azimuth::add(y, x))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(x, y), azimuth::multiply(y, x))
  
  // 加法单位元
  azimuth::assert_eq(azimuth::add(x, 0), x)
  
  // 乘法单位元
  azimuth::assert_eq(azimuth::multiply(x, 1), x)
  
  // 减法的逆运算
  azimuth::assert_eq(azimuth::add(azimuth::subtract(x, y), y), x)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值测试
  azimuth::assert_eq(azimuth::add(0, 0), 0)
  azimuth::assert_eq(azimuth::multiply(0, 5), 0)
  azimuth::assert_eq(azimuth::subtract(0, 0), 0)
  
  // 单位值测试
  azimuth::assert_eq(azimuth::add(1, 0), 1)
  azimuth::assert_eq(azimuth::multiply(1, 5), 5)
  azimuth::assert_eq(azimuth::subtract(1, 0), 1)
  azimuth::assert_eq(azimuth::divide_with_ceil(5, 1), 5)
  
  // 负一测试
  azimuth::assert_eq(azimuth::add(-1, 1), 0)
  azimuth::assert_eq(azimuth::multiply(-1, 5), -5)
  azimuth::assert_eq(azimuth::subtract(0, -1), 1)
}

test "large_number_operations" {
  // 大数运算测试
  azimuth::assert_eq(azimuth::add(1000000, 1000000), 2000000)
  azimuth::assert_eq(azimuth::multiply(10000, 10000), 100000000)
  azimuth::assert_eq(azimuth::subtract(1000000, 500000), 500000)
  azimuth::assert_eq(azimuth::divide_with_ceil(1000000, 3), 333334)
}

test "precision_operations" {
  // 精度运算测试
  azimuth::assert_eq(azimuth::divide_with_ceil(1, 2), 1)
  azimuth::assert_eq(azimuth::divide_with_ceil(2, 3), 1)
  azimuth::assert_eq(azimuth::divide_with_ceil(3, 4), 1)
  azimuth::assert_eq(azimuth::divide_with_ceil(100, 99), 2)
  azimuth::assert_eq(azimuth::divide_with_ceil(99, 100), 1)
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算测试
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  
  // 计算小计
  let subtotal1 = azimuth::multiply(item1_price, item1_qty)  // 75
  let subtotal2 = azimuth::multiply(item2_price, item2_qty)  // 30
  
  // 计算总价
  let total = azimuth::add(subtotal1, subtotal2)  // 105
  
  // 计算税费（10%，向上取整）
  let tax = azimuth::divide_with_ceil(azimuth::multiply(total, 10), 100)  // 11
  
  // 计算最终价格
  let final_total = azimuth::add(total, tax)  // 116
  
  azimuth::assert_eq(subtotal1, 75)
  azimuth::assert_eq(subtotal2, 30)
  azimuth::assert_eq(total, 105)
  azimuth::assert_eq(tax, 11)
  azimuth::assert_eq(final_total, 116)
}