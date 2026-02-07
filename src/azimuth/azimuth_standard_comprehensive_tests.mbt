// Azimuth 项目标准综合测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(add(5, 3), 8)
  assert_eq(multiply(4, 6), 24)
  assert_eq(subtract(10, 7), 3)
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
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

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(0, 1), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(99, 100), 1)
}

test "string_greet_function" {
  // 字符串问候函数测试
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
  assert_eq_string(greet("!@#"), "Hello, !@#!")
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial = 100
  let step1 = multiply(initial, 3)  // 300
  let step2 = add(step1, 15)        // 315
  let step3 = subtract(step2, 5)    // 310
  let step4 = divide_with_ceil(step3, 4)  // 78
  
  assert_eq(step1, 300)
  assert_eq(step2, 315)
  assert_eq(step3, 310)
  assert_eq(step4, 78)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 17
  let y = 23
  let z = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值测试
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  assert_eq(divide_with_ceil(0, 10), 0)
  
  // 单位值测试
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 负单位测试
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 5), -5)
  assert_eq(subtract(0, -1), 1)
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(add(1000000, 1000000), 2000000)
  assert_eq(multiply(10000, 10000), 100000000)
  assert_eq(subtract(1000000, 500000), 500000)
  assert_eq(divide_with_ceil(1000000, 3), 333334)
  assert_eq(divide_with_ceil(999999, 1000000), 1)
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算测试
  // 商品价格和数量
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  
  // 计算小计
  let item1_total = multiply(item1_price, item1_quantity)  // 75
  let item2_total = multiply(item2_price, item2_quantity)  // 30
  let subtotal = add(item1_total, item2_total)  // 105
  
  // 计算税费（10%向上取整）
  let tax = divide_with_ceil(multiply(subtotal, 10), 100)  // 11
  let total = add(subtotal, tax)  // 116
  
  assert_eq(item1_total, 75)
  assert_eq(item2_total, 30)
  assert_eq(subtotal, 105)
  assert_eq(tax, 11)
  assert_eq(total, 116)
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  // 除零错误处理
  assert_eq(divide_with_ceil(50, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(-50, 0), 0)
  
  // 大数溢出测试
  let large_num = 1000000
  assert_eq(add(large_num, large_num), 2000000)
  assert_eq(multiply(large_num, 2), 2000000)
  
  // 精度测试
  assert_eq(divide_with_ceil(1, 1000000), 1)
  assert_eq(divide_with_ceil(999999, 1000000), 1)
  assert_eq(divide_with_ceil(1000001, 1000000), 2)
}