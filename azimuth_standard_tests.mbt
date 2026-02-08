// Azimuth 项目标准 MoonBit 测试用例
// 测试核心库函数的功能和边界情况

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  assert_eq(5, azimuth.add(2, 3))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(-1, azimuth.add(-3, 2))
  assert_eq(100, azimuth.add(50, 50))
  assert_eq(-100, azimuth.add(-50, -50))
}

test "multiply_function_comprehensive" {
  // 测试 multiply 函数的综合情况
  assert_eq(6, azimuth.multiply(2, 3))
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(-15, azimuth.multiply(-3, 5))
  assert_eq(15, azimuth.multiply(-3, -5))
  assert_eq(10000, azimuth.multiply(100, 100))
}

test "subtract_function_edge_cases" {
  // 测试 subtract 函数的边界情况
  assert_eq(5, azimuth.subtract(10, 5))
  assert_eq(-5, azimuth.subtract(5, 10))
  assert_eq(0, azimuth.subtract(15, 15))
  assert_eq(20, azimuth.subtract(10, -10))
  assert_eq(-20, azimuth.subtract(-10, 10))
}

test "divide_with_ceil_function_various_scenarios" {
  // 测试 divide_with_ceil 函数的各种情况
  // 正数除法
  assert_eq(4, azimuth.divide_with_ceil(10, 3))
  assert_eq(3, azimuth.divide_with_ceil(9, 3))
  assert_eq(1, azimuth.divide_with_ceil(1, 5))
  
  // 零值情况
  assert_eq(0, azimuth.divide_with_ceil(0, 10))
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 负数情况
  assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  assert_eq(-4, azimuth.divide_with_ceil(10, -3))
  assert_eq(4, azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_comprehensive" {
  // 测试 greet 函数的综合情况
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
  assert_eq_string("Hello, 中文!", azimuth.greet("中文"))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let initial = 10
  let step1 = azimuth.add(initial, 5)        // 15
  let step2 = azimuth.multiply(step1, 2)     // 30
  let step3 = azimuth.subtract(step2, 8)     // 22
  let step4 = azimuth.divide_with_ceil(step3, 4)  // 6
  
  assert_eq(step1, 15)
  assert_eq(step2, 30)
  assert_eq(step3, 22)
  assert_eq(step4, 6)
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 加法单位元
  assert_eq(azimuth.add(a, 0), a)
  
  // 乘法单位元
  assert_eq(azimuth.multiply(a, 1), a)
  
  // 减法的逆运算
  assert_eq(azimuth.add(azimuth.subtract(a, b), b), a)
}

test "boundary_value_analysis" {
  // 测试边界值分析
  // 零值边界
  assert_eq(azimuth.add(0, 0), 0)
  assert_eq(azimuth.multiply(0, 100), 0)
  assert_eq(azimuth.subtract(0, 0), 0)
  assert_eq(azimuth.divide_with_ceil(0, 10), 0)
  
  // 单位值边界
  assert_eq(azimuth.add(1, 0), 1)
  assert_eq(azimuth.multiply(1, 100), 100)
  assert_eq(azimuth.subtract(1, 0), 1)
  assert_eq(azimuth.divide_with_ceil(100, 1), 100)
  
  // 负单位边界
  assert_eq(azimuth.add(-1, 1), 0)
  assert_eq(azimuth.multiply(-1, 100), -100)
  assert_eq(azimuth.subtract(0, -1), 1)
  assert_eq(azimuth.divide_with_ceil(-100, 1), -100)
}

test "real_world_shopping_cart_scenario" {
  // 测试真实世界购物车场景
  // 商品价格和数量
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  
  // 计算小计
  let item1_total = azimuth.multiply(item1_price, item1_quantity)  // 75
  let item2_total = azimuth.multiply(item2_price, item2_quantity)  // 30
  
  // 计算总计
  let subtotal = azimuth.add(item1_total, item2_total)  // 105
  
  // 计算税费（10%，向上取整）
  let tax = azimuth.divide_with_ceil(azimuth.multiply(subtotal, 10), 100)  // 11
  
  // 计算总价
  let total = azimuth.add(subtotal, tax)  // 116
  
  assert_eq(item1_total, 75)
  assert_eq(item2_total, 30)
  assert_eq(subtotal, 105)
  assert_eq(tax, 11)
  assert_eq(total, 116)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  // 除零错误处理
  let result1 = azimuth.divide_with_ceil(50, 0)
  let result2 = azimuth.divide_with_ceil(0, 0)
  let result3 = azimuth.divide_with_ceil(-50, 0)
  
  // 验证错误情况下的默认行为
  assert_eq(result1, 0)
  assert_eq(result2, 0)
  assert_eq(result3, 0)
  
  // 大数运算
  let large_num = 100000
  let large_sum = azimuth.add(large_num, large_num)
  let large_product = azimuth.multiply(large_num, 2)
  
  assert_eq(large_sum, 200000)
  assert_eq(large_product, 200000)
  
  // 字符串边界情况
  assert_eq_string(azimuth.greet(" "), "Hello,  !")
  assert_eq_string(azimuth.greet("!@#$"), "Hello, !@#$!")
}