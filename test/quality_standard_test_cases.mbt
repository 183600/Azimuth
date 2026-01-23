// 高质量标准 MoonBit 测试用例
// 针对 azimuth 库的全面测试，包含边界情况和实际应用场景

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  assert_eq(0, add(0, 0))
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, multiply(100, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, multiply(-1, -1))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(34, divide_with_ceil(100, 3))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 特殊情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_various_scenarios" {
  // 测试问候函数的各种场景
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  let c = 19
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_business_calculations" {
  // 实际业务计算场景
  let product_price = 89
  let quantity = 12
  let tax_rate = 8
  let shipping_cost = 25
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(add(subtotal, tax_amount), shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(1068, subtotal)
  assert_eq(86, tax_amount)
  assert_eq(1179, total_before_discount)
  assert_eq(1129, final_total)
}

test "complex_nested_calculations" {
  // 复杂嵌套计算
  let x = 15
  let y = 8
  let z = 6
  
  // 计算 (x + y) * z - x / y
  let result1 = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  
  // 计算 x * (y + z) + y / z
  let result2 = add(multiply(x, add(y, z)), divide_with_ceil(y, z))
  
  // 计算 (x * y) + (y * z) + (x * z)
  let result3 = add(add(multiply(x, y), multiply(y, z)), multiply(x, z))
  
  assert_eq(136, result1)  // (15 + 8) * 6 - ceil(15/8) = 23*6 - 2 = 138 - 2 = 136
  assert_eq(212, result2)  // 15 * (8 + 6) + ceil(8/6) = 15*14 + 2 = 210 + 2 = 212
  assert_eq(258, result3)  // 15*8 + 8*6 + 15*6 = 120 + 48 + 90 = 258
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let big_num = 1000
  assert_eq(2000, add(big_num, big_num))
  assert_eq(1000000, multiply(big_num, big_num))
  
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
}

test "performance_and_efficiency_calculations" {
  // 性能和效率计算
  let base_operations = 1000
  let optimization_factor = 5
  let overhead_percentage = 20
  
  // 计算优化后的操作数
  let optimized_operations = multiply(base_operations, optimization_factor)
  
  // 计算开销
  let overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = add(optimized_operations, -overhead)
  
  // 计算性能提升倍数
  let performance_improvement = divide_with_ceil(actual_performance, base_operations)
  
  assert_eq(5000, optimized_operations)
  assert_eq(1000, overhead)
  assert_eq(4000, actual_performance)
  assert_eq(4, performance_improvement)
}

test "comprehensive_integration_test" {
  // 综合集成测试
  // 模拟一个完整的电商订单处理流程
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 45
  let item2_quantity = 2
  let item3_price = 15
  let item3_quantity = 4
  
  let tax_rate = 8
  let shipping_per_item = 5
  let discount_amount = 30
  
  // 计算各项商品总价
  let item1_total = multiply(item1_price, item1_quantity)
  let item2_total = multiply(item2_price, item2_quantity)
  let item3_total = multiply(item3_price, item3_quantity)
  
  // 计算商品总价
  let subtotal = add(add(item1_total, item2_total), item3_total)
  
  // 计算总运费
  let total_items = add(add(item1_quantity, item2_quantity), item3_quantity)
  let total_shipping = multiply(shipping_per_item, total_items)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, total_shipping), tax_amount)
  
  // 应用折扣
  let final_total = add(pre_discount_total, -discount_amount)
  
  // 生成订单确认消息
  let confirmation_message = greet("订单已确认")
  
  assert_eq(75, item1_total)
  assert_eq(90, item2_total)
  assert_eq(60, item3_total)
  assert_eq(225, subtotal)
  assert_eq(45, total_shipping)
  assert_eq(18, tax_amount)
  assert_eq(288, pre_discount_total)
  assert_eq(258, final_total)
  assert_eq_string("Hello, 订单已确认!", confirmation_message)
}