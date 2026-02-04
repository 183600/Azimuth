// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "basic_arithmetic_combinations" {
  // 测试基本算术运算的组合
  let a = 15
  let b = 7
  let c = 3
  
  // 测试 (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(66, result1)  // (15 + 7) * 3 = 22 * 3 = 66
  
  // 测试 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(108, result2)  // 15 * 7 + 3 = 105 + 3 = 108
  
  // 测试 a - b * c
  let result3 = subtract(a, multiply(b, c))
  assert_eq(-6, result3)  // 15 - 7 * 3 = 15 - 21 = -6
}

test "boundary_value_testing" {
  // 测试边界值
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  
  // 单值测试
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  
  // 负一测试
  assert_eq(-1, add(-1, 0))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算（使用较小数字避免溢出）
  let large_num = 10000
  assert_eq(20000, add(large_num, large_num))
  assert_eq(100000000, multiply(large_num, large_num))
  
  // 负数边界
  assert_eq(-20000, add(-large_num, -large_num))
  assert_eq(-100000000, multiply(-large_num, large_num))
}

test "unicode_string_processing" {
  // 测试 Unicode 字符串处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 12
  let y = 8
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
  
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(x, add(x, 0))
  
  // 单位元性质
  assert_eq(x, multiply(x, 1))
}

test "business_logic_calculation" {
  // 测试业务逻辑计算
  // 订单计算
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8
  let shipping_cost = 25
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价（含运费）
  let total_with_shipping = add(add(subtotal, tax_amount), shipping_cost)
  
  // 应用折扣
  let final_total = add(total_with_shipping, -discount)
  
  assert_eq(540, subtotal)
  assert_eq(44, tax_amount)  // ceil(540 * 8 / 100) = ceil(43.2) = 44
  assert_eq(609, total_with_shipping)  // 540 + 44 + 25 = 609
  assert_eq(559, final_total)  // 609 - 50 = 559
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let base = 10
  let multiplier = 5
  let offset = 15
  
  // 模拟幂运算：base^3
  let power_result = multiply(multiply(base, base), base)
  
  // 复杂表达式：(base^3 * multiplier) + offset
  let complex_result = add(multiply(power_result, multiplier), offset)
  
  // 嵌套表达式：((base + offset) * multiplier) / base
  let nested_result = divide_with_ceil(multiply(add(base, offset), multiplier), base)
  
  assert_eq(1000, power_result)  // 10^3 = 1000
  assert_eq(5015, complex_result)  // 1000 * 5 + 15 = 5015
  assert_eq(13, nested_result)  // ceil((10 + 15) * 5 / 10) = ceil(12.5) = 13
}

test "algorithmic_implementation" {
  // 测试算法实现
  // 模拟查找最大值（直接使用条件表达式）
  let max_val = 15  // 初始值
  
  // 模拟查找过程
  let current_max1 = if max_val < 8 { 8 } else { max_val }
  let current_max2 = if current_max1 < 23 { 23 } else { current_max1 }
  let current_max3 = if current_max2 < 4 { 4 } else { current_max2 }
  let current_max4 = if current_max3 < 19 { 19 } else { current_max3 }
  
  assert_eq(23, current_max4)
  
  // 模拟求和算法
  let sum_result = add(add(add(add(15, 8), 23), 4), 19)
  assert_eq(69, sum_result)
  
  // 模拟平均值计算（向上取整）
  let average = divide_with_ceil(sum_result, 5)
  assert_eq(14, average)  // ceil(69/5) = ceil(13.8) = 14
}

test "data_structure_operations" {
  // 测试数据结构操作
  // 模拟栈操作
  let stack_top = 10
  let stack_push = 20
  let stack_pop = 10
  
  // Push 操作
  let new_top = stack_push
  
  // Pop 操作
  let popped_value = stack_top
  let final_top = stack_pop
  
  assert_eq(20, new_top)
  assert_eq(10, popped_value)
  assert_eq(10, final_top)
  
  // 模拟队列操作
  let queue_front = 5
  let queue_rear = 15
  let queue_size = add(queue_rear, -queue_front)
  
  assert_eq(10, queue_size)
  
  // 模拟数组访问
  let array_index = 3
  let array_value = multiply(array_index, 7)
  assert_eq(21, array_value)
}

test "real_world_application_scenario" {
  // 测试实际应用场景
  // 电商库存管理
  let product_id = 1001
  let initial_stock = 500
  let daily_sales = 25
  let restock_amount = 200
  let days_to_simulate = 7
  
  // 计算总销售量
  let total_sales = multiply(daily_sales, days_to_simulate)
  
  // 计算剩余库存
  let remaining_stock = subtract(initial_stock, total_sales)
  
  // 补货后的库存
  let final_stock = add(remaining_stock, restock_amount)
  
  // 计算库存周转率
  let turnover_rate = divide_with_ceil(multiply(total_sales, 100), initial_stock)
  
  assert_eq(175, total_sales)  // 25 * 7 = 175
  assert_eq(325, remaining_stock)  // 500 - 175 = 325
  assert_eq(525, final_stock)  // 325 + 200 = 525
  assert_eq(35, turnover_rate)  // ceil(175 * 100 / 500) = ceil(35) = 35
  
  // 测试产品标识生成
  let product_label = greet("Product-1001")
  assert_eq_string("Hello, Product-1001!", product_label)
}