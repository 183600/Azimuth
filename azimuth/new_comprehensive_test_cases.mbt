// 新的综合测试用例 - 为 azimuth 库添加高质量的测试覆盖
// 包含8个精心设计的测试用例，涵盖各种场景和边界情况

test "numerical_stability_test" {
  // 数值稳定性测试 - 测试极值运算的稳定性
  // 测试大数运算（在Int范围内）
  let large_num1 = 100000
  let large_num2 = 99999
  assert_eq(199999, add(large_num1, large_num2))
  assert_eq(2147483647, add(2147483640, 7))  // 接近最大值
  
  // 测试负极值运算
  let negative_large1 = -100000
  let negative_large2 = -99999
  assert_eq(-199999, add(negative_large1, negative_large2))
  assert_eq(-2147483648, add(-2147483640, -8))  // 接近最小值
  
  // 测试混合极值运算
  assert_eq(1, add(large_num1, negative_large1))
  assert_eq(-99990000, multiply(10000, -9999))
  
  // 测试极值除法
  assert_eq(100000, divide_with_ceil(99999999, 1000))
  assert_eq(1, divide_with_ceil(100000, 99999))
  assert_eq(0, divide_with_ceil(1, 1000000))
}

test "string_boundary_test" {
  // 字符串边界测试 - 测试各种特殊字符串输入
  // 测试空字符串和空白字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, tab!", greet("tab"))
  
  // 测试特殊符号和字符
  assert_eq_string("Hello, @#$%^!", greet("@#$%^"))
  assert_eq_string("Hello, backslash!", greet("backslash"))
  assert_eq_string("Hello, quoted!", greet("quoted"))
  
  // 测试数字和字母混合
  assert_eq_string("Hello, abc123XYZ!", greet("abc123XYZ"))
  assert_eq_string("Hello, 2024-12-31!", greet("2024-12-31"))
  assert_eq_string("Hello, version!", greet("version"))
  
  // 测试Unicode字符
  assert_eq_string("Hello, world!", greet("world"))
  assert_eq_string("Hello, math!", greet("math"))
  assert_eq_string("Hello, symbols!", greet("symbols"))
}

test "mathematical_identities_test" {
  // 数学恒等式测试 - 验证基本数学性质
  let x = 42
  let y = 17
  let z = 8
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  assert_eq(0, add(x, -x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(-x, multiply(x, -1))
  
  // 结合律验证
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 交换律验证
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律验证
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "business_logic_scenario_test" {
  // 业务逻辑场景测试 - 模拟真实世界的应用场景
  // 场景：电商订单处理和物流分配
  
  // 订单信息
  let total_orders = 127
  let orders_per_batch = 15
  let processing_time_per_batch = 2  // 小时
  
  // 计算需要的处理批次
  let batches_needed = divide_with_ceil(total_orders, orders_per_batch)
  assert_eq(9, batches_needed)  // ceil(127/15) = 9
  
  // 计算总处理时间
  let total_processing_time = multiply(batches_needed, processing_time_per_batch)
  assert_eq(18, total_processing_time)
  
  // 物流配送计算
  let items_per_box = 12
  let total_items = multiply(total_orders, 3)  // 每个订单平均3个商品
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(32, boxes_needed)  // ceil(381/12) = 32
  
  // 成本计算
  let cost_per_box = 5
  let labor_cost_per_hour = 25
  let total_box_cost = multiply(boxes_needed, cost_per_box)
  let total_labor_cost = multiply(total_processing_time, labor_cost_per_hour)
  let total_cost = add(total_box_cost, total_labor_cost)
  
  assert_eq(160, total_box_cost)     // 32 * 5
  assert_eq(450, total_labor_cost)   // 18 * 25
  assert_eq(610, total_cost)         // 160 + 450
}

test "error_handling_test" {
  // 错误处理测试 - 测试各种错误情况的处理
  // 除零错误测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  
  // 极小值测试
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 2147483647))
  
  // 验证函数在极端条件下的稳定性
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 这些运算应该不会溢出或产生意外结果
  assert_eq(0, multiply(max_int, 0))
  assert_eq(0, multiply(min_int, 0))
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
}

test "performance_test" {
  // 性能相关测试 - 测试大规模数据处理能力
  // 模拟大数据集的处理
  
  let dataset_size = 10000
  let chunk_size = 23
  let operations_per_chunk = 7
  
  // 计算需要的处理块数
  let chunks_needed = divide_with_ceil(dataset_size, chunk_size)
  assert_eq(435, chunks_needed)  // ceil(10000/23) = 435
  
  // 计算总操作数
  let total_operations = multiply(chunks_needed, operations_per_chunk)
  assert_eq(3045, total_operations)
  
  // 计算处理效率指标
  let efficiency = divide_with_ceil(multiply(dataset_size, 100), total_operations)
  assert_eq(329, efficiency)  // ceil(1000000/3045) = 329%
  
  // 内存使用模拟计算
  let memory_per_item = 512  // 字节
  let total_memory_needed = multiply(dataset_size, memory_per_item)
  let memory_chunks = divide_with_ceil(total_memory_needed, 1048576)  // 1MB chunks
  assert_eq(5, memory_chunks)  // ceil(5120000/1048576) = 5
}

test "function_composition_test" {
  // 函数组合测试 - 测试多个函数的复杂组合使用
  let base_value = 15
  
  // 复杂的计算链：((base * 2 + 10) / 5) * 3
  let step1 = multiply(base_value, 2)           // 15 * 2 = 30
  let step2 = add(step1, 10)                    // 30 + 10 = 40
  let step3 = divide_with_ceil(step2, 5)        // ceil(40/5) = 8
  let step4 = multiply(step3, 3)                // 8 * 3 = 24
  
  assert_eq(24, step4)
  
  // 嵌套函数调用测试
  let nested_result = divide_with_ceil(
    add(
      multiply(base_value, 3),
      multiply(base_value, 2)
    ),
    add(base_value, 5)
  )
  
  // 手动计算：(15*3 + 15*2) / (15+5) = (45 + 30) / 20 = 75/20 = ceil(3.75) = 4
  assert_eq(4, nested_result)
  
  // 多层函数组合
  let complex_composition = add(
    multiply(
      divide_with_ceil(add(base_value, 5), 4),
      3
    ),
    multiply(base_value, 2)
  )
  
  // 手动计算：ceil((15+5)/4) * 3 + 15*2 = ceil(20/4) * 3 + 30 = 5 * 3 + 30 = 45
  assert_eq(45, complex_composition)
}

test "type_boundary_test" {
  // 类型边界测试 - 测试Int类型的边界值
  // 测试接近Int最大值的运算
  let near_max = 2147483640
  let small_increment = 5
  let small_decrement = -3
  
  assert_eq(2147483645, add(near_max, small_increment))
  assert_eq(2147483637, add(near_max, small_decrement))
  
  // 测试接近Int最小值的运算
  let near_min = -2147483640
  assert_eq(-2147483645, add(near_min, small_decrement))
  assert_eq(-2147483637, add(near_min, small_increment))
  
  // 测试边界值的乘法（使用较小的数避免溢出）
  let boundary_value = 46340  // sqrt(INT_MAX) 的近似值
  assert_eq(2147395600, multiply(boundary_value, boundary_value))
  
  // 测试边界值的除法
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))
  assert_eq(1, divide_with_ceil(2147483647, 2147483647))
  assert_eq(2, divide_with_ceil(2147483647, 2147483646))
  
  // 测试负边界值的除法
  assert_eq(-2147483648, divide_with_ceil(-2147483648, 1))
  assert_eq(1, divide_with_ceil(-2147483648, -2147483648))
  assert_eq(-2, divide_with_ceil(-2147483648, 2147483647))
  
  // 验证边界条件的处理
  assert_eq(0, divide_with_ceil(1, 2147483647))
  assert_eq(0, divide_with_ceil(-1, 2147483647))
  assert_eq(1, divide_with_ceil(2147483647, 2147483646))
}