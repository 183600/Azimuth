// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法编写
// 包含8个高质量测试用例，覆盖不同场景

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  assert_eq(2147483646, add(2147483645, 1))  // 接近Int最大值
  assert_eq(-2147483647, add(-2147483646, -1))  // 接近Int最小值
  assert_eq(1, add(-1000, 1001))  // 大数相加
  assert_eq(0, add(1000, -1000))  // 正负相消
}

test "multiply_function_precision" {
  // 测试乘法函数的精度和大数处理
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
  assert_eq(1000000, multiply(-1000, -1000))
  assert_eq(999999, multiply(999, 1001))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边缘情况
  assert_eq_string("Hello, \n\r\t!", greet("\n\r\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", greet("'single'"))
  assert_eq_string("Hello, \\escaped\\!", greet("\\escaped\\"))
  assert_eq_string("Hello, null!", greet("null"))
  assert_eq_string("Hello, undefined!", greet("undefined"))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(-1, 2))
  assert_eq(-1, divide_with_ceil(-3, 2))
  assert_eq(1000000, divide_with_ceil(999999000, 1000))
}

test "real_world_inventory_management" {
  // 真实世界库存管理场景测试
  let total_products = 1234
  let box_capacity = 45
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(28, boxes_needed)  // ceil(1234/45) = 28
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(1260, total_capacity)
  
  // 计算剩余空间
  let unused_space = subtract(total_capacity, total_products)
  assert_eq(26, unused_space)
}

test "financial_loan_calculation" {
  // 金融贷款计算场景
  let loan_amount = 50000
  let monthly_payment = 1850
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  assert_eq(28, months_needed)  // ceil(50000/1850) = 28
  
  // 计算总还款额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(51800, total_payment)
  
  // 计算最后一个月还款额
  let final_payment = subtract(total_payment, multiply(months_needed - 1, monthly_payment))
  assert_eq(300, final_payment)
}

test "complex_nested_operations" {
  // 复杂嵌套运算测试
  let base = 15
  
  // 嵌套运算：ceil((base*3 + base*2) / (base + ceil(base/4)))
  let inner_div = divide_with_ceil(base, 4)  // ceil(15/4) = 4
  let numerator = add(multiply(base, 3), multiply(base, 2))  // 15*3 + 15*2 = 75
  let denominator = add(base, inner_div)  // 15 + 4 = 19
  let result = divide_with_ceil(numerator, denominator)  // ceil(75/19) = 4
  
  assert_eq(4, result)
  
  // 另一个复杂运算链
  let chain_result = divide_with_ceil(
    multiply(
      add(base, 5),  // 20
      divide_with_ceil(base, 3)  // ceil(15/3) = 5
    ),
    subtract(base, 3)  // 12
  )
  assert_eq(9, chain_result)  // ceil(20*5/12) = ceil(100/12) = 9
}

test "data_processing_pipeline" {
  // 数据处理流水线测试
  // 场景：批量数据处理
  // 使用单独的变量而不是数组来避免数组导入问题
  
  // 第一批数据
  let batch_1_total = 150
  let batch_1_size = 25
  let rounds_1 = divide_with_ceil(batch_1_total, batch_1_size)  // ceil(150/25) = 6
  
  // 第二批数据
  let batch_2_total = 89
  let batch_2_size = 17
  let rounds_2 = divide_with_ceil(batch_2_total, batch_2_size)  // ceil(89/17) = 6
  
  // 第三批数据
  let batch_3_total = 267
  let batch_3_size = 43
  let rounds_3 = divide_with_ceil(batch_3_total, batch_3_size)  // ceil(267/43) = 7
  
  // 第四批数据
  let batch_4_total = 41
  let batch_4_size = 8
  let rounds_4 = divide_with_ceil(batch_4_total, batch_4_size)  // ceil(41/8) = 6
  
  // 计算总轮次
  let total_rounds = add(add(rounds_1, rounds_2), add(rounds_3, rounds_4))
  assert_eq(25, total_rounds)
  
  // 计算总处理能力
  let capacity_1 = multiply(rounds_1, batch_1_size)  // 6*25 = 150
  let capacity_2 = multiply(rounds_2, batch_2_size)  // 6*17 = 102
  let capacity_3 = multiply(rounds_3, batch_3_size)  // 7*43 = 301
  let capacity_4 = multiply(rounds_4, batch_4_size)  // 6*8 = 48
  
  let total_capacity = add(add(capacity_1, capacity_2), add(capacity_3, capacity_4))
  assert_eq(601, total_capacity)
  
  // 计算利用率
  let total_data = add(add(batch_1_total, batch_2_total), add(batch_3_total, batch_4_total))
  let utilization = divide_with_ceil(multiply(total_data, 100), total_capacity)
  assert_eq(91, utilization)  // ceil(547*100/601) = ceil(91%) = 91
}

// 辅助函数
fn subtract(a : Int, b : Int) -> Int {
  add(a, -b)
}