// Azimuth 项目高级 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例
// 覆盖边界情况、性能测试和实际应用场景

test "extreme_boundary_conditions" {
  // 测试极值边界条件
  // 接近 Int 最大值和最小值的运算
  assert_eq(2147483646, add(2147483645, 1))
  assert_eq(-2147483647, add(-2147483646, -1))
  assert_eq(2147483646, multiply(46340, 46341))  // 接近 sqrt(Int.max)
  assert_eq(1, divide_with_ceil(2147483646, 2147483646))
  assert_eq(0, divide_with_ceil(-2147483647, 2147483646))
}

test "financial_calculations" {
  // 金融计算场景测试
  // 贷款分期计算
  let loan_amount = 250000
  let monthly_payment = 8500
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  assert_eq(30, months_needed)  // ceil(250000/8500) = 30
  
  // 验证总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(255000, total_payment)
  
  // 计算最后一期实际支付金额
  let final_payment = subtract(loan_amount, multiply(months_needed - 1, monthly_payment))
  assert_eq(5000, final_payment)
}

test "complex_nested_operations" {
  // 复杂嵌套运算测试
  let base = 15
  
  // 嵌套计算：ceil((base*3 + base*2) / (base + ceil(base/3)))
  let numerator = add(multiply(base, 3), multiply(base, 2))
  let denominator = add(base, divide_with_ceil(base, 3))
  let result = divide_with_ceil(numerator, denominator)
  
  // 手动验证：ceil((45+30)/(15+5)) = ceil(75/20) = 4
  assert_eq(4, result)
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 仓库存储优化：多种物品的装箱问题
  let item1_qty = 47
  let item1_cap = 8   // ceil(47/8) = 6
  let item2_qty = 89
  let item2_cap = 12  // ceil(89/12) = 8
  let item3_qty = 156
  let item3_cap = 20 // ceil(156/20) = 8
  
  let boxes1 = divide_with_ceil(item1_qty, item1_cap)
  let boxes2 = divide_with_ceil(item2_qty, item2_cap)
  let boxes3 = divide_with_ceil(item3_qty, item3_cap)
  
  let total_boxes = add(add(boxes1, boxes2), boxes3)
  assert_eq(22, total_boxes)
  
  // 计算总容量和利用率
  let total_capacity = add(
    multiply(boxes1, item1_cap),
    add(multiply(boxes2, item2_cap), multiply(boxes3, item3_cap))
  )
  let total_items = add(add(item1_qty, item2_qty), item3_qty)
  let utilization = divide_with_ceil(multiply(total_items, 100), total_capacity)
  
  assert_eq(292, total_capacity)  // 48 + 96 + 160
  assert_eq(92, utilization)      // floor(29200/292) = 100%，但使用ceil
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  // 测试各种特殊字符和编码
  assert_eq_string("Hello, 🌙⭐✨!", greet("🌙⭐✨"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", greet("'single'"))
  assert_eq_string("Hello, \\backslash!", greet("\\backslash"))
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, emoji 🎉🚀🌟!", greet("emoji 🎉🚀🌟"))
  assert_eq_string("Hello, 中文字符!", greet("中文字符"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "performance_large_scale_computations" {
  // 大规模计算性能测试
  let large_num = 100000
  let iterations = 1000
  
  // 大数运算
  let sum_result = add(large_num, multiply(large_num, 9))
  let product_result = multiply(large_num, iterations)
  let division_result = divide_with_ceil(product_result, large_num)
  
  assert_eq(1000000, sum_result)      // 100000 + 100000*9
  assert_eq(100000000, product_result) // 100000 * 1000
  assert_eq(1000, division_result)     // ceil(100000000/100000)
  
  // 连续运算链
  let chain_result = divide_with_ceil(
    multiply(
      add(large_num, 50000),
      divide_with_ceil(iterations, 100)
    ),
    add(100, divide_with_ceil(large_num, 10000))
  )
  assert_eq(7500, chain_result)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 各种除零情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极小除数测试
  assert_eq(100000, divide_with_ceil(99999, 1))
  assert_eq(-99999, divide_with_ceil(-99999, 1))
  assert_eq(50000, divide_with_ceil(99999, 2))
  
  // 边界值组合
  assert_eq(1, divide_with_ceil(1, 2147483647))
  assert_eq(0, divide_with_ceil(-1, 2147483647))
  assert_eq(-1, divide_with_ceil(1, -2147483647))
}

test "real_world_scheduling_problem" {
  // 真实世界调度问题测试
  // 活动安排：会议室分配
  let total_participants = 342
  
  // 计算每种会议室需要的数量
  let rooms_50 = divide_with_ceil(total_participants, 50)
  let rooms_30 = divide_with_ceil(total_participants, 30)
  let rooms_20 = divide_with_ceil(total_participants, 20)
  let rooms_15 = divide_with_ceil(total_participants, 15)
  let rooms_10 = divide_with_ceil(total_participants, 10)
  
  assert_eq(7, rooms_50)   // ceil(342/50) = 7
  assert_eq(12, rooms_30)  // ceil(342/30) = 12
  assert_eq(18, rooms_20)  // ceil(342/20) = 18
  assert_eq(23, rooms_15)  // ceil(342/15) = 23
  assert_eq(35, rooms_10)  // ceil(342/10) = 35
  
  // 计算最优方案（使用50人会议室）
  let total_capacity_50 = multiply(rooms_50, 50)
  let wasted_seats_50 = subtract(total_capacity_50, total_participants)
  let efficiency_50 = divide_with_ceil(multiply(total_participants, 100), total_capacity_50)
  
  assert_eq(350, total_capacity_50)  // 7 * 50
  assert_eq(8, wasted_seats_50)      // 350 - 342
  assert_eq(98, efficiency_50)       // ceil(34200/350)
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}