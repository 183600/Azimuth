// Azimuth 项目标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

// 导入 azimuth 库
use azimuth

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 42
  
  // 加法恒等式：x + 0 = x
  assert_eq(x, azimuth.add(x, 0))
  assert_eq(x, azimuth.add(0, x))
  
  // 乘法恒等式：x * 1 = x
  assert_eq(x, azimuth.multiply(x, 1))
  assert_eq(x, azimuth.multiply(1, x))
  
  // 乘法零元性质：x * 0 = 0
  assert_eq(0, azimuth.multiply(x, 0))
  assert_eq(0, azimuth.multiply(0, x))
  
  // 减法恒等式：x - x = 0
  assert_eq(0, azimuth.subtract(x, x))
  assert_eq(x, azimuth.subtract(x, 0))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试：电商订单处理
  let order_items = 47
  let items_per_box = 12
  let boxes_per_pallet = 8
  let cost_per_item = 25
  
  // 计算需要的箱子数量
  let boxes_needed = azimuth.divide_with_ceil(order_items, items_per_box)
  assert_eq(4, boxes_needed)
  
  // 计算需要的托盘数量
  let pallets_needed = azimuth.divide_with_ceil(boxes_needed, boxes_per_pallet)
  assert_eq(1, pallets_needed)
  
  // 计算总成本
  let total_cost = azimuth.multiply(order_items, cost_per_item)
  assert_eq(1175, total_cost)
  
  // 计算总容量（可能有剩余空间）
  let total_box_capacity = azimuth.multiply(boxes_needed, items_per_box)
  let unused_space = azimuth.subtract(total_box_capacity, order_items)
  assert_eq(1, unused_space)
}

test "unicode_and_internationalization" {
  // Unicode 和国际化测试
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, São Paulo!", azimuth.greet("São Paulo"))
  assert_eq_string("Hello, Москва!", azimuth.greet("Москва"))
  assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  assert_eq_string("Hello, café!", azimuth.greet("café"))
  assert_eq_string("Hello, ñoño!", azimuth.greet("ñoño"))
  assert_eq_string("Hello, 🏳️‍🌈!", azimuth.greet("🏳️‍🌈"))
}

test "edge_case_boundary_values" {
  // 边界值和极值测试
  // 零值操作
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(0, azimuth.multiply(0, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 1))
  assert_eq(0, azimuth.divide_with_ceil(0, -1))
  
  // 单位操作
  assert_eq(1, azimuth.divide_with_ceil(1, 1))
  assert_eq(1, azimuth.divide_with_ceil(-1, -1))
  assert_eq(1, azimuth.divide_with_ceil(100, 100))
  
  // 大数运算（在安全范围内）
  assert_eq(2000000, azimuth.add(1000000, 1000000))
  assert_eq(1000000, azimuth.multiply(1000, 1000))
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误处理
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
  assert_eq(0, azimuth.divide_with_ceil(-10, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 0))
  assert_eq(0, azimuth.divide_with_ceil(2147483647, 0))
  
  // 极值除法测试
  assert_eq(1, azimuth.divide_with_ceil(1, 2147483647))
  assert_eq(-1, azimuth.divide_with_ceil(-1, 2147483647))
  assert_eq(1, azimuth.divide_with_ceil(-1, -2147483647))
}

test "mathematical_sequence_operations" {
  // 数学序列操作测试：斐波那契风格计算
  let n1 = 5
  let n2 = 8
  let n3 = 13
  
  // 计算连续的数学操作
  let sum_12 = azimuth.add(n1, n2)  // 5 + 8 = 13
  let sum_23 = azimuth.add(n2, n3)  // 8 + 13 = 21
  let product_12 = azimuth.multiply(n1, n2)  // 5 * 8 = 40
  
  assert_eq(13, sum_12)
  assert_eq(21, sum_23)
  assert_eq(40, product_12)
  
  // 验证斐波那契性质
  assert_eq(sum_12, n3)  // 5 + 8 = 13
  
  // 复杂计算：((n1 + n2) * n3) / n2
  let complex_calc = azimuth.divide_with_ceil(
    azimuth.multiply(sum_12, n3), 
    n2
  )
  assert_eq(22, complex_calc)  // ceil(13 * 13 / 8) = ceil(169 / 8) = 22
}

test "real_world_resource_allocation" {
  // 真实世界资源分配测试：项目团队管理
  let total_tasks = 125
  let team_members = 8
  let working_days = 15
  let tasks_per_day_capacity = 10
  
  // 计算团队每日处理能力
  let daily_capacity = azimuth.multiply(team_members, tasks_per_day_capacity)
  assert_eq(80, daily_capacity)
  
  // 计算项目总天数需求
  let days_needed = azimuth.divide_with_ceil(total_tasks, daily_capacity)
  assert_eq(2, days_needed)
  
  // 验证时间充足性
  assert_true(days_needed <= working_days)
  
  // 计算剩余时间
  let remaining_days = azimuth.subtract(working_days, days_needed)
  assert_eq(13, remaining_days)
  
  // 计算资源利用率
  let total_capacity = azimuth.multiply(daily_capacity, days_needed)
  let utilization = azimuth.divide_with_ceil(
    azimuth.multiply(total_tasks, 100), 
    total_capacity
  )
  assert_eq(79, utilization)  // floor(12500/160) = 78%
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello,   !", azimuth.greet("  "))
  assert_eq_string("Hello, \t\n!", azimuth.greet("\t\n"))
  assert_eq_string("Hello, !", azimuth.greet(String.from_char('\0')))
  
  // 数字字符串
  assert_eq_string("Hello, 12345!", azimuth.greet("12345"))
  assert_eq_string("Hello, -678!", azimuth.greet("-678"))
  assert_eq_string("Hello, 3.14!", azimuth.greet("3.14"))
  
  // 特殊符号字符串
  assert_eq_string("Hello, @#$%^&*()!", azimuth.greet("@#$%^&*()"))
  assert_eq_string("Hello, []{}|\\!", azimuth.greet("[]{}|\\"))
  assert_eq_string("Hello, <>?/!", azimuth.greet("<>?/"))
}

test "performance_and_stress_operations" {
  // 性能和压力测试：重复操作
  let base_value = 7
  
  // 连续加法操作
  let sum_result = azimuth.add(
    azimuth.add(
      azimuth.add(base_value, 3), 
      5
    ), 
    10
  )
  assert_eq(25, sum_result)
  
  // 连续乘法操作
  let product_result = azimuth.multiply(
    azimuth.multiply(
      azimuth.multiply(base_value, 2), 
      3
    ), 
    4
  )
  assert_eq(168, product_result)
  
  // 混合操作链
  let chain_result = azimuth.divide_with_ceil(
    azimuth.multiply(
      azimuth.add(base_value, 3), 
      5
    ), 
    8
  )
  assert_eq(7, chain_result)  // ceil((7+3)*5/8) = ceil(50/8) = 7
}

test "comprehensive_calculation_scenario" {
  // 综合计算场景测试：库存管理系统
  let product_a_qty = 120
  let product_b_qty = 85
  let product_c_qty = 45
  let box_capacity = 24
  let cost_per_box = 15
  let budget = 150
  
  // 计算总产品数量
  let total_products = azimuth.add(
    azimuth.add(product_a_qty, product_b_qty), 
    product_c_qty
  )
  assert_eq(250, total_products)
  
  // 计算需要的箱子数量
  let boxes_needed = azimuth.divide_with_ceil(total_products, box_capacity)
  assert_eq(11, boxes_needed)
  
  // 计算总成本
  let total_cost = azimuth.multiply(boxes_needed, cost_per_box)
  assert_eq(165, total_cost)
  
  // 检查预算是否充足
  let budget_overage = azimuth.subtract(total_cost, budget)
  assert_eq(15, budget_overage)
  
  // 计算额外需要的预算百分比
  let overage_percentage = azimuth.divide_with_ceil(
    azimuth.multiply(budget_overage, 100), 
    budget
  )
  assert_eq(10, overage_percentage)  // ceil(15*100/150) = ceil(1500/150) = 10
  
  // 计算箱子利用率
  let total_capacity = azimuth.multiply(boxes_needed, box_capacity)
  let unused_space = azimuth.subtract(total_capacity, total_products)
  let space_utilization = azimuth.divide_with_ceil(
    azimuth.multiply(total_products, 100), 
    total_capacity
  )
  assert_eq(94, space_utilization)  // floor(25000/264) = 94%
}