// 增强测试套件 - 为 Azimuth 项目添加的高质量测试用例
// 使用标准 MoonBit 测试语法，包含10个实用测试用例

// 导入被测试的模块
use azimuth

test "performance_large_number_operations" {
  // 性能测试：大数运算
  let large_num1 = 999999
  let large_num2 = 888888
  
  // 大数加法
  azimuth.assert_eq(1888887, azimuth.add(large_num1, large_num2))
  
  // 大数乘法（调整数值以避免溢出）
  azimuth.assert_eq(888887112, azimuth.multiply(large_num1, 888))
  
  // 大数除法向上取整
  azimuth.assert_eq(2, azimuth.divide_with_ceil(large_num1, large_num2))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(large_num2, large_num1))
  
  // 极值测试
  azimuth.assert_eq(1999998, azimuth.add(999999, 999999))
  azimuth.assert_eq(998001, azimuth.multiply(999, 999))
}

test "data_validation_and_boundaries" {
  // 数据验证和边界测试
  // 零值测试
  azimuth.assert_eq(0, azimuth.add(0, 0))
  azimuth.assert_eq(0, azimuth.multiply(0, 999999))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 999999))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(999999, 0))
  
  // 单位值测试
  azimuth.assert_eq(1000000, azimuth.add(999999, 1))
  azimuth.assert_eq(999999, azimuth.multiply(999999, 1))
  azimuth.assert_eq(999999, azimuth.multiply(1, 999999))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(999999, 999999))
  
  // 负数边界测试
  azimuth.assert_eq(-999999, azimuth.add(-500000, -499999))
  azimuth.assert_eq(999999, azimuth.multiply(-999999, -1))
  azimuth.assert_eq(-1, azimuth.divide_with_ceil(-999999, 999999))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：计算项目成本和资源分配
  
  let team_size = 15
  let hourly_rate = 75
  let project_hours = 160
  let management_overhead = 20  // 百分比
  
  // 计算基础成本
  let base_cost = azimuth.multiply(team_size, azimuth.multiply(hourly_rate, project_hours))
  azimuth.assert_eq(180000, base_cost)  // 15 * 75 * 160 = 180000
  
  // 计算管理费用
  let overhead_cost = azimuth.divide_with_ceil(azimuth.multiply(base_cost, management_overhead), 100)
  azimuth.assert_eq(36000, overhead_cost)  // ceil(180000 * 20 / 100) = 36000
  
  // 计算总成本
  let total_cost = azimuth.add(base_cost, overhead_cost)
  azimuth.assert_eq(216000, total_cost)  // 180000 + 36000 = 216000
  
  // 计算每个冲刺的成本（假设4个冲刺）
  let sprints = 4
  let cost_per_sprint = azimuth.divide_with_ceil(total_cost, sprints)
  azimuth.assert_eq(54000, cost_per_sprint)  // ceil(216000 / 4) = 54000
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：优化服务器资源分配
  
  let total_requests = 10000
  let servers_available = 3
  let requests_per_server = azimuth.divide_with_ceil(total_requests, servers_available)
  azimuth.assert_eq(3334, requests_per_server)  // ceil(10000 / 3) = 3334
  
  // 计算实际处理能力
  let actual_capacity = azimuth.multiply(requests_per_server, servers_available)
  azimuth.assert_eq(10002, actual_capacity)  // 3334 * 3 = 10002
  
  // 计算资源利用率
  let utilization = azimuth.divide_with_ceil(azimuth.multiply(total_requests, 100), actual_capacity)
  azimuth.assert_eq(100, utilization)  // ceil(1000000 / 10002) = 100
  
  // 计算剩余处理能力
  let remaining_capacity = azimuth.subtract(actual_capacity, total_requests)
  azimuth.assert_eq(2, remaining_capacity)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试各种特殊字符和格式
  
  azimuth.assert_eq_string("Hello, JSON@API!", azimuth.greet("JSON@API"))
  azimuth.assert_eq_string("Hello, URL/endpoint!", azimuth.greet("URL/endpoint"))
  azimuth.assert_eq_string("Hello, query?param=value!", azimuth.greet("query?param=value"))
  azimuth.assert_eq_string("Hello, email@domain.com!", azimuth.greet("email@domain.com"))
  azimuth.assert_eq_string("Hello, phone+1234567890!", azimuth.greet("phone+1234567890"))
  azimuth.assert_eq_string("Hello, version#1.2.3!", azimuth.greet("version#1.2.3"))
  
  // 测试包含空格的复杂字符串
  azimuth.assert_eq_string("Hello, multi word string!", azimuth.greet("multi word string"))
  azimuth.assert_eq_string("Hello,  leading and trailing !", azimuth.greet(" leading and trailing "))
}

test "mathematical_algorithm_implementation" {
  // 数学算法实现测试
  // 场景：实现简单的数学算法
  
  // 计算阶乘的一部分（简化版本）
  let factorial_5 = azimuth.multiply(azimuth.multiply(5, 4), azimuth.multiply(3, azimuth.multiply(2, 1)))
  azimuth.assert_eq(120, factorial_5)
  
  // 计算幂运算（5^3）
  let power_5_3 = azimuth.multiply(azimuth.multiply(5, 5), 5)
  azimuth.assert_eq(125, power_5_3)
  
  // 计算等差数列和（1+2+3+...+10）
  let sum_1_to_10 = azimuth.multiply(azimuth.divide_with_ceil(azimuth.add(1, 10), 2), 10)
  azimuth.assert_eq(55, sum_1_to_10)
  
  // 计算等比数列和（2+4+8+16）
  let geometric_sum = azimuth.add(azimuth.add(2, 4), azimuth.add(8, 16))
  azimuth.assert_eq(30, geometric_sum)
}

test "time_based_calculations" {
  // 基于时间的计算测试
  // 场景：项目时间管理和排期
  
  let total_tasks = 125
  let working_hours_per_day = 8
  let tasks_per_hour = 2
  let team_members = 4
  
  // 计算每日处理能力
  let daily_capacity = azimuth.multiply(team_members, azimuth.multiply(working_hours_per_day, tasks_per_hour))
  azimuth.assert_eq(64, daily_capacity)  // 4 * 8 * 2 = 64
  
  // 计算需要的天数
  let days_needed = azimuth.divide_with_ceil(total_tasks, daily_capacity)
  azimuth.assert_eq(2, days_needed)  // ceil(125 / 64) = 2
  
  // 计算总工作小时数
  let total_hours = azimuth.multiply(days_needed, daily_capacity)
  azimuth.assert_eq(128, total_hours)  // 2 * 64 = 128
  
  // 计算剩余处理能力
  let remaining_capacity = azimuth.subtract(total_hours, total_tasks)
  azimuth.assert_eq(3, remaining_capacity)
}

test "inventory_management_system" {
  // 库存管理系统测试
  // 场景：多产品库存管理
  
  // 产品A：234个，每箱15个
  let product_a_qty = 234
  let product_a_box = 15
  let boxes_a = azimuth.divide_with_ceil(product_a_qty, product_a_box)
  azimuth.assert_eq(16, boxes_a)  // ceil(234 / 15) = 16
  
  // 产品B：567个，每箱20个
  let product_b_qty = 567
  let product_b_box = 20
  let boxes_b = azimuth.divide_with_ceil(product_b_qty, product_b_box)
  azimuth.assert_eq(29, boxes_b)  // ceil(567 / 20) = 29
  
  // 产品C：89个，每箱10个
  let product_c_qty = 89
  let product_c_box = 10
  let boxes_c = azimuth.divide_with_ceil(product_c_qty, product_c_box)
  azimuth.assert_eq(9, boxes_c)  // ceil(89 / 10) = 9
  
  // 计算总箱子数
  let total_boxes = azimuth.add(azimuth.add(boxes_a, boxes_b), boxes_c)
  azimuth.assert_eq(54, total_boxes)  // 16 + 29 + 9 = 54
  
  // 计算总存储容量
  let total_capacity = azimuth.add(
    azimuth.multiply(boxes_a, product_a_box),
    azimuth.add(azimuth.multiply(boxes_b, product_b_box), azimuth.multiply(boxes_c, product_c_box))
  )
  azimuth.assert_eq(910, total_capacity)  // 16*15 + 29*20 + 9*10 = 240 + 580 + 90 = 910
}

test "financial_calculations" {
  // 金融计算测试
  // 场景：贷款和投资计算
  
  let principal = 50000
  let annual_rate = 6  // 百分比
  let years = 5
  
  // 计算简单利息
  let total_interest = azimuth.divide_with_ceil(azimuth.multiply(principal, azimuth.multiply(annual_rate, years)), 100)
  azimuth.assert_eq(15000, total_interest)  // ceil(50000 * 6 * 5 / 100) = 15000
  
  // 计算总还款额
  let total_payment = azimuth.add(principal, total_interest)
  azimuth.assert_eq(65000, total_payment)  // 50000 + 15000 = 65000
  
  // 计算月还款额（假设60个月）
  let months = azimuth.multiply(years, 12)
  let monthly_payment = azimuth.divide_with_ceil(total_payment, months)
  azimuth.assert_eq(1084, monthly_payment)  // ceil(65000 / 60) = 1084
  
  // 计算实际总还款额
  let actual_total = azimuth.multiply(monthly_payment, months)
  azimuth.assert_eq(65040, actual_total)  // 1084 * 60 = 65040
  
  // 计算额外支付
  let extra_payment = azimuth.subtract(actual_total, total_payment)
  azimuth.assert_eq(40, extra_payment)
}

test "error_handling_edge_cases" {
  // 错误处理和边缘情况测试
  // 测试各种异常和边界情况
  
  // 除零错误的全面测试
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(999999, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-999999, 0))
  
  // 极值运算测试
  let max_safe = 1000000
  let min_safe = -1000000
  
  azimuth.assert_eq(0, azimuth.multiply(max_safe, 0))
  azimuth.assert_eq(0, azimuth.multiply(min_safe, 0))
  azimuth.assert_eq(max_safe, azimuth.add(max_safe, 0))
  azimuth.assert_eq(min_safe, azimuth.add(min_safe, 0))
  
  // 边界除法测试
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, max_safe))
  azimuth.assert_eq(-1, azimuth.divide_with_ceil(-1, max_safe))
  azimuth.assert_eq(max_safe, azimuth.divide_with_ceil(max_safe, 1))
  azimuth.assert_eq(-max_safe, azimuth.divide_with_ceil(-max_safe, 1))
  
  // 字符串边界测试
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello, x!", azimuth.greet("x"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(" "))
}