// Azimuth 项目增强标准测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例
// 涵盖边界条件、实际应用场景和数学性质验证

test "arithmetic_boundary_conditions" {
  // 测试算术运算的边界条件
  assert_eq(2147483646, add(2147483645, 1))  // 接近最大值
  assert_eq(-2147483647, add(-2147483646, -1))  // 接近最小值
  assert_eq(1, add(2147483647, -2147483646))  // 大数相减
  assert_eq(0, multiply(2147483647, 0))  // 大数乘零
  assert_eq(1, divide_with_ceil(2147483647, 2147483647))  // 大数除自身
}

test "financial_calculation_scenario" {
  // 金融计算场景：贷款分期和利息计算
  let principal = 50000
  let monthly_payment = 2200
  let months_needed = divide_with_ceil(principal, monthly_payment)
  assert_eq(23, months_needed)  // ceil(50000/2200) = 23
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)
  
  // 计算最后一个月的实际支付
  let final_payment = principal - multiply(months_needed - 1, monthly_payment)
  assert_eq(200, final_payment)
}

test "resource_allocation_optimization" {
  // 资源分配优化：服务器和带宽分配
  let total_bandwidth = 1000  // Mbps
  let servers = 3
  let bandwidth_per_server = divide_with_ceil(total_bandwidth, servers)
  assert_eq(334, bandwidth_per_server)  // ceil(1000/3) = 334
  
  // 验证总分配带宽
  let total_allocated = multiply(bandwidth_per_server, servers)
  assert_eq(1002, total_allocated)
  
  // 计算带宽浪费
  let bandwidth_waste = total_allocated - total_bandwidth
  assert_eq(2, bandwidth_waste)
}

test "inventory_management_complex" {
  // 复杂库存管理：多产品装箱优化
  // 产品A：156个，每箱24个
  let product_a = 156
  let box_capacity_a = 24
  let boxes_a = divide_with_ceil(product_a, box_capacity_a)
  assert_eq(7, boxes_a)  // ceil(156/24) = 7
  
  // 产品B：89个，每箱16个
  let product_b = 89
  let box_capacity_b = 16
  let boxes_b = divide_with_ceil(product_b, box_capacity_b)
  assert_eq(6, boxes_b)  // ceil(89/16) = 6
  
  // 计算总箱数和总容量
  let total_boxes = add(boxes_a, boxes_b)
  let total_capacity = add(multiply(boxes_a, box_capacity_a), 
                                   multiply(boxes_b, box_capacity_b))
  
  assert_eq(13, total_boxes)
  assert_eq(264, total_capacity)
}

test "mathematical_properties_comprehensive" {
  // 综合数学性质验证
  let x = 15
  let y = 7
  let z = 3
  
  // 加法结合律
  assert_eq(add(add(x, y), z), 
            add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), 
            multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(multiply(x, add(y, z)),
            add(multiply(x, y), multiply(x, z)))
  
  // 零元素性质
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
  assert_eq(x, multiply(x, 1))
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
  assert_eq_string("Hello,   spaces!", greet("  spaces  "))
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  
  // 极值除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数运算
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "real_world_project_planning" {
  // 真实世界项目规划场景
  let project_tasks = 127
  let team_members = 8
  let working_days = 22  // 每月工作日
  let tasks_per_person_per_day = 2
  
  // 计算团队日产能
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  assert_eq(16, daily_capacity)
  
  // 计算需要的工作日
  let days_needed = divide_with_ceil(project_tasks, daily_capacity)
  assert_eq(8, days_needed)  // ceil(127/16) = 8
  
  // 计算需要的月数
  let months_needed = divide_with_ceil(days_needed, working_days)
  assert_eq(1, months_needed)  // ceil(8/22) = 1
  
  // 计算总处理能力
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(128, total_capacity)
  
  // 计算剩余处理能力
  let spare_capacity = total_capacity - project_tasks
  assert_eq(1, spare_capacity)
}