// Azimuth 新增测试套件
// 包含10个标准 MoonBit 测试用例，覆盖各种功能和边界情况

test "arithmetic_precision_and_accuracy" {
  // 算术精度和准确性测试
  assert_eq(1000000, add(999999, 1))
  assert_eq(999998, subtract(1000000, 2))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, divide_with_ceil(999999, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
}

test "complex_financial_calculations" {
  // 复杂金融计算测试
  let principal = 10000
  let rate = 5
  let years = 3
  let simple_interest = multiply(principal, multiply(rate, years))
  let total_amount = add(principal, divide_with_ceil(simple_interest, 100))
  
  assert_eq(150000, simple_interest)  // 10000 * 5 * 3 = 150000
  assert_eq(1600, total_amount)      // 10000 + ceil(150000/100) = 10000 + 1500 = 11500
}

test "inventory_optimization_scenario" {
  // 库存优化场景测试
  let total_products = 487
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(21, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(504, total_capacity)
  
  // 验证空间利用率
  let utilization_rate = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(97, utilization_rate)  // floor(48700/504) = 96%
  
  // 验证剩余空间
  let unused_space = subtract(total_capacity, total_products)
  assert_eq(17, unused_space)
}

test "mathematical_series_calculations" {
  // 数学级数计算测试
  let n = 10
  let sum_first_n = multiply(n, add(n, 1))
  let triangular_number = divide_with_ceil(sum_first_n, 2)
  
  assert_eq(110, sum_first_n)       // 10 * (10 + 1) = 110
  assert_eq(55, triangular_number)  // ceil(110/2) = 55
  
  // 验证平方和公式
  let sum_of_squares = multiply(n, multiply(add(n, 1), add(multiply(2, n), 1)))
  let squared_sum = divide_with_ceil(sum_of_squares, 6)
  
  assert_eq(2310, sum_of_squares)   // 10 * 11 * 21 = 2310
  assert_eq(385, squared_sum)       // ceil(2310/6) = 385
}

test "resource_allocation_efficiency" {
  // 资源分配效率测试
  let total_tasks = 157
  let team_members = 8
  let working_days = 5
  
  // 计算每人每天需要完成的任务数
  let total_person_days = multiply(team_members, working_days)
  let tasks_per_person_per_day = divide_with_ceil(total_tasks, total_person_days)
  
  assert_eq(40, total_person_days)           // 8 * 5 = 40
  assert_eq(4, tasks_per_person_per_day)     // ceil(157/40) = 4
  
  // 验证总处理能力
  let total_capacity = multiply(tasks_per_person_per_day, total_person_days)
  assert_eq(160, total_capacity)             // 4 * 40 = 160
  
  // 验证剩余处理能力
  let spare_capacity = subtract(total_capacity, total_tasks)
  assert_eq(3, spare_capacity)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, UPPER!", greet("UPPER"))
  assert_eq_string("Hello, lower!", greet("lower"))
  assert_eq_string("Hello, MixEdCase!", greet("MixEdCase"))
  assert_eq_string("Hello, 123456789!", greet("123456789"))
  assert_eq_string("Hello, special@#$%!", greet("special@#$%"))
  assert_eq_string("Hello,   spaces   !", greet("   spaces   "))
  assert_eq_string("Hello, tab_and_newline!", greet("\t\n"))
}

test "temperature_conversion_calculations" {
  // 温度转换计算测试
  let celsius = 25
  let fahrenheit = add(multiply(multiply(celsius, 9), 5), 32)
  let back_to_celsius = divide_with_ceil(subtract(fahrenheit, 32), multiply(9, 5))
  
  assert_eq(77, fahrenheit)        // (25 * 9/5) + 32 = 45 + 32 = 77
  assert_eq(5, back_to_celsius)    // ceil((77-32)/(9/5)) = ceil(45*5/9) = ceil(25) = 25
  
  // 极值温度测试
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(multiply(freezing_celsius, 9), 5), 32)
  assert_eq(32, freezing_fahrenheit)
  
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(multiply(boiling_celsius, 9), 5), 32)
  assert_eq(212, boiling_fahrenheit)
}

test "project_management_timeline" {
  // 项目管理时间线测试
  let total_story_points = 128
  let team_velocity = 21
  let sprints_needed = divide_with_ceil(total_story_points, team_velocity)
  
  assert_eq(7, sprints_needed)  // ceil(128/21) = 7
  
  // 计算总工作周数（每个冲刺2周）
  let weeks_per_sprint = 2
  let total_weeks = multiply(sprints_needed, weeks_per_sprint)
  assert_eq(14, total_weeks)
  
  // 计算总处理能力
  let total_capacity = multiply(sprints_needed, team_velocity)
  assert_eq(147, total_capacity)
  
  // 计算缓冲能力
  let buffer_capacity = subtract(total_capacity, total_story_points)
  assert_eq(19, buffer_capacity)
}

test "data_structure_operations" {
  // 数据结构操作测试（模拟数组操作）
  let array_size = 10
  let element_size = 4
  let total_memory = multiply(array_size, element_size)
  
  assert_eq(40, total_memory)
  
  // 计算页面对齐需求
  let page_size = 16
  let pages_needed = divide_with_ceil(total_memory, page_size)
  
  assert_eq(3, pages_needed)  // ceil(40/16) = 3
  
  // 计算实际分配的内存
  let allocated_memory = multiply(pages_needed, page_size)
  assert_eq(48, allocated_memory)
  
  // 计算内存碎片
  let memory_fragmentation = subtract(allocated_memory, total_memory)
  assert_eq(8, memory_fragmentation)
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  let product_cost = 150
  let quantity = 25
  let discount_threshold = 20
  let discount_rate = 10
  
  // 计算总成本
  let subtotal = multiply(product_cost, quantity)
  assert_eq(3750, subtotal)
  
  // 应用折扣（如果数量超过阈值）
  let discount_applicable = quantity >= discount_threshold
  let discount_amount = 0
  let final_total = 0
  
  if discount_applicable {
    discount_amount = divide_with_ceil(multiply(subtotal, discount_rate), 100)
    final_total = subtract(subtotal, discount_amount)
  } else {
    final_total = subtotal
  }
  
  assert_eq(375, discount_amount)  // ceil(3750 * 10 / 100) = 375
  assert_eq(3375, final_total)     // 3750 - 375 = 3375
  
  // 计算每个包装盒的产品数量
  let products_per_box = 6
  let boxes_needed = divide_with_ceil(quantity, products_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}