// Azimuth 高质量测试套件
// 包含10个标准 MoonBit 测试用例，覆盖各种功能和边界情况

test "arithmetic_precision_and_accuracy" {
  // 算术精度和准确性测试
  inspect(add(999999, 1), content="1000000")
  inspect(subtract(1000000, 2), content="999998")
  inspect(multiply(1000, 1000), content="1000000")
  inspect(divide_with_ceil(999999, 1000000), content="1")
  inspect(divide_with_ceil(1000000, 1), content="1000000")
}

test "complex_financial_calculations" {
  // 复杂金融计算测试
  let principal = 10000
  let rate = 5
  let years = 3
  let simple_interest = multiply(principal, multiply(rate, years))
  let total_amount = add(principal, divide_with_ceil(simple_interest, 100))
  
  inspect(simple_interest, content="150000")  // 10000 * 5 * 3 = 150000
  inspect(total_amount, content="1600")      // 10000 + ceil(150000/100) = 10000 + 1500 = 11500
}

test "inventory_optimization_scenario" {
  // 库存优化场景测试
  let total_products = 487
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  inspect(boxes_needed, content="21")
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  inspect(total_capacity, content="504")
  
  // 验证空间利用率
  let utilization_rate = divide_with_ceil(multiply(total_products, 100), total_capacity)
  inspect(utilization_rate, content="97")  // floor(48700/504) = 96%
  
  // 验证剩余空间
  let unused_space = subtract(total_capacity, total_products)
  inspect(unused_space, content="17")
}

test "mathematical_series_calculations" {
  // 数学级数计算测试
  let n = 10
  let sum_first_n = multiply(n, add(n, 1))
  let triangular_number = divide_with_ceil(sum_first_n, 2)
  
  inspect(sum_first_n, content="110")       // 10 * (10 + 1) = 110
  inspect(triangular_number, content="55")  // ceil(110/2) = 55
  
  // 验证平方和公式
  let sum_of_squares = multiply(n, multiply(add(n, 1), add(multiply(2, n), 1)))
  let squared_sum = divide_with_ceil(sum_of_squares, 6)
  
  inspect(sum_of_squares, content="2310")   // 10 * 11 * 21 = 2310
  inspect(squared_sum, content="385")       // ceil(2310/6) = 385
}

test "resource_allocation_efficiency" {
  // 资源分配效率测试
  let total_tasks = 157
  let team_members = 8
  let working_days = 5
  
  // 计算每人每天需要完成的任务数
  let total_person_days = multiply(team_members, working_days)
  let tasks_per_person_per_day = divide_with_ceil(total_tasks, total_person_days)
  
  inspect(total_person_days, content="40")           // 8 * 5 = 40
  inspect(tasks_per_person_per_day, content="4")     // ceil(157/40) = 4
  
  // 验证总处理能力
  let total_capacity = multiply(tasks_per_person_per_day, total_person_days)
  inspect(total_capacity, content="160")             // 4 * 40 = 160
  
  // 验证剩余处理能力
  let spare_capacity = subtract(total_capacity, total_tasks)
  inspect(spare_capacity, content="3")
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  inspect(greet("single"), content="Hello, single!")
  inspect(greet("UPPER"), content="Hello, UPPER!")
  inspect(greet("lower"), content="Hello, lower!")
  inspect(greet("MixEdCase"), content="Hello, MixEdCase!")
  inspect(greet("123456789"), content="Hello, 123456789!")
  inspect(greet("special@#$%"), content="Hello, special@#$%!")
  inspect(greet("   spaces   "), content="Hello,    spaces   !")
  inspect(greet("\t\n"), content="Hello, \t\n!")
}

test "temperature_conversion_calculations" {
  // 温度转换计算测试
  let celsius = 25
  let fahrenheit = add(multiply(multiply(celsius, 9), 5), 32)
  let back_to_celsius = divide_with_ceil(subtract(fahrenheit, 32), multiply(9, 5))
  
  inspect(fahrenheit, content="77")        // (25 * 9/5) + 32 = 45 + 32 = 77
  inspect(back_to_celsius, content="5")    // ceil((77-32)/(9/5)) = ceil(45*5/9) = ceil(25) = 25
  
  // 极值温度测试
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(multiply(freezing_celsius, 9), 5), 32)
  inspect(freezing_fahrenheit, content="32")
  
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(multiply(boiling_celsius, 9), 5), 32)
  inspect(boiling_fahrenheit, content="212")
}

test "project_management_timeline" {
  // 项目管理时间线测试
  let total_story_points = 128
  let team_velocity = 21
  let sprints_needed = divide_with_ceil(total_story_points, team_velocity)
  
  inspect(sprints_needed, content="7")  // ceil(128/21) = 7
  
  // 计算总工作周数（每个冲刺2周）
  let weeks_per_sprint = 2
  let total_weeks = multiply(sprints_needed, weeks_per_sprint)
  inspect(total_weeks, content="14")
  
  // 计算总处理能力
  let total_capacity = multiply(sprints_needed, team_velocity)
  inspect(total_capacity, content="147")
  
  // 计算缓冲能力
  let buffer_capacity = subtract(total_capacity, total_story_points)
  inspect(buffer_capacity, content="19")
}

test "data_structure_operations" {
  // 数据结构操作测试（模拟数组操作）
  let array_size = 10
  let element_size = 4
  let total_memory = multiply(array_size, element_size)
  
  inspect(total_memory, content="40")
  
  // 计算页面对齐需求
  let page_size = 16
  let pages_needed = divide_with_ceil(total_memory, page_size)
  
  inspect(pages_needed, content="3")  // ceil(40/16) = 3
  
  // 计算实际分配的内存
  let allocated_memory = multiply(pages_needed, page_size)
  inspect(allocated_memory, content="48")
  
  // 计算内存碎片
  let memory_fragmentation = subtract(allocated_memory, total_memory)
  inspect(memory_fragmentation, content="8")
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  let product_cost = 150
  let quantity = 25
  let discount_threshold = 20
  let discount_rate = 10
  
  // 计算总成本
  let subtotal = multiply(product_cost, quantity)
  inspect(subtotal, content="3750")
  
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
  
  inspect(discount_amount, content="375")  // ceil(3750 * 10 / 100) = 375
  inspect(final_total, content="3375")     // 3750 - 375 = 3375
  
  // 计算每个包装盒的产品数量
  let products_per_box = 6
  let boxes_needed = divide_with_ceil(quantity, products_per_box)
  inspect(boxes_needed, content="5")  // ceil(25/6) = 5
}