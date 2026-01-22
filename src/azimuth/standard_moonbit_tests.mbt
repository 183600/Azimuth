// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能测试
// 包含基本运算、字符串处理和边界情况测试

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(15, -5))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(12, multiply(-3, -4))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_operations_combination" {
  // 测试数学运算的组合使用
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5+3)*2 = 8*2 = 16
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-11, -3))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "greet_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "business_calculation_scenario" {
  // 测试业务计算场景：购物车总价
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(105, total)  // 25*3 + 15*2 = 75 + 30 = 105
}

test "packaging_calculation" {
  // 测试包装计算场景：计算需要的包装箱数量
  let total_items = 23
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(23/5) = 5
}

test "complex_expression_evaluation" {
  // 测试复杂表达式计算
  let a = 10
  let b = 20
  let c = 3
  
  // 计算：(a + b) * c - a / b
  let result = add(multiply(add(a, b), c), -divide_with_ceil(a, b))
  assert_eq(89, result)  // (10 + 20) * 3 - ceil(10/20) = 30*3 - 1 = 90 - 1 = 89
}

// 新增的10个高质量测试用例
test "temperature_conversion" {
  // 温度转换测试：摄氏度到华氏度
  // F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9) / 5, 32)
  assert_eq(77, fahrenheit)
  
  // 零度测试
  let freezing_point = add(multiply(0, 9) / 5, 32)
  assert_eq(32, freezing_point)
  
  // 负温度测试
  let negative_celsius = -10
  let negative_fahrenheit = add(multiply(negative_celsius, 9) / 5, 32)
  assert_eq(14, negative_fahrenheit)
}

test "loan_interest_calculation" {
  // 贷款利息计算测试
  let principal = 10000
  let annual_rate_percent = 6
  let years = 3
  
  // 简单利息计算：A = P * (1 + r * t)
  let total_interest = multiply(principal, multiply(annual_rate_percent, years)) / 100
  let total_amount = add(principal, total_interest)
  
  assert_eq(1800, total_interest)
  assert_eq(11800, total_amount)
}

test "batch_processing_simulation" {
  // 批处理模拟测试
  let total_records = 157
  let batch_size = 25
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_records, batch_size)
  
  // 计算最后一批的记录数
  let last_batch_size = total_records - multiply(batches_needed - 1, batch_size)
  
  assert_eq(7, batches_needed)
  assert_eq(7, last_batch_size)
}

test "game_score_calculation" {
  // 游戏得分计算测试
  let base_score = 1000
  let level_multiplier = 3
  let bonus_points = 250
  let penalty_points = 50
  
  // 计算最终得分
  let level_score = multiply(base_score, level_multiplier)
  let final_score = add(add(level_score, bonus_points), -penalty_points)
  
  assert_eq(3000, level_score)
  assert_eq(3200, final_score)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 1500
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以启动的额外小型项目数量
  let small_project_cost = 300
  let additional_projects = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(4500, allocated_budget)
  assert_eq(500, remaining_budget)
  assert_eq(2, additional_projects)
}

test "cryptographic_hash_simulation" {
  // 加密哈希模拟测试
  let input_number = 12345
  let prime_multiplier = 31
  let modulo_value = 100000
  
  // 简单的哈希函数模拟
  let hash_step1 = multiply(input_number, prime_multiplier)
  let hash_step2 = add(hash_step1, input_number)
  let final_hash = hash_step2 % modulo_value
  
  assert_eq(382760, hash_step1)
  assert_eq(395105, hash_step2)
  assert_eq(95105, final_hash)
}

test "parallel_processing_efficiency" {
  // 并行处理效率测试
  let single_core_time = 120  // 分钟
  let core_count = 4
  let parallel_efficiency_percent = 75  // 并行效率75%
  
  // 计算理论并行时间
  let theoretical_parallel_time = divide_with_ceil(single_core_time, core_count)
  
  // 考虑并行效率的实际时间
  let efficiency_factor = divide_with_ceil(multiply(theoretical_parallel_time, 100), parallel_efficiency_percent)
  
  assert_eq(30, theoretical_parallel_time)
  assert_eq(40, efficiency_factor)
}

test "data_structure_operations" {
  // 数据结构操作测试
  let array_size = 10
  let page_size = 3
  
  // 计算分页数量
  let total_pages = divide_with_ceil(array_size, page_size)
  
  // 计算完整页面的元素数量
  let full_pages = array_size / page_size
  let full_page_elements = multiply(full_pages, page_size)
  
  // 计算最后一页的元素数量
  let last_page_elements = array_size - full_page_elements
  
  assert_eq(4, total_pages)
  assert_eq(9, full_page_elements)
  assert_eq(1, last_page_elements)
}

test "financial_compound_interest" {
  // 复利计算测试
  let principal = 5000
  let annual_rate_percent = 8
  let years = 4
  
  // 逐年计算复利
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, annual_rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, annual_rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, annual_rate_percent), 100))
  let year4_amount = add(year3_amount, divide_with_ceil(multiply(year3_amount, annual_rate_percent), 100))
  
  assert_eq(5400, year1_amount)
  assert_eq(5832, year2_amount)
  assert_eq(6299, year3_amount)
  assert_eq(6803, year4_amount)
}

test "queue_simulation" {
  // 队列模拟测试
  let arrival_rate = 15  // 每小时到达人数
  let service_rate = 20   // 每小时服务人数
  let time_hours = 8      // 营业时间
  
  // 计算总到达人数和总服务人数
  let total_arrivals = multiply(arrival_rate, time_hours)
  let total_served = multiply(service_rate, time_hours)
  
  // 计算队列中剩余人数
  let remaining_in_queue = add(total_arrivals, -total_served)
  if remaining_in_queue < 0 {
    remaining_in_queue = 0
  }
  
  // 计算需要的额外服务时间（分钟）
  let additional_service_time = divide_with_ceil(multiply(remaining_in_queue, 60), service_rate)
  
  assert_eq(120, total_arrivals)
  assert_eq(160, total_served)
  assert_eq(0, remaining_in_queue)
  assert_eq(0, additional_service_time)
}

test "simple_new_test" {
  // 简单的新测试用例
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq_string("Hello, Test!", greet("Test"))
  assert_eq(3, divide_with_ceil(8, 3))
}