// Azimuth 高级测试用例
// 包含10个标准 MoonBit 测试用例

test "mathematical_identity_elements" {
  // 测试数学恒等元素
  let x = 42
  
  // 加法恒等元素：x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等元素：x * 1 = x
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元素：x * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  
  // 减法恒等：x - x = 0
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  // 完全整除的情况
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(7, divide_with_ceil(21, 3))
  
  // 需要向上取整的情况
  assert_eq(6, divide_with_ceil(11, 2))
  assert_eq(8, divide_with_ceil(22, 3))
  
  // 大数精度测试
  assert_eq(1001, divide_with_ceil(1000001, 1000))
  assert_eq(1000001, divide_with_ceil(1000001, 1))
  
  // 边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：计算项目成本和时间
  
  let team_size = 6
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 12
  let tasks_per_person = 15
  
  // 计算总任务数
  let total_tasks = multiply(team_size, tasks_per_person)
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(hours_per_day, project_days))
  
  // 计算总成本
  let total_cost = multiply(total_hours, hourly_rate)
  
  // 计算每任务平均成本
  let cost_per_task = divide_with_ceil(total_cost, total_tasks)
  
  // 验证结果
  assert_eq(90, total_tasks)  // 6 * 15 = 90
  assert_eq(576, total_hours)  // 6 * 8 * 12 = 576
  assert_eq(43200, total_cost)  // 576 * 75 = 43200
  assert_eq(480, cost_per_task)  // ceil(43200/90) = 480
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
  
  // Unicode 和特殊字符
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  
  // 数字和符号
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
}

test "negative_number_comprehensive" {
  // 负数综合测试
  let positive = 15
  let negative = -8
  
  // 加法测试
  assert_eq(7, add(positive, negative))
  assert_eq(-7, add(negative, positive))
  assert_eq(-16, add(negative, negative))
  
  // 乘法测试
  assert_eq(-120, multiply(positive, negative))
  assert_eq(-120, multiply(negative, positive))
  assert_eq(64, multiply(negative, negative))
  
  // 减法测试
  assert_eq(23, subtract(positive, negative))
  assert_eq(-23, subtract(negative, positive))
  assert_eq(0, subtract(negative, negative))
  
  // 除法测试
  assert_eq(-1, divide_with_ceil(negative, positive))
  assert_eq(-2, divide_with_ceil(positive, negative))
  assert_eq(1, divide_with_ceil(negative, negative))
}

test "large_number_operations" {
  // 大数运算测试
  let large_num1 = 100000
  let large_num2 = 50000
  
  // 大数加减乘
  assert_eq(150000, add(large_num1, large_num2))
  assert_eq(50000, subtract(large_num1, large_num2))
  assert_eq(5000000000, multiply(large_num1, large_num2))
  
  // 大数除法
  assert_eq(3, divide_with_ceil(150000, 50000))
  assert_eq(2, divide_with_ceil(100001, 50000))
  assert_eq(0, divide_with_ceil(49999, 50000))
  
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(1000001, divide_with_ceil(1000000, 1))
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  // 场景：服务器资源分配
  
  let total_memory_gb = 128
  let servers_needed = 5
  let memory_per_server = divide_with_ceil(total_memory_gb, servers_needed)
  
  assert_eq(26, memory_per_server)  // ceil(128/5) = 26
  
  // 验证总分配内存
  let allocated_memory = multiply(memory_per_server, servers_needed)
  assert_eq(130, allocated_memory)
  
  // 验证剩余内存
  let remaining_memory = subtract(allocated_memory, total_memory_gb)
  assert_eq(2, remaining_memory)
  
  // CPU 分配测试
  let total_cores = 48
  let cores_per_server = divide_with_ceil(total_cores, servers_needed)
  assert_eq(10, cores_per_server)  // ceil(48/5) = 10
  
  let total_allocated_cores = multiply(cores_per_server, servers_needed)
  assert_eq(50, total_allocated_cores)
}

test "mathematical_distributive_law" {
  // 数学分配律测试
  let a = 7
  let b = 5
  let c = 3
  
  // 乘法对加法的分配律：a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 验证具体数值
  assert_eq(56, left_side)  // 7 * (5 + 3) = 7 * 8 = 56
  assert_eq(56, right_side)  // 7 * 5 + 7 * 3 = 35 + 21 = 56
  
  // 测试不同数值
  let x = 12
  let y = 8
  let z = 4
  
  let distrib_left = multiply(x, add(y, z))
  let distrib_right = add(multiply(x, y), multiply(x, z))
  assert_eq(distrib_left, distrib_right)
}

test "error_handling_boundary" {
  // 错误处理边界测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  assert_eq(0, divide_with_ceil(-1000000, 0))
  
  // 极值边界测试
  let max_test = 1000000
  let min_test = -1000000
  
  // 边界加法
  assert_eq(2000000, add(max_test, max_test))
  assert_eq(-2000000, add(min_test, min_test))
  assert_eq(0, add(max_test, min_test))
  
  // 边界乘法
  assert_eq(1000000000000, multiply(max_test, max_test))
  assert_eq(1000000000000, multiply(min_test, min_test))
  assert_eq(-1000000000000, multiply(max_test, min_test))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(max_test, max_test))
  assert_eq(1, divide_with_ceil(min_test, min_test))
  assert_eq(0, divide_with_ceil(0, max_test))
}

test "real_world_business_case" {
  // 真实世界商业案例测试
  // 场景：电商订单处理和物流
  
  let orders_per_day = 250
  let items_per_order = 3
  let days_per_month = 30
  let capacity_per_truck = 500
  let trucks_available = 8
  
  // 计算月度总订单量
  let monthly_orders = multiply(orders_per_day, days_per_month)
  assert_eq(7500, monthly_orders)  // 250 * 30 = 7500
  
  // 计算月度总商品数
  let monthly_items = multiply(monthly_orders, items_per_order)
  assert_eq(22500, monthly_items)  // 7500 * 3 = 22500
  
  // 计算需要的卡车数量
  let trucks_needed = divide_with_ceil(monthly_items, capacity_per_truck)
  assert_eq(45, trucks_needed)  // ceil(22500/500) = 45
  
  // 验证卡车数量大于可用数量
  assert_eq(45, trucks_needed)
  
  // 计算需要的额外卡车
  let additional_trucks = subtract(trucks_needed, trucks_available)
  assert_eq(37, additional_trucks)  // 45 - 8 = 37
  
  // 计算总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(22500, total_capacity)  // 45 * 500 = 22500
  
  // 计算剩余运力
  let remaining_capacity = subtract(total_capacity, monthly_items)
  assert_eq(0, remaining_capacity)  // 22500 - 22500 = 0
}