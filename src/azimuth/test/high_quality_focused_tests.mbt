// 高质量专注测试用例
// 针对 azimuth 库的核心功能进行深度测试

test "addition_boundary_values" {
  // 测试加法的边界值
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
}

test "multiplication_precision" {
  // 测试乘法精度和大数运算
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3)) // -11/-3 = 3.666... -> 4
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))    // 除数为0的安全处理
}

test "greet_function_comprehensive" {
  // 全面测试问候函数
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 125
  let quantity = 8
  let tax_rate = 8
  let discount = 50
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total_before_discount = @azimuth.add(subtotal, tax_amount)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(1000, subtotal)
  @azimuth.assert_eq(80, tax_amount)
  @azimuth.assert_eq(1080, total_before_discount)
  @azimuth.assert_eq(1030, final_total)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 15
  let y = 25
  let z = 10
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律 a * (b + c) = a * b + a * c
  let distributive_left = @azimuth.multiply(x, @azimuth.add(y, z))
  let distributive_right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(distributive_left, distributive_right)
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_budget = 5000
  let project_cost = 1200
  let team_size = 8
  
  let remaining_budget = @azimuth.add(total_budget, -@azimuth.multiply(project_cost, 3))
  let budget_per_person = @azimuth.divide_with_ceil(remaining_budget, team_size)
  
  @azimuth.assert_eq(1400, remaining_budget)
  @azimuth.assert_eq(175, budget_per_person)
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_day = 6
  let total_tasks = 47
  let working_days = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = @azimuth.multiply(working_days, 8)
  
  @azimuth.assert_eq(8, working_days)  // ceil(47/6) = 8
  @azimuth.assert_eq(64, total_hours)  // 8 * 8 = 64
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试极值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 2147483647))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
}

test "data_analysis_simulation" {
  // 数据分析模拟测试
  let data_points = [@azimuth.add(10, 20), @azimuth.multiply(5, 8), @azimuth.divide_with_ceil(100, 3)]
  let sum = @azimuth.add(@azimuth.add(data_points[0], data_points[1]), data_points[2])
  let average = @azimuth.divide_with_ceil(sum, 3)
  
  @azimuth.assert_eq(30, data_points[0])  // 10 + 20
  @azimuth.assert_eq(40, data_points[1])  // 5 * 8
  @azimuth.assert_eq(34, data_points[2])  // ceil(100/3)
  @azimuth.assert_eq(104, sum)           // 30 + 40 + 34
  @azimuth.assert_eq(35, average)        // ceil(104/3)
}