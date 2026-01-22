// 增强的 Azimuth 测试用例
// 包含10个新的标准 MoonBit 测试用例，覆盖不同应用场景和边界情况

test "mathematical_commutative_laws" {
  // 测试数学交换律：a + b = b + a 和 a * b = b * a
  let a = 17
  let b = 23
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(40, add(a, b))
  assert_eq(391, multiply(a, b))
  
  // 负数交换律测试
  let x = -15
  let y = 8
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

test "extreme_boundary_values" {
  // 测试极端边界值的运算
  let large_positive = 1000000
  let large_negative = -1000000
  let small_value = 1
  
  // 大数加法
  assert_eq(2000000, add(large_positive, large_positive))
  assert_eq(0, add(large_positive, large_negative))
  
  // 大数乘法
  assert_eq(1000000000000, multiply(large_positive, large_positive))
  assert_eq(-1000000000000, multiply(large_positive, large_negative))
  
  // 边界除法
  assert_eq(1000000, divide_with_ceil(large_positive, small_value))
  assert_eq(-1000000, divide_with_ceil(large_negative, small_value))
  assert_eq(1, divide_with_ceil(small_value, large_positive))
  assert_eq(0, divide_with_ceil(large_positive, 0))
}

test "division_precision_comprehensive" {
  // 全面测试除法精度
  // 测试各种余数情况
  assert_eq(4, divide_with_ceil(13, 4))    // 13/4 = 3.25 -> 4
  assert_eq(7, divide_with_ceil(50, 8))    // 50/8 = 6.25 -> 7
  assert_eq(2, divide_with_ceil(9, 5))     // 9/5 = 1.8 -> 2
  
  // 测试接近整数的情况
  assert_eq(10, divide_with_ceil(99, 10))  // 99/10 = 9.9 -> 10
  assert_eq(5, divide_with_ceil(101, 20))  // 101/20 = 5.05 -> 6
  
  // 测试负数精度
  assert_eq(-2, divide_with_ceil(-7, 3))   // -7/3 = -2.333... -> -2
  assert_eq(-3, divide_with_ceil(-8, 3))   // -8/3 = -2.666... -> -3
  assert_eq(3, divide_with_ceil(-7, -3))   // -7/-3 = 2.333... -> 3
  
  // 测试小数除大数
  assert_eq(1, divide_with_ceil(3, 10))    // 3/10 = 0.3 -> 1
  assert_eq(1, divide_with_ceil(1, 100))   // 1/100 = 0.01 -> 1
}

test "advanced_string_combinations" {
  // 测试高级字符串组合
  let first_name = "张"
  let last_name = "三"
  let title = "工程师"
  let company = "MoonBit公司"
  
  // 构建完整姓名
  let full_name = first_name + last_name
  let full_title = greet(full_name + "，" + title)
  
  assert_eq_string("Hello, 张三，工程师!", full_title)
  
  // 测试复杂字符串组合
  let department = "研发部"
  let project = "Azimuth项目"
  let team_info = company + " " + department + " " + project
  let team_greeting = greet(team_info)
  
  assert_eq_string("Hello, MoonBit公司 研发部 Azimuth项目!", team_greeting)
  
  // 测试包含数字和特殊字符的字符串
  let product_code = "MB-2024-001"
  let version = "v1.0.0"
  let complex_id = product_code + "_" + version
  let complex_greeting = greet(complex_id)
  
  assert_eq_string("Hello, MB-2024-001_v1.0.0!", complex_greeting)
}

test "area_calculation_practical" {
  // 实际面积计算测试
  let room_length = 12
  let room_width = 8
  let room_height = 3
  let door_area = 2
  let window_area = 6
  
  // 计算房间面积
  let floor_area = multiply(room_length, room_width)
  let wall_area = multiply(2, multiply(add(room_length, room_width), room_height))
  let ceiling_area = floor_area
  
  // 计算需要粉刷的总面积（墙壁和天花板，减去门窗）
  let paintable_area = add(wall_area, ceiling_area) - door_area - window_area
  
  // 计算需要的涂料量（假设每升涂料覆盖10平方米）
  let paint_needed = divide_with_ceil(paintable_area, 10)
  
  // 计算地砖数量（假设每块地砖1平方米）
  let tiles_needed = floor_area
  
  assert_eq(96, floor_area)
  assert_eq(120, wall_area)
  assert_eq(96, ceiling_area)
  assert_eq(208, paintable_area)
  assert_eq(21, paint_needed)
  assert_eq(96, tiles_needed)
}

test "physics_motion_calculation" {
  // 物理运动计算测试
  let initial_velocity = 10  // m/s
  let acceleration = 2       // m/s²
  let time = 5               // seconds
  
  // 计算最终速度：v = u + at
  let final_velocity = add(initial_velocity, multiply(acceleration, time))
  
  // 计算位移：s = ut + 0.5at²
  let displacement = add(multiply(initial_velocity, time), multiply(acceleration, multiply(time, time)) / 2)
  
  // 计算平均速度
  let average_velocity = divide_with_ceil(add(initial_velocity, final_velocity), 2)
  
  // 计算需要的分段数量（假设每段位移不超过50米）
  let segments_needed = divide_with_ceil(displacement, 50)
  
  assert_eq(20, final_velocity)
  assert_eq(75, displacement)
  assert_eq(15, average_velocity)
  assert_eq(2, segments_needed)
}

test "arithmetic_series_calculation" {
  // 等差数列计算测试
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 10
  
  // 计算第n项：an = a1 + (n-1)d
  let last_term = add(first_term, multiply(number_of_terms - 1, common_difference))
  
  // 计算数列和：S = n/2 * (a1 + an)
  let series_sum = divide_with_ceil(multiply(number_of_terms, add(first_term, last_term)), 2)
  
  // 计算平均值
  let average_term = divide_with_ceil(series_sum, number_of_terms)
  
  // 验数列中特定项的值
  let fifth_term = add(first_term, multiply(4, common_difference))
  let eighth_term = add(first_term, multiply(7, common_difference))
  
  assert_eq(39, last_term)
  assert_eq(210, series_sum)
  assert_eq(21, average_term)
  assert_eq(19, fifth_term)
  assert_eq(31, eighth_term)
}

test "business_percentage_calculations" {
  // 商业百分比计算测试
  let original_price = 200
  let discount_percent = 15
  let tax_percent = 8
  let profit_margin_percent = 20
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_percent) / 100
  let discounted_price = add(original_price, -discount_amount)
  
  // 计算税费
  let tax_amount = multiply(discounted_price, tax_percent) / 100
  let final_price = add(discounted_price, tax_amount)
  
  // 计算利润
  let cost_price = multiply(final_price, 100 - profit_margin_percent) / 100
  let profit_amount = add(final_price, -cost_price)
  
  // 计算利润率验证
  let actual_profit_margin = multiply(profit_amount, 100) / final_price
  
  assert_eq(30, discount_amount)
  assert_eq(170, discounted_price)
  assert_eq(14, tax_amount)
  assert_eq(184, final_price)
  assert_eq(147, cost_price)
  assert_eq(37, profit_amount)
  assert_eq(20, actual_profit_margin)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算的稳定性
  let max_32bit = 2147483647
  let min_32bit = -2147483648
  
  // 测试加法溢出边界
  let safe_add = add(max_32bit - 1000, 500)
  let safe_add_negative = add(min_32bit + 1000, -500)
  
  // 测试乘法边界
  let safe_multiply = multiply(46340, 46340)  // 接近sqrt(2^31-1)
  let safe_multiply_negative = multiply(-46340, 46340)
  
  // 测试极端除法
  let extreme_division = divide_with_ceil(max_32bit, 1)
  let extreme_division_large = divide_with_ceil(max_32bit, 1000000)
  
  assert_eq(2147482647, safe_add)
  assert_eq(-2147482648, safe_add_negative)
  assert_eq(2147395600, safe_multiply)
  assert_eq(-2147395600, safe_multiply_negative)
  assert_eq(2147483647, extreme_division)
  assert_eq(2148, extreme_division_large)
}

test "performance_benchmark_simple" {
  // 简单性能基准测试
  let base_value = 42
  let iterations = 1000
  
  // 模拟多次运算的性能测试
  let sum_result = multiply(base_value, iterations)
  let multiply_result = multiply(iterations, iterations)
  
  // 复杂运算链
  let chain_result = add(
    multiply(divide_with_ceil(sum_result, 100), 3),
    multiply(divide_with_ceil(multiply_result, 10000), 2)
  )
  
  // 测试批量处理效率
  let batch_size = 25
  let total_items = 1000
  let batches_needed = divide_with_ceil(total_items, batch_size)
  let last_batch_size = total_items - multiply(batches_needed - 1, batch_size)
  
  // 测试并行处理模拟
  let parallel_tasks = 4
  let items_per_task = divide_with_ceil(total_items, parallel_tasks)
  let max_task_load = items_per_task
  
  assert_eq(42000, sum_result)
  assert_eq(1000000, multiply_result)
  assert_eq(1460, chain_result)
  assert_eq(40, batches_needed)
  assert_eq(0, last_batch_size)
  assert_eq(250, items_per_task)
  assert_eq(250, max_task_load)
}