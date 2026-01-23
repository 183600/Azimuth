// 质量增强测试用例 - 针对 azimuth 库的全面测试
// 使用标准的 MoonBit 测试语法

test "add_function_boundary_cases" {
  // 测试加法函数的边界情况
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))  // 接近 Int 最大值
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))  // 接近 Int 最小值
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))  // 最大值与相反数相加
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))  // 大数相加
}

test "multiply_function_precision" {
  // 测试乘法函数的精度和边界情况
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))  // 大数乘法
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))  // 负数乘负数
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))  // 正数乘负数
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))  // 最大值乘零
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))  // 10/3 = 3.333... -> 4
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))  // -10/-3 = 3.333... -> 4
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))  // 0/5 = 0 -> 0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))  // 除数为零的安全处理
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏!", @azimuth.greet("🌍🌎🌏"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let interest_rate = 5
  let years = 3
  
  // 计算复利（简化版）
  let year1_amount = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, interest_rate), 100))
  let year2_amount = @azimuth.add(year1_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year1_amount, interest_rate), 100))
  let year3_amount = @azimuth.add(year2_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year2_amount, interest_rate), 100))
  
  @azimuth.assert_eq(10500, year1_amount)  // 10000 + ceil(10000*5/100) = 10000 + 500 = 10500
  @azimuth.assert_eq(11025, year2_amount)  // 10500 + ceil(10500*5/100) = 10500 + 525 = 11025
  @azimuth.assert_eq(11576, year3_amount)  // 11025 + ceil(11025*5/100) = 11025 + 551 = 11576
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 1000
  let daily_sales = 45
  let restock_batch = 200
  let days_to_simulate = 7
  
  // 计算一周后的库存
  let total_sold = @azimuth.multiply(daily_sales, days_to_simulate)
  let stock_before_restock = @azimuth.add(initial_stock, -total_sold)
  let final_stock = @azimuth.add(stock_before_restock, restock_batch)
  
  @azimuth.assert_eq(315, total_sold)  // 45 * 7 = 315
  @azimuth.assert_eq(685, stock_before_restock)  // 1000 - 315 = 685
  @azimuth.assert_eq(885, final_stock)  // 685 + 200 = 885
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 验证加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 验证乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证分配律 a * (b + c) = a * b + a * c
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distributive_left, distributive_right)
  
  // 验证零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}

test "real_world_project_planning" {
  // 实际项目规划场景
  let team_size = 8
  let tasks_per_person_per_day = 3
  let project_days = 15
  let buffer_percentage = 20
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(team_size, @azimuth.multiply(tasks_per_person_per_day, project_days))
  
  // 计算缓冲时间
  let buffer_days = @azimuth.divide_with_ceil(@azimuth.multiply(project_days, buffer_percentage), 100)
  
  // 计算总项目时间
  let total_project_days = @azimuth.add(project_days, buffer_days)
  
  @azimuth.assert_eq(360, total_tasks)  // 8 * 3 * 15 = 360
  @azimuth.assert_eq(3, buffer_days)  // ceil(15 * 20 / 100) = ceil(3) = 3
  @azimuth.assert_eq(18, total_project_days)  // 15 + 3 = 18
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 测试除数为零的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试极值运算
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试大数与小数的运算
  @azimuth.assert_eq(2147483648, @azimuth.add(max_int, 1))  // 可能溢出，但测试行为
  @azimuth.assert_eq(-2147483649, @azimuth.add(min_int, -1))  // 可能下溢，但测试行为
  
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))  // 空字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))  // 单字符
}