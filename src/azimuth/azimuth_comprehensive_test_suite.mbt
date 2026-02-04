// Azimuth 综合测试套件 - 10个高质量测试用例
// 使用标准 MoonBit 测试语法

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
  assert_eq(1000, subtract(500, -500))
  assert_eq(-1000, subtract(-500, 500))
}

test "divide_with_ceil_comprehensive_boundary" {
  // 全面测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "arithmetic_operations_consistency" {
  // 测试算术运算的一致性和数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 测试加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 测试乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 测试分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 测试减法与加法的关系
  assert_eq(subtract(a, b), add(a, -b))
}

test "greet_function_unicode_comprehensive" {
  // 全面测试问候函数的 Unicode 字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🚀🛸!", greet("🚀🛸"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, こんにちは!", greet("こんにちは"))
}

test "complex_financial_calculations" {
  // 复杂金融计算场景
  let principal = 10000
  let rate_percent = 7
  let years = 3
  
  // 计算复利
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  // 验证结果
  assert_eq(10700, year1)
  assert_eq(11449, year2)
  assert_eq(12250, year3)
  
  // 计算总收益
  let total_return = subtract(year3, principal)
  assert_eq(2250, total_return)
}

test "inventory_logistics_optimization" {
  // 库存物流优化计算
  let warehouse_capacity = 500
  let product_a_qty = 120
  let product_b_qty = 85
  let product_c_qty = 200
  let safety_stock = 50
  
  // 计算总库存
  let total_inventory = add(add(product_a_qty, product_b_qty), product_c_qty)
  
  // 计算剩余容量
  let remaining_capacity = subtract(warehouse_capacity, total_inventory)
  
  // 计算需要的额外仓库空间
  let additional_space_needed = add(safety_stock, -remaining_capacity)
  let additional_warehouses = divide_with_ceil(additional_space_needed, warehouse_capacity)
  
  assert_eq(405, total_inventory)
  assert_eq(95, remaining_capacity)
  assert_eq(0, additional_warehouses)
}

test "mathematical_sequence_calculations" {
  // 数学序列计算
  let first_term = 3
  let common_difference = 4
  let terms_count = 6
  
  // 计算等差数列的第n项
  let nth_term = add(first_term, multiply(common_difference, terms_count - 1))
  
  // 计算等差数列的和
  let series_sum = divide_with_ceil(
    multiply(terms_count, add(first_term, nth_term)),
    2
  )
  
  assert_eq(23, nth_term)
  assert_eq(78, series_sum)
  
  // 验证斐波那契数列前几项
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试除零保护
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 999999
  assert_eq(1999999, add(large_num1, large_num2))
  assert_eq(1, subtract(large_num1, large_num2))
  
  // 测试极值运算
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(-2147483647, multiply(-1, 2147483647))
}

test "real_world_project_management" {
  // 真实世界项目管理计算
  let team_members = 8
  let tasks_per_member = 5
  let hours_per_task = 4
  let working_days = 5
  let hours_per_day = 8
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_member)
  
  // 计算总工时
  let total_hours = multiply(total_tasks, hours_per_task)
  
  // 计算需要的工作日
  let workdays_needed = divide_with_ceil(total_hours, multiply(hours_per_day, working_days))
  
  // 计算每周工作进度
  let weekly_capacity = multiply(team_members, multiply(hours_per_day, working_days))
  let weeks_needed = divide_with_ceil(total_hours, weekly_capacity)
  
  assert_eq(40, total_tasks)
  assert_eq(160, total_hours)
  assert_eq(4, workdays_needed)
  assert_eq(1, weeks_needed)
}

test "advanced_data_analysis" {
  // 高级数据分析计算
  let data_points = [10, 15, 20, 25, 30, 35, 40, 45, 50]
  
  // 计算数据总和（模拟）
  let data_sum = add(add(add(add(10, 15), add(20, 25)), add(30, 35)), add(40, add(45, 50)))
  
  // 计算平均值
  let data_count = 9
  let average = divide_with_ceil(data_sum, data_count)
  
  // 计算中位数（奇数个数据点）
  let median = 30
  
  // 计算范围
  let min_value = 10
  let max_value = 50
  let range = subtract(max_value, min_value)
  
  // 计算标准差的近似值（简化计算）
  let variance = divide_with_ceil(
    add(
      add(
        add(
          add(
            multiply(subtract(10, average), subtract(10, average)),
            multiply(subtract(15, average), subtract(15, average))
          ),
          add(
            multiply(subtract(20, average), subtract(20, average)),
            multiply(subtract(25, average), subtract(25, average))
          )
        ),
        add(
          multiply(subtract(30, average), subtract(30, average)),
          multiply(subtract(35, average), subtract(35, average))
        )
      ),
      add(
        add(
          multiply(subtract(40, average), subtract(40, average)),
          multiply(subtract(45, average), subtract(45, average))
        ),
        multiply(subtract(50, average), subtract(50, average))
      )
    ),
    data_count
  )
  
  assert_eq(270, data_sum)
  assert_eq(30, average)
  assert_eq(30, median)
  assert_eq(40, range)
  assert_eq(147, variance)
}