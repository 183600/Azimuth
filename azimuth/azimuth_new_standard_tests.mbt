// Azimuth 项目新增标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，涵盖不同场景和边界情况

test "advanced_arithmetic_combinations" {
  // 高级算术组合测试
  let a = 12
  let b = 7
  let c = 3
  
  // 复合运算：(a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(57, result1)  // (12 + 7) * 3 = 57
  
  // 复合运算：a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(87, result2)  // 12 * 7 + 3 = 87
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_tasks = 45
  let team_members = 8
  let working_days = 5
  
  // 计算每人每天需要完成的任务数
  let tasks_per_person_per_day = @azimuth.divide_with_ceil(
    @azimuth.divide_with_ceil(total_tasks, team_members), 
    working_days
  )
  
  @azimuth.assert_eq(2, tasks_per_person_per_day)
  
  // 验证总处理能力
  let total_capacity = @azimuth.multiply(
    @azimuth.multiply(tasks_per_person_per_day, team_members), 
    working_days
  )
  
  @azimuth.assert_eq(80, total_capacity)
  @azimuth.assert_true(total_capacity >= total_tasks)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算测试
  // 计算等差数列前n项和：1 + 3 + 5 + 7 + 9
  let n = 5
  let first_term = 1
  let common_difference = 2
  
  // 等差数列求和公式：n/2 * (2a + (n-1)d)
  let sum_formula = @azimuth.divide_with_ceil(
    @azimuth.multiply(n, @azimuth.add(
      @azimuth.multiply(2, first_term),
      @azimuth.multiply(@azimuth.subtract(n, 1), common_difference)
    )),
    2
  )
  
  @azimuth.assert_eq(25, sum_formula)
  
  // 验证通过逐项相加
  let manual_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 3), 5), 7), 9)
  @azimuth.assert_eq(sum_formula, manual_sum)
}

test "financial_calculation_rounding" {
  // 金融计算舍入测试
  let total_amount = 1000
  let payment_periods = 3
  
  // 计算每期支付金额（向上取整）
  let payment_per_period = @azimuth.divide_with_ceil(total_amount, payment_periods)
  @azimuth.assert_eq(334, payment_per_period)
  
  // 计算实际总支付金额
  let actual_total = @azimuth.multiply(payment_per_period, payment_periods)
  @azimuth.assert_eq(1002, actual_total)
  
  // 计算额外支付金额
  let extra_payment = @azimuth.subtract(actual_total, total_amount)
  @azimuth.assert_eq(2, extra_payment)
}

test "time_conversion_calculations" {
  // 时间转换计算测试
  let total_minutes = 475
  
  // 转换为小时和分钟
  let hours = @azimuth.divide_with_ceil(total_minutes, 60)
  let remaining_minutes = @azimuth.subtract(total_minutes, @azimuth.multiply(@azimuth.subtract(hours, 1), 60))
  
  @azimuth.assert_eq(8, hours)
  @azimuth.assert_eq(55, remaining_minutes)
  
  // 验证转换结果
  let total_back = @azimuth.add(@azimuth.multiply(@azimuth.subtract(hours, 1), 60), remaining_minutes)
  @azimuth.assert_eq(475, total_back)
}

test "inventory_optimization_problem" {
  // 库存优化问题测试
  let demand_forecast = 157
  let package_size = 24
  let storage_cost_per_package = 5
  
  // 计算需要的包装数量
  let packages_needed = @azimuth.divide_with_ceil(demand_forecast, package_size)
  @azimuth.assert_eq(7, packages_needed)
  
  // 计算总存储成本
  let total_storage_cost = @azimuth.multiply(packages_needed, storage_cost_per_package)
  @azimuth.assert_eq(35, total_storage_cost)
  
  // 计算剩余库存
  let total_supply = @azimuth.multiply(packages_needed, package_size)
  let remaining_inventory = @azimuth.subtract(total_supply, demand_forecast)
  @azimuth.assert_eq(11, remaining_inventory)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试极值情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1000000, 1000000))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(1000001, 1000000))
  
  // 测试零值交互
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.add(0, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.subtract(1000000, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1000000))
}

test "unicode_and_special_character_handling" {
  // Unicode和特殊字符处理测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 🏳️‍🌈!", @azimuth.greet("🏳️‍🌈"))
  
  // 测试特殊符号和数字组合
  @azimuth.assert_eq_string("Hello, C++!", @azimuth.greet("C++"))
  @azimuth.assert_eq_string("Hello, C#!", @azimuth.greet("C#"))
  @azimuth.assert_eq_string("Hello, JavaScript!", @azimuth.greet("JavaScript"))
}