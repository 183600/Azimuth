// Azimuth 增强测试用例
// 标准 MoonBit 测试语法，包含8个高质量测试用例

test "advanced_arithmetic_sequence" {
  // 高级算术序列测试
  let start = 5
  let difference = 3
  let terms = 4
  
  // 计算等差数列的和
  let term1 = start
  let term2 = add(term1, difference)
  let term3 = add(term2, difference)
  let term4 = add(term3, difference)
  
  let sum = add(add(add(term1, term2), term3), term4)
  assert_eq(32, sum)  // 5 + 8 + 11 + 14 = 38
  
  // 计算平均值
  let average = divide_with_ceil(sum, terms)
  assert_eq(10, average)  // ceil(38/4) = 10
}

test "geometric_progression_calculation" {
  // 几何级数计算测试
  let first_term = 2
  let ratio = 3
  let terms = 4
  
  // 计算几何级数的各项
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  
  assert_eq(2, term1)
  assert_eq(6, term2)
  assert_eq(18, term3)
  assert_eq(54, term4)
  
  // 计算总和
  let sum = add(add(add(term1, term2), term3), term4)
  assert_eq(80, sum)  // 2 + 6 + 18 + 54 = 80
}

test "resource_optimization_scenario" {
  // 资源优化场景测试
  let total_tasks = 100
  let workers = 8
  let hours_per_day = 8
  let days_available = 5
  
  // 计算总工时
  let total_work_hours = multiply(workers, multiply(hours_per_day, days_available))
  assert_eq(320, total_work_hours)  // 8 * 8 * 5 = 320
  
  // 计算每小时需要完成的任务数
  let tasks_per_hour = divide_with_ceil(total_tasks, total_work_hours)
  assert_eq(1, tasks_per_hour)  // ceil(100/320) = 1
  
  // 计算实际完成时间
  let actual_hours_needed = divide_with_ceil(total_tasks, multiply(workers, tasks_per_hour))
  let actual_days_needed = divide_with_ceil(actual_hours_needed, hours_per_day)
  assert_eq(13, actual_hours_needed)  // ceil(100/(8*1)) = 13
  assert_eq(2, actual_days_needed)   // ceil(13/8) = 2
}

test "string_concatenation_validation" {
  // 字符串连接验证测试
  let prefix = "Azimuth"
  let suffix = "Project"
  
  // 测试问候函数的组合使用
  let greeting1 = greet(prefix)
  let greeting2 = greet(suffix)
  
  assert_eq_string("Hello, Azimuth!", greeting1)
  assert_eq_string("Hello, Project!", greeting2)
  
  // 测试空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   spaces   !", greet("  spaces   "))
}

test "mathematical_series_summation" {
  // 数学级数求和测试
  let n = 10
  
  // 计算平方和：1² + 2² + ... + n²
  let square_sum = 0
  let i = 1
  while (i <= n) {
    square_sum = add(square_sum, multiply(i, i))
    i = add(i, 1)
  }
  assert_eq(385, square_sum)  // 1² + 2² + ... + 10² = 385
  
  // 计算立方和：1³ + 2³ + ... + n³
  let cube_sum = 0
  i = 1
  while (i <= n) {
    cube_sum = add(cube_sum, multiply(multiply(i, i), i))
    i = add(i, 1)
  }
  assert_eq(3025, cube_sum)  // 1³ + 2³ + ... + 10³ = 3025
}

test "inventory_turnover_calculation" {
  // 库存周转率计算测试
  let annual_sales = 120000
  let average_inventory = 30000
  let days_per_year = 365
  
  // 计算库存周转率
  let turnover_rate = divide_with_ceil(annual_sales, average_inventory)
  assert_eq(4, turnover_rate)  // ceil(120000/30000) = 4
  
  // 计算库存周转天数
  let turnover_days = divide_with_ceil(days_per_year, turnover_rate)
  assert_eq(91, turnover_days)  // ceil(365/4) = 91
  
  // 计算月平均库存
  let monthly_inventory = divide_with_ceil(average_inventory, 12)
  assert_eq(2500, monthly_inventory)  // ceil(30000/12) = 2500
}

test "compound_interest_simulation" {
  // 复利计算模拟测试
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算（逐年计算）
  let year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, annual_rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, annual_rate), 100))
  
  assert_eq(10500, year1)  // 10000 + ceil(10000*5/100) = 10500
  assert_eq(11025, year2)  // 10500 + ceil(10500*5/100) = 11025
  assert_eq(11576, year3)  // 11025 + ceil(11025*5/100) = 11576
  
  // 计算总收益
  let total_return = subtract(year3, principal)
  assert_eq(1576, total_return)  // 11576 - 10000 = 1576
}

test "production_efficiency_metrics" {
  // 生产效率指标测试
  let units_produced = 1250
  let labor_hours = 250
  let machine_hours = 180
  let defect_rate = 2  // 2%
  
  // 计算单位产品劳动时间
  let labor_per_unit = divide_with_ceil(labor_hours, units_produced)
  assert_eq(1, labor_per_unit)  // ceil(250/1250) = 1
  
  // 计算单位产品机器时间
  let machine_per_unit = divide_with_ceil(machine_hours, units_produced)
  assert_eq(1, machine_per_unit)  // ceil(180/1250) = 1
  
  // 计算缺陷数量
  let defect_count = divide_with_ceil(multiply(units_produced, defect_rate), 100)
  assert_eq(25, defect_count)  // ceil(1250*2/100) = 25
  
  // 计算合格产品数量
  let good_units = subtract(units_produced, defect_count)
  assert_eq(1225, good_units)  // 1250 - 25 = 1225
  
  // 计算合格率
  let quality_rate = subtract(100, defect_rate)
  assert_eq(98, quality_rate)  // 100 - 2 = 98
}