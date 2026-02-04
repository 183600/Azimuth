// Azimuth 项目新增的 MoonBit 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法
// 覆盖边界条件、实际应用场景和数学性质

test "boundary_value_operations" {
  // 边界值测试
  assert_eq(2147483647, add(2147483646, 1))  // 接近Int最大值
  assert_eq(-2147483647, add(-2147483647, -1))  // 接近Int最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数相加
  assert_eq(1, divide_with_ceil(2147483647, 2147483647))  // 大数相除
  assert_eq(0, divide_with_ceil(-2147483647, 2147483647))  // 不同符号大数相除
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 计算复利（简化版本，使用向上取整）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(10800, year1_amount)
  assert_eq(11664, year2_amount)
  assert_eq(12597, year3_amount)
}

test "inventory_optimization" {
  // 库存优化计算
  let monthly_demand = 450
  let ordering_cost = 50
  let holding_cost_per_unit = 2
  
  // 计算经济订货量（简化版本）
  let eoq_numerator = multiply(multiply(2, monthly_demand), ordering_cost)
  let eoq = divide_with_ceil(eoq_numerator, holding_cost_per_unit)
  
  // 计算年订货次数
  let annual_orders = divide_with_ceil(multiply(monthly_demand, 12), eoq)
  
  assert_eq(45000, eoq_numerator)
  assert_eq(22500, eoq)
  assert_eq(3, annual_orders)
}

test "string_processing_internationalization" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, München!", greet("München"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, القاهرة!", greet("القاهرة"))
  assert_eq_string("Hello, 東京!", greet("東京"))
  assert_eq_string("Hello, 🌏!", greet("🌏"))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟
  let input_size = 1000
  let linear_constant = 3
  let quadratic_constant = 2
  
  // 模拟O(n)算法
  let linear_operations = multiply(input_size, linear_constant)
  
  // 模拟O(n²)算法
  let quadratic_operations = multiply(multiply(input_size, input_size), quadratic_constant)
  
  // 计算总操作数
  let total_operations = add(linear_operations, divide_with_ceil(quadratic_operations, 1000))
  
  assert_eq(3000, linear_operations)
  assert_eq(2000000, quadratic_operations)
  assert_eq(5000, total_operations)
}

test "data_structure_operations" {
  // 数据结构操作模拟
  let array_size = 100
  let hash_table_size = 151  // 通常是质数
  
  // 模拟数组访问
  let array_accesses = multiply(array_size, 10)
  
  // 模拟哈希表操作
  let hash_operations = divide_with_ceil(array_accesses, 2)
  
  // 计算哈希表负载因子
  let load_factor = divide_with_ceil(multiply(array_size, 100), hash_table_size)
  
  assert_eq(1000, array_accesses)
  assert_eq(500, hash_operations)
  assert_eq(67, load_factor)
}

test "engineering_calculations" {
  // 工程计算测试
  let beam_length = 10  // 米
  let load_per_meter = 500  // 千克/米
  let safety_factor = 2
  
  // 计算总载荷
  let total_load = multiply(beam_length, load_per_meter)
  
  // 应用安全系数
  let design_load = multiply(total_load, safety_factor)
  
  // 计算支撑点数量（每3米一个支撑）
  let support_points = divide_with_ceil(beam_length, 3)
  
  assert_eq(5000, total_load)
  assert_eq(10000, design_load)
  assert_eq(4, support_points)
}

test "statistical_analysis" {
  // 统计分析模拟
  let data_points = 50
  let sum_values = 1250
  let sum_squares = 40000
  
  // 计算平均值
  let mean = divide_with_ceil(sum_values, data_points)
  
  // 计算方差（简化版本）
  let variance_numerator = subtract(sum_squares, multiply(divide_with_ceil(multiply(sum_values, sum_values), data_points), 1))
  let variance = divide_with_ceil(variance_numerator, data_points)
  
  // 计算标准差（向上取整）
  let std_deviation = divide_with_ceil(variance, 1)
  
  assert_eq(25, mean)
  assert_eq(8750, variance_numerator)
  assert_eq(175, variance)
  assert_eq(175, std_deviation)
}