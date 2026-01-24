// Azimuth 库的增强测试用例
// 使用标准 MoonBit 测试语法

test "enhanced_error_handling_division" {
  // 增强的错误处理测试：除数为零的多种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
}

test "enhanced_mathematical_series" {
  // 数学级数：等比数列计算
  let first_term = 2
  let ratio = 3
  let terms = 5
  
  // 计算等比数列的和：a * (r^n - 1) / (r - 1)
  let power_n = multiply(multiply(multiply(ratio, ratio), ratio), ratio)  // 3^4 = 81
  let numerator = add(multiply(first_term, power_n), -first_term)  // 2 * (81 - 1) = 160
  let sum = divide_with_ceil(numerator, add(ratio, -1))  // 160 / 2 = 80
  
  assert_eq(242, sum)  // 2 + 6 + 18 + 54 + 162 = 242
}

test "enhanced_financial_compound_interest" {
  // 金融复利计算的精确测试
  let principal = 10000
  let rate_percent = 5
  let years = 4
  
  // 年度复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  let year4 = add(year3, divide_with_ceil(multiply(year3, rate_percent), 100))
  
  assert_eq(12167, year4)  // 由于向上取整，结果略高于标准复利
}

test "enhanced_resource_optimization" {
  // 资源优化算法测试
  let total_resources = 1000
  let project_requirements = multiply(45, 8)
  let contingency_percentage = 10
  
  let allocated_resources = project_requirements
  let contingency = divide_with_ceil(multiply(allocated_resources, contingency_percentage), 100)
  let total_allocated = add(allocated_resources, contingency)
  let remaining = add(total_resources, -total_allocated)
  
  assert_eq(360, allocated_resources)
  assert_eq(36, contingency)
  assert_eq(396, total_allocated)
  assert_eq(604, remaining)
}

test "enhanced_string_validation" {
  // 字符串验证和边界情况
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello,   multiple   spaces   !", greet("   multiple   spaces   "))
  assert_eq_string("Hello, newline\n!", greet("newline\n"))
  assert_eq_string("Hello, tab\tcharacter!", greet("tab\tcharacter"))
  assert_eq_string("Hello, mixed123!@#", greet("mixed123!@#"))
}

test "enhanced_temperature_conversion" {
  // 温度转换计算（模拟）
  let celsius = 25
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 1), 32)
  let back_to_celsius = divide_with_ceil(multiply(add(fahrenheit, -32), 5), 9)
  
  assert_eq(77, fahrenheit)  // 25°C = 77°F
  assert_eq(25, back_to_celsius)  // 转换回去
}

test "enhanced_performance_benchmark" {
  // 性能基准测试模拟
  let base_operations = 1000
  let optimization_factor = 4
  let overhead_percentage = 12
  
  let optimized_ops = multiply(base_operations, optimization_factor)
  let overhead = divide_with_ceil(multiply(optimized_ops, overhead_percentage), 100)
  let actual_performance = add(optimized_ops, -overhead)
  let improvement_ratio = divide_with_ceil(actual_performance, base_operations)
  
  assert_eq(4000, optimized_ops)
  assert_eq(480, overhead)
  assert_eq(3520, actual_performance)
  assert_eq(4, improvement_ratio)
}

test "enhanced_inventory_turnover" {
  // 库存周转率计算
  let beginning_inventory = 500
  let ending_inventory = 300
  let cost_of_goods_sold = 2400
  
  let average_inventory = divide_with_ceil(add(beginning_inventory, ending_inventory), 2)
  let inventory_turnover = divide_with_ceil(multiply(cost_of_goods_sold, 100), average_inventory)
  
  assert_eq(400, average_inventory)
  assert_eq(600, inventory_turnover)
}

test "enhanced_network_throughput" {
  // 网络吞吐量计算
  let packet_size = 1500
  let packets_per_second = 1000
  let transmission_overhead = 15
  
  let raw_throughput = multiply(packet_size, packets_per_second)
  let overhead_bytes = divide_with_ceil(multiply(raw_throughput, transmission_overhead), 100)
  let effective_throughput = add(raw_throughput, -overhead_bytes)
  
  assert_eq(1500000, raw_throughput)
  assert_eq(225000, overhead_bytes)
  assert_eq(1275000, effective_throughput)
}

test "enhanced_statistical_analysis" {
  // 统计分析计算
  let data_points = [15, 22, 28, 35, 42, 48, 55]
  let sum = add(add(add(add(add(add(data_points[0], data_points[1]), data_points[2]), data_points[3]), data_points[4]), data_points[5]), data_points[6])
  let mean = divide_with_ceil(sum, 7)
  let variance_base = add(add(add(add(add(add(multiply(data_points[0] - mean, data_points[0] - mean), multiply(data_points[1] - mean, data_points[1] - mean)), multiply(data_points[2] - mean, data_points[2] - mean)), multiply(data_points[3] - mean, data_points[3] - mean)), multiply(data_points[4] - mean, data_points[4] - mean)), multiply(data_points[5] - mean, data_points[5] - mean)), multiply(data_points[6] - mean, data_points[6] - mean))
  let variance = divide_with_ceil(variance_base, 7)
  
  assert_eq(245, sum)
  assert_eq(35, mean)
  assert_eq(154, variance)
}