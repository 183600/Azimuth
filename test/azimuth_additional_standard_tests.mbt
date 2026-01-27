// Azimuth 额外的标准测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(7, 8))
  assert_eq(15, add(8, 7))
  assert_eq(0, add(5, -5))
  assert_eq(0, add(-5, 5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_mixed_operations" {
  // 测试除法向上取整与混合运算
  assert_eq(7, divide_with_ceil(add(10, 5), 2))
  assert_eq(3, divide_with_ceil(multiply(2, 5), 3))
  assert_eq(8, add(divide_with_ceil(15, 2), multiply(2, 2)))
}

test "greet_function_with_special_characters" {
  // 测试包含特殊字符的问候功能
  assert_eq_string("Hello, John-Doe!", greet("John-Doe"))
  assert_eq_string("Hello, O'Reilly!", greet("O'Reilly"))
  assert_eq_string("Hello, C++!", greet("C++"))
  assert_eq_string("Hello, Python@3.9!", greet("Python@3.9"))
}

test "complex_financial_calculation" {
  // 复杂财务计算场景
  let principal = 10000
  let interest_rate = 5
  let years = 3
  
  // 简化的复利计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate), 100))
  
  assert_eq(10500, year1_amount)
  assert_eq(11025, year2_amount)
  assert_eq(11576, year3_amount)
}

test "inventory_optimization" {
  // 库存优化计算
  let demand_forecast = 157
  let safety_stock = 23
  let reorder_point = add(demand_forecast, safety_stock)
  let package_quantity = 24
  let packages_to_order = divide_with_ceil(reorder_point, package_quantity)
  
  assert_eq(180, reorder_point)
  assert_eq(8, packages_to_order)
}

test "temperature_conversion" {
  // 温度转换计算（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  
  assert_eq(77, fahrenheit)
}

test "data_structure_simulation" {
  // 模拟数据结构操作
  let array_size = 10
  let element_size = 4
  let total_memory = multiply(array_size, element_size)
  let pages_needed = divide_with_ceil(total_memory, 32)  // 假设每页32字节
  
  assert_eq(40, total_memory)
  assert_eq(2, pages_needed)
}

test "algorithm_complexity" {
  // 算法复杂度计算
  let n = 100
  // O(n^2) 操作数
  let quadratic_ops = multiply(n, n)
  // O(n log n) 操作数（简化为 O(n * log2(n))）
  let log_approx = divide_with_ceil(n, 10)  // 简化的对数近似
  let n_log_n_ops = multiply(n, log_approx)
  
  assert_eq(10000, quadratic_ops)
  assert_eq(1000, n_log_n_ops)
}

test "network_throughput" {
  // 网络吞吐量计算
  let packet_size = 1500  // 字节
  let packets_per_second = 1000
  let bandwidth_bps = multiply(packet_size, packets_per_second)
  let bandwidth_mbps = divide_with_ceil(bandwidth_bps, 1000000)
  
  assert_eq(1500000, bandwidth_bps)
  assert_eq(2, bandwidth_mbps)
}