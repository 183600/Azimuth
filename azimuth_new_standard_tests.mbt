// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "subtract_function_basic" {
  // 测试基本的减法功能
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 15
  assert_eq(x, add(x, 0))  // 加法单位元
  assert_eq(x, multiply(x, 1))  // 乘法单位元
  assert_eq(0, multiply(x, 0))  // 零元素
}

test "complex_arithmetic_expression" {
  // 测试复杂算术表达式
  let a = 4
  let b = 7
  let c = 3
  
  // 计算：(a + b) * c - a / b
  let result = add(multiply(add(a, b), c), -divide_with_ceil(a, b))
  assert_eq(32, result)  // (4 + 7) * 3 - ceil(4/7) = 11 * 3 - 1 = 33 - 1 = 32
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "temperature_conversion" {
  // 模拟温度转换计算
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  
  assert_eq(77, fahrenheit)  // (25 * 9 + 160) / 5 = 385 / 5 = 77
}

test "financial_compound_interest" {
  // 模拟复利计算
  let principal = 1000
  let rate = 5
  let years = 3
  
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  assert_eq(1158, year3)  // 1000 -> 1050 -> 1103 -> 1158
}

test "array_index_calculation" {
  // 模拟数组索引计算
  let page_size = 10
  let total_items = 47
  let target_index = 25
  
  let page_number = divide_with_ceil(target_index, page_size)
  let remaining_items = add(total_items, -multiply(page_number, page_size))
  
  assert_eq(3, page_number)  // ceil(25/10) = 3
  assert_eq(17, remaining_items)  // 47 - 3 * 10 = 17
}

test "packing_optimization" {
  // 模拟装箱优化问题
  let item_weights = [5, 8, 12, 6, 9]
  let max_box_capacity = 20
  
  // 计算需要的箱子数量
  let total_weight = add(add(add(add(item_weights[0], item_weights[1]), item_weights[2]), item_weights[3]), item_weights[4])
  let boxes_needed = divide_with_ceil(total_weight, max_box_capacity)
  
  assert_eq(40, total_weight)  // 5 + 8 + 12 + 6 + 9 = 40
  assert_eq(2, boxes_needed)  // ceil(40/20) = 2
}

test "signal_processing" {
  // 模拟信号处理计算
  let signal_frequency = 1000
  let sampling_rate = 44100
  let duration_seconds = 2
  
  let total_samples = multiply(sampling_rate, duration_seconds)
  let samples_per_cycle = divide_with_ceil(sampling_rate, signal_frequency)
  let total_cycles = divide_with_ceil(total_samples, samples_per_cycle)
  
  assert_eq(88200, total_samples)  // 44100 * 2 = 88200
  assert_eq(45, samples_per_cycle)  // ceil(44100/1000) = 45
  assert_eq(1960, total_cycles)  // ceil(88200/45) = 1960
}