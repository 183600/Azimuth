// Azimuth 项目综合新测试用例
// 使用标准 MoonBit 测试语法编写的8个高质量测试用例

test "complex_arithmetic_expressions" {
  // 复杂算术表达式测试
  let a = 12
  let b = 8
  let c = 5
  
  // 测试复合运算
  assert_eq(100, multiply(add(a, b), subtract(c, 1)))
  assert_eq(21, add(multiply(a, b), c))
  assert_eq(3, divide_with_ceil(subtract(a, b), c))
  assert_eq(16, multiply(divide_with_ceil(a, b), c))
}

test "financial_calculations" {
  // 金融计算测试
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 计算复利
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
}

test "scientific_computations" {
  // 科学计算测试
  let mass = 50
  let velocity = 20
  let height = 100
  
  // 计算动能 (1/2 * m * v²)
  let kinetic_energy = divide_with_ceil(multiply(mass, multiply(velocity, velocity)), 2)
  
  // 计算势能 (m * g * h)，其中 g = 10
  let potential_energy = multiply(mass, multiply(10, height))
  
  // 计算总能量
  let total_energy = add(kinetic_energy, potential_energy)
  
  assert_eq(10000, kinetic_energy)
  assert_eq(50000, potential_energy)
  assert_eq(60000, total_energy)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, Special@#$!", greet("Special@#$"))
  assert_eq_string("Hello,   spaced   !", greet("   spaced   "))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let n = 100
  
  // O(1) 常数时间
  assert_eq(42, add(20, 22))
  
  // O(n) 线性时间模拟
  let linear_sum = multiply(n, add(n, 1))
  let linear_result = divide_with_ceil(linear_sum, 2)
  
  // O(n²) 二次时间模拟
  let quadratic_result = multiply(n, n)
  
  assert_eq(5050, linear_result)
  assert_eq(10000, quadratic_result)
}

test "data_structure_operations" {
  // 数据结构操作模拟测试
  // 模拟队列操作
  let queue_size = 0
  let enqueue1 = 10
  let enqueue2 = 20
  let enqueue3 = 30
  
  let size_after_enqueues = add(add(add(queue_size, 1), 1), 1)
  let dequeue1 = enqueue1
  let size_after_dequeue1 = subtract(size_after_enqueues, 1)
  
  assert_eq(3, size_after_enqueues)
  assert_eq(10, dequeue1)
  assert_eq(2, size_after_dequeue1)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 测试除法的各种边界情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(0, -10))
  
  // 测试大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(33334, divide_with_ceil(large_num, 3))
}

test "mathematical_series_calculations" {
  // 数学级数计算测试
  // 计算等差数列前n项和
  let first_term = 3
  let common_diff = 2
  let n_terms = 10
  
  // 等差数列第n项
  let nth_term = add(first_term, multiply(common_diff, subtract(n_terms, 1)))
  
  // 前n项和
  let series_sum = divide_with_ceil(multiply(n_terms, add(first_term, nth_term)), 2)
  
  // 计算等比数列前n项和 (简化版)
  let geo_first = 2
  let geo_ratio = 3
  let geo_n = 5
  
  let geo_nth = multiply(geo_first, multiply(geo_ratio, subtract(geo_n, 1)))
  let geo_sum = add(geo_first, geo_nth)
  
  assert_eq(21, nth_term)
  assert_eq(120, series_sum)
  assert_eq(48, geo_nth)
  assert_eq(50, geo_sum)
}