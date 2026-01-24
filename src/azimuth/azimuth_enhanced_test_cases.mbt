// Azimuth 增强测试用例 - 标准MoonBit测试语法
// 包含8个高质量测试用例，覆盖边界情况和实际应用场景

test "add_function_overflow_boundary" {
  // 测试加法函数的溢出边界
  assert_eq(2147483646, add(2147483645, 1))
  assert_eq(-2147483647, add(-2147483646, -1))
  assert_eq(0, add(2147483647, -2147483647))
}

test "multiply_function_power_operations" {
  // 测试乘法函数的幂运算场景
  assert_eq(1, multiply(1, 1))
  assert_eq(256, multiply(16, 16))
  assert_eq(1024, multiply(32, 32))
  assert_eq(-1024, multiply(-32, 32))
}

test "divide_with_ceil_precision_edge" {
  // 测试向上取整除法的精度边界
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(9999, 100))
  assert_eq(-99, divide_with_ceil(-987, 10))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简化的复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
}

test "inventory_optimization_problem" {
  // 库存优化问题
  let products = [(25, 8), (47, 12), (156, 20)]
  
  let boxes_1 = divide_with_ceil(products[0].0, products[0].1)
  let boxes_2 = divide_with_ceil(products[1].0, products[1].1)
  let boxes_3 = divide_with_ceil(products[2].0, products[2].1)
  
  let total_boxes = add(add(boxes_1, boxes_2), boxes_3)
  assert_eq(16, total_boxes)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟
  let input_size = 1000
  
  // O(n) 线性算法
  let linear_ops = multiply(input_size, 3)
  
  // O(log n) 对数算法（简化模拟）
  let log_ops = divide_with_ceil(input_size, 100)
  
  // 验证复杂度差异
  let ratio = divide_with_ceil(linear_ops, log_ops)
  assert_eq(30, ratio)
}

test "resource_allocation_scenario" {
  // 资源分配场景
  let total_budget = 5000
  let projects = [1200, 800, 1500, 1000]
  
  let project_1 = projects[0]
  let project_2 = projects[1]
  let project_3 = projects[2]
  let project_4 = projects[3]
  
  let total_spent = add(add(add(project_1, project_2), project_3), project_4)
  let remaining = add(total_budget, -total_spent)
  
  // 计算可以支持的小项目数量
  let small_projects = divide_with_ceil(remaining, 300)
  
  assert_eq(4500, total_spent)
  assert_eq(500, remaining)
  assert_eq(2, small_projects)
}