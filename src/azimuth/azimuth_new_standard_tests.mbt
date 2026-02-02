// Azimuth 新标准测试用例
// 使用标准 MoonBit 测试语法的简洁测试集合

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(7, 8))
  assert_eq(15, add(8, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 3
  let b = 4
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(5, divide_with_ceil(21, 5))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "subtract_function_edge_cases" {
  // 测试减法的边界情况
  assert_eq(0, subtract(10, 10))
  assert_eq(-10, subtract(0, 10))
  assert_eq(10, subtract(10, 0))
  assert_eq(20, subtract(5, -15))
}

test "greet_function_multilingual_support" {
  // 测试多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_financial_calculation_new" {
  // 复杂金融计算场景
  let principal = 1000
  let rate = 5
  
  // 简化复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "inventory_optimization" {
  // 库存优化计算
  let demand = 47
  let batch_size = 12
  let safety_stock = 5
  
  let batches_needed = divide_with_ceil(demand, batch_size)
  let total_stock = add(multiply(batches_needed, batch_size), safety_stock)
  
  assert_eq(4, batches_needed)
  assert_eq(53, total_stock)
}

test "mathematical_sequence_fibonacci" {
  // 斐波那契数列计算
  let a = 1
  let b = 1
  let c = add(a, b)
  let d = add(b, c)
  let e = add(c, d)
  
  assert_eq(2, c)
  assert_eq(3, d)
  assert_eq(5, e)
}

test "resource_allocation_algorithm" {
  // 资源分配算法
  let total_resources = 100
  let projects = 3
  let overhead = 10
  
  let base_allocation = divide_with_ceil(total_resources, projects)
  let adjusted_allocation = add(base_allocation, -overhead)
  
  assert_eq(34, base_allocation)
  assert_eq(24, adjusted_allocation)
}

test "error_handling_boundary_values" {
  // 错误处理边界值测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}