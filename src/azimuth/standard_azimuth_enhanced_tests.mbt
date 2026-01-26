// 标准 MoonBit 测试用例 - Azimuth 库增强测试
// 包含基本功能、边界情况、数学性质和实际应用场景的测试

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果
  assert_eq(4, divide_with_ceil(10, 3))   // 3.333... -> 4
  assert_eq(3, divide_with_ceil(8, 3))    // 2.666... -> 3
  assert_eq(7, divide_with_ceil(13, 2))   // 6.5 -> 7
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 -> 2
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "negative_number_operations_comprehensive" {
  // 测试负数运算的综合情况
  assert_eq(-10, add(-3, -7))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "zero_element_properties" {
  // 测试零元素性质
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  let unit_price = 45
  let quantity = 12
  let discount_rate = 10
  let shipping_cost = 25
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算折扣
  let discount = divide_with_ceil(multiply(subtotal, discount_rate), 100)
  // 计算最终总价
  let final_total = add(add(subtotal, -discount), shipping_cost)
  
  assert_eq(540, subtotal)
  assert_eq(54, discount)
  assert_eq(511, final_total)
}

test "resource_allocation_calculation" {
  // 测试资源分配计算
  let total_items = 157
  let container_capacity = 25
  let containers_needed = divide_with_ceil(total_items, container_capacity)
  
  // 计算剩余空间
  let total_capacity = multiply(containers_needed, container_capacity)
  let remaining_space = add(total_capacity, -total_items)
  
  assert_eq(7, containers_needed)
  assert_eq(175, total_capacity)
  assert_eq(18, remaining_space)
}

test "mathematical_series_summation" {
  // 测试数学级数求和
  // 计算等差数列前n项和: 1 + 3 + 5 + ... + (2n-1) = n²
  let n = 10
  let series_sum = multiply(n, n)
  
  // 验证结果
  assert_eq(100, series_sum)
  
  // 使用另一种方法验证
  let manual_sum = add(add(add(add(add(1, 3), add(5, 7)), add(9, 11)), add(add(13, 15), add(17, 19)))
  assert_eq(series_sum, manual_sum)
}

test "time_and_project_management" {
  // 测试时间和项目管理计算
  let total_tasks = 67
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  // 计算总工作小时数
  let total_work_hours = multiply(work_days_needed, 8)
  
  assert_eq(9, work_days_needed)
  assert_eq(2, weeks_needed)
  assert_eq(72, total_work_hours)
}