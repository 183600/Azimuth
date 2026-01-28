// 高质量 MoonBit 测试用例 - 专注于核心功能和边界情况

test "boundary_value_addition" {
  // 边界值加法测试
  assert_eq(20000, add(10000, 10000))
  assert_eq(0, add(5000, -5000))
  assert_eq(-15000, add(-8000, -7000))
}

test "precision_division_ceil" {
  // 向上取整除法精度测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  let unit_price = 89
  let quantity = 12
  let shipping = 25
  let discount = 50
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = total_before_discount - discount
  
  assert_eq(1068, subtotal)
  assert_eq(1093, total_before_discount)
  assert_eq(1043, final_total)
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 15
  let y = 25
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(3, add(x, y)), add(multiply(3, x), multiply(3, y)))
}

test "unicode_greeting_functionality" {
  // Unicode 问候功能测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "error_handling_division" {
  // 除法错误处理测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "real_world_packaging" {
  // 真实世界包装计算测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "scientific_data_processing" {
  // 科学数据处理测试
  let measurement1 = 125
  let measurement2 = 137
  let measurement3 = 119
  
  let sum_measurements = add(add(measurement1, measurement2), measurement3)
  let average = divide_with_ceil(sum_measurements, 3)
  
  assert_eq(381, sum_measurements)
  assert_eq(127, average)
}

test "financial_interest_calculation" {
  // 金融利息计算测试
  let principal = 10000
  let rate = 5
  
  let year1_interest = divide_with_ceil(multiply(principal, rate), 100)
  let year1_amount = add(principal, year1_interest)
  
  assert_eq(500, year1_interest)
  assert_eq(10500, year1_amount)
}

test "performance_algorithm_simulation" {
  // 性能算法模拟测试
  let input_size = 100
  let linear_factor = 3
  let constant_overhead = 20
  
  let linear_time = multiply(input_size, linear_factor)
  let total_time = add(linear_time, constant_overhead)
  
  assert_eq(300, linear_time)
  assert_eq(320, total_time)
}