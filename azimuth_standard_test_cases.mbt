// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，针对核心功能进行全面测试

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(15, -5))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(100, multiply(10, 10))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数处理
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, áéíóú!", greet("áéíóú"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  let unit_price = 45
  let quantity = 6
  let discount = 20
  let tax = 15
  
  let subtotal = multiply(unit_price, quantity)
  let after_discount = add(subtotal, -discount)
  let final_total = add(after_discount, tax)
  
  assert_eq(270, subtotal)
  assert_eq(250, after_discount)
  assert_eq(265, final_total)
}

test "packaging_optimization" {
  // 测试包装优化计算
  let total_items = 58
  let box_capacity = 12
  
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  let total_capacity = multiply(boxes_needed, box_capacity)
  let wasted_space = subtract(total_capacity, total_items)
  
  assert_eq(5, boxes_needed)
  assert_eq(60, total_capacity)
  assert_eq(2, wasted_space)
}

test "financial_compound_interest" {
  // 测试金融复利计算
  let principal = 1000
  let rate = 5
  
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "array_processing_simulation" {
  // 测试数组处理模拟
  let base_value = 10
  let increment = 3
  let iterations = 5
  
  // 模拟数组累加：base + (base+inc) + (base+2*inc) + ...
  let sum1 = base_value
  let sum2 = add(sum1, add(base_value, increment))
  let sum3 = add(sum2, add(base_value, multiply(2, increment)))
  let sum4 = add(sum3, add(base_value, multiply(3, increment)))
  let sum5 = add(sum4, add(base_value, multiply(4, increment)))
  
  assert_eq(10, sum1)
  assert_eq(23, sum2)
  assert_eq(39, sum3)
  assert_eq(58, sum4)
  assert_eq(80, sum5)
}