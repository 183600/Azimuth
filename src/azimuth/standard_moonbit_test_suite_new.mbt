// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_comprehensive" {
  // 测试加法函数的各种情况
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
  assert_eq(-1, add(5, -6))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(100, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(10000, multiply(100, 100))
  assert_eq(1, multiply(-1, -1))
}

test "greet_function_special_cases" {
  // 测试问候函数的特殊情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的全面情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "packaging_optimization" {
  // 包装优化计算
  let total_items = 47
  let box_capacity = 12
  let cost_per_box = 5
  
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  let total_cost = multiply(boxes_needed, cost_per_box)
  
  assert_eq(4, boxes_needed)
  assert_eq(20, total_cost)
}

test "string_processing_workflow" {
  // 字符串处理工作流
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
}

test "financial_calculation_scenario" {
  // 金融计算场景
  let principal = 1000
  let interest_rate = 5
  let years = 3
  
  let year1_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let year1_total = add(principal, year1_interest)
  let year2_interest = divide_with_ceil(multiply(year1_total, interest_rate), 100)
  let year2_total = add(year1_total, year2_interest)
  let year3_interest = divide_with_ceil(multiply(year2_total, interest_rate), 100)
  let final_total = add(year2_total, year3_interest)
  
  assert_eq(50, year1_interest)
  assert_eq(1050, year1_total)
  assert_eq(1158, final_total)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试边界值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}