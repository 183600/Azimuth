// 高质量标准 MoonBit 测试用例
// 为 Azimuth 项目创建，包含核心功能的全面测试

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000, add(500, 500))
  assert_eq(1, add(-1, 2))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "subtract_function_validation" {
  // 减法函数验证测试
  assert_eq(3, subtract(8, 5))
  assert_eq(-3, subtract(5, 8))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
  assert_eq(-100, subtract(-50, 50))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, María!", greet("María"))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(a, add(a, 0))
  assert_eq(0, multiply(a, 0))
  
  // 分配律 a * (b + c) = a * b + a * c
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "packaging_optimization" {
  // 包装优化测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, items_per_box)
  let empty_space = subtract(total_capacity, total_items)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, empty_space)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let principal = 1000
  let interest_rate = 5
  let years = 3
  
  // 简化的复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极端数值
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 测试字符串边界情况
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
}