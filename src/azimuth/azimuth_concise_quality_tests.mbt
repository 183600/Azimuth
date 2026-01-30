// Azimuth 库的简洁高质量测试用例
// 使用标准 MoonBit 测试语法

test "add_function_basic_operations" {
  // 测试基本加法运算
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(8, -3))
}

test "multiply_function_comprehensive" {
  // 测试乘法运算的各种情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(12, multiply(-3, -4))
}

test "greet_function_various_inputs" {
  // 测试问候函数的不同输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "combined_operations_business_scenario" {
  // 组合运算的业务场景
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_calculation_practical" {
  // 实际包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, items_per_box)
  let empty_space = add(total_capacity, -total_items)
  assert_eq(1, empty_space)
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 1000
  let rate_percent = 5
  let _years = 3
  
  // 简化的复利计算
  let amount_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let amount_year2 = add(amount_year1, divide_with_ceil(multiply(amount_year1, rate_percent), 100))
  let amount_year3 = add(amount_year2, divide_with_ceil(multiply(amount_year2, rate_percent), 100))
  
  assert_eq(1050, amount_year1)
  assert_eq(1103, amount_year2)
  assert_eq(1159, amount_year3)
}

test "edge_cases_and_boundary_values" {
  // 边界值和特殊情况测试
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 极端除法情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(999999, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  
  // 字符串边界情况
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, This is a very long name!", greet("This is a very long name"))
}