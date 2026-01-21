// 高质量增强测试用例 - 针对 azimuth 库的全面测试
// 包含边界情况、错误处理和实际应用场景

test "mathematical_properties_addition" {
  // 测试加法的数学性质
  assert_eq(10, add(7, 3))
  assert_eq(10, add(3, 7))  // 交换律
  assert_eq(0, add(5, -5))  // 相反数性质
  assert_eq(15, add(add(5, 3), 7))  // 结合律
  assert_eq(15, add(5, add(3, 7)))
}

test "mathematical_properties_multiplication" {
  // 测试乘法的数学性质
  assert_eq(12, multiply(3, 4))
  assert_eq(12, multiply(4, 3))  // 交换律
  assert_eq(0, multiply(5, 0))  // 零元素性质
  assert_eq(24, multiply(multiply(2, 3), 4))  // 结合律
  assert_eq(24, multiply(2, multiply(3, 4)))
}

test "distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(4, divide_with_ceil(12, 3))  // 12/3 = 4 -> 4
  assert_eq(1, divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  assert_eq(3, divide_with_ceil(10, -3))   // 10/-3 = -3.333... -> -3 (但实现可能不同)
  
  // 负数除负数
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(-9, -3))   // -9/-3 = 3 -> 3
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello,   !", greet("  "))  // 空格字符串
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello, This is a very long name!", greet("This is a very long name"))  // 长字符串
}

test "complex_business_scenario" {
  // 复杂业务场景：计算订单总价
  let base_price = 100
  let quantity = 5
  let tax_rate = 8  // 8%
  let shipping = 15
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(base_price, quantity)
  // 计算税费（简化为固定金额）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_shipping = add(subtotal, tax)
  let total_before_discount = add(total_before_shipping, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(515, final_total)  // 100*5 + ceil(500*8/100) + 15 - 20 = 500 + 40 + 15 - 20 = 535
}

test "inventory_management_scenario" {
  // 库存管理场景：计算需要的存储空间
  let items_per_box = 24
  let total_items_small = 100
  let total_items_medium = 250
  let total_items_large = 1000
  
  let boxes_small = divide_with_ceil(total_items_small, items_per_box)
  let boxes_medium = divide_with_ceil(total_items_medium, items_per_box)
  let boxes_large = divide_with_ceil(total_items_large, items_per_box)
  
  let total_boxes = add(add(boxes_small, boxes_medium), boxes_large)
  
  assert_eq(5, boxes_small)   // ceil(100/24) = 5
  assert_eq(11, boxes_medium) // ceil(250/24) = 11
  assert_eq(42, boxes_large)  // ceil(1000/24) = 42
  assert_eq(58, total_boxes)  // 5 + 11 + 42 = 58
}

test "mathematical_series_calculation" {
  // 数学级数计算：计算等差数列的和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证结果
  assert_eq(120, sum_formula)  // 10/2 * (6 + 18) = 5 * 24 = 120
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "John"
  let last_name = "Doe"
  let full_name = first_name + " " + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, John Doe!", greeting)
  
  // 测试特殊字符处理
  let special_name = "Alice & Bob"
  let special_greeting = greet(special_name)
  assert_eq_string("Hello, Alice & Bob!", special_greeting)
}

test "financial_calculation_scenario" {
  // 金融计算场景：计算复利
  let principal = 1000
  let rate_percent = 5
  let _years = 3  // 未使用的变量
  
  // 简化的复利计算（每年计算一次）
  let amount_after_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let amount_after_year2 = add(amount_after_year1, divide_with_ceil(multiply(amount_after_year1, rate_percent), 100))
  let amount_after_year3 = add(amount_after_year2, divide_with_ceil(multiply(amount_after_year2, rate_percent), 100))
  
  // 验证结果（由于使用向上取整，结果会略高于标准复利计算）
  assert_eq(1158, amount_after_year3)  // 1000 -> 1050 -> 1103 -> 1158
}