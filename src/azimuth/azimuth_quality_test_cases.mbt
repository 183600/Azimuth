// 高质量 MoonBit 测试用例 - 针对 azimuth 库核心功能的全面测试
// 包含边界情况、实际应用场景和数学性质验证

test "addition_commutative_and_associative" {

test "addition_commutative_and_associative" {
  // 测试加法交换律和结合律
  let a = 15
  let b = 27
  let c = 42
  
  // 交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 单位元性质: a + 0 = a
  assert_eq(a, add(a, 0))
  assert_eq(0, add(0, 0))
}

test "multiplication_properties_and_edge_cases" {
  // 测试乘法性质和边界情况
  let a = 12
  let b = 8
  
  // 交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质: a * 0 = 0
  assert_eq(0, multiply(a, 0))
  assert_eq(0, multiply(0, b))
  assert_eq(0, multiply(0, 0))
  
  // 单位元性质: a * 1 = a
  assert_eq(a, multiply(a, 1))
  assert_eq(b, multiply(1, b))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 全面测试向上取整除法的各种场景
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // 3.333... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为零
  assert_eq(0, divide_with_ceil(0, 5))    // 被除数为零
  assert_eq(1, divide_with_ceil(5, 5))    // 相等数
}

test "greet_function_multilingual_and_special_chars" {
  // 测试问候函数的多语言和特殊字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello, 123!", greet("123"))  // 数字
}

test "complex_business_calculation_workflow" {
  // 复杂业务计算工作流：订单处理
  let unit_price = 89
  let quantity = 6
  let tax_rate = 8  // 8%
  let shipping = 25
  let discount = 50
  
  // 步骤1: 计算小计
  let subtotal = multiply(unit_price, quantity)
  assert_eq(534, subtotal)  // 89 * 6
  
  // 步骤2: 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  assert_eq(43, tax_amount)  // ceil(534 * 8 / 100) = ceil(42.72) = 43
  
  // 步骤3: 计算总价
  let total_before_discount = add(add(subtotal, tax_amount), shipping)
  assert_eq(602, total_before_discount)  // 534 + 43 + 25
  
  // 步骤4: 应用折扣
  let final_total = add(total_before_discount, -discount)
  assert_eq(552, final_total)  // 602 - 50
}

test "mathematical_distributive_property" {
  // 验证数学分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 7 * (12 + 8) = 7 * 12 + 7 * 8 = 140
}

test "packaging_and_logistics_optimization" {
  // 包装和物流优化计算
  let product_a_qty = 45
  let product_b_qty = 67
  let product_c_qty = 23
  let items_per_box = 12
  
  // 计算每种产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a_qty, items_per_box)
  let boxes_b = divide_with_ceil(product_b_qty, items_per_box)
  let boxes_c = divide_with_ceil(product_c_qty, items_per_box)
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(4, boxes_a)   // ceil(45/12) = 4
  assert_eq(6, boxes_b)   // ceil(67/12) = 6
  assert_eq(2, boxes_c)   // ceil(23/12) = 2
  assert_eq(12, total_boxes)  // 4 + 6 + 2 = 12
}

test "financial_compound_interest_simulation" {
  // 金融复利计算模拟
  let principal = 10000
  let rate_percent = 6
  let years = 3
  
  // 简化的年复利计算（向上取整）
  let amount_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let amount_year2 = add(amount_year1, divide_with_ceil(multiply(amount_year1, rate_percent), 100))
  let amount_year3 = add(amount_year2, divide_with_ceil(multiply(amount_year2, rate_percent), 100))
  
  assert_eq(10600, amount_year1)  // 10000 + ceil(10000*6/100) = 10000 + 600
  assert_eq(11236, amount_year2)  // 10600 + ceil(10600*6/100) = 10600 + 636
  assert_eq(11911, amount_year3)  // 11236 + ceil(11236*6/100) = 11236 + 675
}