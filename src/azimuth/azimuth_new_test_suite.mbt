// Azimuth 新增标准测试套件
// 包含10个高质量测试用例，覆盖核心功能

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  let x = -10
  let y = 25
  assert_eq(add(x, y), add(y, x))
  
  let m = 0
  let n = 100
  assert_eq(add(m, n), add(n, m))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 验证具体数值：5 * (8 + 12) = 5 * 8 + 5 * 12 = 100
  assert_eq(100, left_side)
}

test "divide_with_ceil_positive_fractions" {
  // 测试正数的向上取整除法
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 -> 2
  assert_eq(4, divide_with_ceil(10, 3))   // 3.333... -> 4
  assert_eq(7, divide_with_ceil(13, 2))   // 6.5 -> 7
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  assert_eq(34, divide_with_ceil(100, 3))  // 33.333... -> 34
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))   // -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))   // -3.666... -> -4
  assert_eq(4, divide_with_ceil(-10, -3))   // 3.333... -> 4
  assert_eq(3, divide_with_ceil(10, -3))    // -3.333... -> -3
  assert_eq(0, divide_with_ceil(5, 0))      // 除数为零时返回0
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🚀🌙!", greet("🚀🌙"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 计算订单总价：商品价格 × 数量 + 运费 - 折扣
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(348, final_total)  // 89*4 + 12 - 20 = 356 + 12 - 20 = 348
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算
  // 计算等差数列的和：1 + 3 + 5 + ... + 19
  let first_term = 1
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(100, sum_formula)  // 10/2 * (2 + 18) = 5 * 20 = 100
}

test "packaging_optimization" {
  // 测试包装优化场景
  // 计算不同商品需要的包装箱数量
  let items_per_box = 24
  
  let product_a_qty = 50
  let product_b_qty = 75
  let product_c_qty = 120
  
  let boxes_a = divide_with_ceil(product_a_qty, items_per_box)
  let boxes_b = divide_with_ceil(product_b_qty, items_per_box)
  let boxes_c = divide_with_ceil(product_c_qty, items_per_box)
  
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(3, boxes_a)    // ceil(50/24) = 3
  assert_eq(4, boxes_b)    // ceil(75/24) = 4
  assert_eq(5, boxes_c)    // ceil(120/24) = 5
  assert_eq(12, total_boxes)  // 3 + 4 + 5 = 12
}

test "financial_compound_interest" {
  // 测试金融复利计算
  let principal = 1000
  let rate_percent = 5
  
  // 年度复利计算（使用向上取整）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1050, year1)  // 1000 + ceil(1000*5/100) = 1000 + 50 = 1050
  assert_eq(1103, year2)  // 1050 + ceil(1050*5/100) = 1050 + 53 = 1103
  assert_eq(1158, year3)  // 1103 + ceil(1103*5/100) = 1103 + 55 = 1158
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  // 测试极值运算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(1000, 0))
  assert_eq(0, multiply(0, -1000))
  
  // 测试除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试零的加法性质
  assert_eq(100, add(100, 0))
  assert_eq(-100, add(-100, 0))
  assert_eq(0, add(0, 0))
}