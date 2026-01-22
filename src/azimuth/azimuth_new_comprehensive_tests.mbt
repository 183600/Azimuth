// 新增的高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "mathematical_laws_commutative" {
  // 测试数学交换律：加法和乘法
  let a = 17
  let b = 29
  assert_eq(add(a, b), add(b, a))  // 加法交换律
  assert_eq(multiply(a, b), multiply(b, a))  // 乘法交换律
}

test "mathematical_laws_associative" {
  // 测试数学结合律：加法和乘法
  let x = 5
  let y = 8
  let z = 12
  assert_eq(add(add(x, y), z), add(x, add(y, z)))  // 加法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))  // 乘法结合律
}

test "divide_with_ceil_comprehensive" {
  // 全面测试除法向上取整功能
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))  // 3.33 -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.33 -> -3
  assert_eq(-3, divide_with_ceil(-9, 3))   // -3 -> -3
  
  // 负数除负数
  assert_eq(4, divide_with_ceil(-10, -3))  // 3.33 -> 4
  
  // 除零安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
}

test "negative_number_operations" {
  // 测试负数运算的各种情况
  assert_eq(-10, add(-3, -7))    // 负数加法
  assert_eq(-5, add(-8, 3))     // 正负数加法
  assert_eq(0, add(-5, 5))      // 相反数相加
  assert_eq(15, multiply(-3, -5))  // 负数乘法（正结果）
  assert_eq(-15, multiply(3, -5))  // 正负数乘法（负结果）
  assert_eq(0, multiply(-5, 0))    // 负数乘零
}

test "extreme_boundary_values" {
  // 测试极端边界值
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000, multiply(1000000, 1000))
  
  // 零值运算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(12345, 0))
  assert_eq(12345, add(12345, 0))
  assert_eq(0, divide_with_ceil(0, 12345))
  
  // 单位元运算
  assert_eq(12345, multiply(12345, 1))
  assert_eq(12345, add(12345, 0))
}

test "greet_function_comprehensive" {
  // 全面测试问候函数
  // 基本功能
  assert_eq_string("Hello, World!", greet("World"))
  
  // 特殊情况
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello,  !", greet(" "))  // 空格
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  
  // Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, José!", greet("José"))
  
  // 数字和符号
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, a+b=c!", greet("a+b=c"))
}

test "complex_business_calculations" {
  // 复杂业务计算场景
  // 订单计算：商品价格 × 数量 + 运费 - 折扣
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)           // 89 * 4
  assert_eq(368, total_before_discount)  // 356 + 12
  assert_eq(348, final_total)        // 368 - 20
}

test "inventory_packaging_optimization" {
  // 库存包装优化计算
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
  assert_eq(58, total_boxes)  // 5 + 11 + 42
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证结果：10/2 * (6 + 18) = 5 * 24 = 120
  assert_eq(120, sum_formula)
}

test "distributive_property_verification" {
  // 验证分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 7 * (12 + 8) = 7 * 12 + 7 * 8 = 140
}

test "real_world_financial_calculation" {
  // 真实世界金融计算：复利计算
  let principal = 1000
  let interest_rate = 5  // 5%
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let year1_total = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_total, interest_rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  // 第三年
  let year3_interest = divide_with_ceil(multiply(year2_total, interest_rate), 100)
  let year3_total = add(year2_total, year3_interest)
  
  assert_eq(50, year1_interest)   // ceil(1000 * 5 / 100) = 50
  assert_eq(1050, year1_total)    // 1000 + 50
  assert_eq(53, year2_interest)   // ceil(1050 * 5 / 100) = 53
  assert_eq(1103, year2_total)    // 1050 + 53
  assert_eq(56, year3_interest)   // ceil(1103 * 5 / 100) = 56
  assert_eq(1159, year3_total)    // 1103 + 56
}