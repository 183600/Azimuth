// 质量 MoonBit 测试用例 - 针对 azimuth 库的全面测试
// 使用标准 MoonBit 测试语法，包含核心功能测试

test "add_function_commutative_property" {

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(-5, 10), add(10, -5))
  assert_eq(add(0, 100), add(100, 0))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 7
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_edge_cases" {
  // 全面测试向上取整除法的边界情况
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))  // 3.33... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.33... -> -3
  assert_eq(3, divide_with_ceil(-9, -3))   // 3 -> 3
  
  // 零除数安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_multilingual_support" {
  // 测试多语言问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_business_calculation_scenario" {
  // 复杂业务计算场景：电商订单总价计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping = 12
  let discount = 20
  
  // 计算商品小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(subtotal, tax_amount)
  let final_total = add(add(total_before_discount, shipping), -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)  // ceil(356 * 8 / 100) = ceil(28.48) = 29
  assert_eq(385, total_before_discount)
  assert_eq(377, final_total)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证：5 + 8 + 11 + 14 + 17 + 20 = 75
  assert_eq(75, sum)
}

test "inventory_management_optimization" {
  // 库存管理优化：计算最优订货量
  let monthly_demand = 250
  let items_per_box = 24
  let safety_stock = 50
  
  // 计算基础订货箱数
  let base_boxes = divide_with_ceil(monthly_demand, items_per_box)
  
  // 计算安全库存箱数
  let safety_boxes = divide_with_ceil(safety_stock, items_per_box)
  
  // 总订货箱数
  let total_boxes = add(base_boxes, safety_boxes)
  
  assert_eq(11, base_boxes)    // ceil(250/24) = 11
  assert_eq(3, safety_boxes)   // ceil(50/24) = 3
  assert_eq(14, total_boxes)   // 11 + 3 = 14
}

test "financial_compound_interest_calculation" {
  // 金融复利计算（简化版）
  let principal = 1000
  let interest_rate = 5  // 5%
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  
  assert_eq(1050, year1)  // 1000 + ceil(1000*5/100) = 1000 + 50 = 1050
  assert_eq(1103, year2)  // 1050 + ceil(1050*5/100) = 1050 + 53 = 1103
  assert_eq(1159, year3)  // 1103 + ceil(1103*5/100) = 1103 + 56 = 1159
}

test "performance_benchmark_simulation" {
  // 性能基准测试模拟
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  
  // 优化后的操作数
  let optimized_ops = multiply(base_operations, optimization_factor)
  
  // 计算开销
  let overhead = divide_with_ceil(multiply(optimized_ops, overhead_percentage), 100)
  
  // 实际性能
  let actual_performance = add(optimized_ops, -overhead)
  
  // 性能提升倍数
  let improvement_ratio = divide_with_ceil(actual_performance, base_operations)
  
  assert_eq(3000, optimized_ops)
  assert_eq(450, overhead)
  assert_eq(2550, actual_performance)
  assert_eq(3, improvement_ratio)
}

test "unicode_and_special_character_processing" {
  // Unicode 和特殊字符处理测试
  let test_cases = [
    ("Hello, 世界!", "世界"),
    ("Hello, 🚀!", "🚀"),
    ("Hello, αβγ!", "αβγ"),
    ("Hello, ñáéíóú!", "ñáéíóú"),
    ("Hello, test@example.com!", "test@example.com"),
    ("Hello, C++!", "C++"),
    ("Hello, 100%!", "100%")
  ]
  
  // 测试每个用例
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, C++!", greet("C++"))
  assert_eq_string("Hello, 100%!", greet("100%"))
}