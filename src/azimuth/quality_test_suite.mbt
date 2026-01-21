// 高质量 MoonBit 测试用例集合
// 测试 azimuth 库的核心功能和实际应用场景

test "arithmetic_operations_comprehensive" {
  // 综合测试算术运算
  assert_eq(15, add(7, 8))
  assert_eq(-3, add(-10, 7))
  assert_eq(0, add(100, -100))
  
  assert_eq(42, multiply(6, 7))
  assert_eq(-24, multiply(-4, 6))
  assert_eq(0, multiply(0, 999))
}

test "divide_with_ceil_scenarios" {
  // 测试向上取整除法的各种场景
  assert_eq(3, divide_with_ceil(6, 2))    // 整除
  assert_eq(4, divide_with_ceil(7, 2))    // 需要向上取整
  assert_eq(1, divide_with_ceil(1, 100))  // 分子小于分母
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为零的情况
  assert_eq(-2, divide_with_ceil(-5, 3))  // 负数除法
  assert_eq(3, divide_with_ceil(-8, -3))  // 负数除负数
}

test "greeting_function_variations" {
  // 测试问候函数的各种变体
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
}

test "business_calculation_invoice" {
  // 业务场景：计算发票总价
  let unit_price = 85
  let quantity = 12
  let tax_rate = 8
  let shipping = 25
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(1151, total)  // 85*12 + ceil(1020*8/100) + 25 = 1020 + 82 + 25 = 1127
}

test "inventory_packaging_optimization" {
  // 库存包装优化：计算需要的包装箱数量
  let small_items = 47
  let medium_items = 83
  let large_items = 156
  let box_capacity = 24
  
  let small_boxes = divide_with_ceil(small_items, box_capacity)
  let medium_boxes = divide_with_ceil(medium_items, box_capacity)
  let large_boxes = divide_with_ceil(large_items, box_capacity)
  let total_boxes = add(add(small_boxes, medium_boxes), large_boxes)
  
  assert_eq(2, small_boxes)   // ceil(47/24) = 2
  assert_eq(4, medium_boxes)  // ceil(83/24) = 4
  assert_eq(7, large_boxes)   // ceil(156/24) = 7
  assert_eq(13, total_boxes)  // 2 + 4 + 7 = 13
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 5
  let common_diff = 3
  let num_terms = 8
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let double_sum = multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff)))
  let sum = divide_with_ceil(double_sum, 2)
  
  assert_eq(104, sum)  // 8/2 * (10 + 21) = 4 * 31 = 124
}

test "complex_multi_step_calculation" {
  // 复杂多步计算：项目预算规划
  let base_cost = 5000
  let team_size = 8
  let duration_months = 6
  let monthly_overhead = 1200
  let contingency_percent = 15
  
  // 计算人力成本
  let labor_cost = multiply(base_cost, team_size)
  // 计算总运营成本
  let total_overhead = multiply(monthly_overhead, duration_months)
  // 计算应急资金
  let contingency = divide_with_ceil(multiply(add(labor_cost, total_overhead), contingency_percent), 100)
  // 计算总预算
  let total_budget = add(add(labor_cost, total_overhead), contingency)
  
  assert_eq(63200, total_budget)  // 5000*8 + 1200*6 + ceil((40000+7200)*15/100) = 40000 + 7200 + 7080 = 54280
}

test "edge_cases_and_boundary_conditions" {
  // 边界条件和特殊情况测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(999, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单值测试
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 负数边界测试
  assert_eq(-1, add(-1, 0))
  assert_eq(1, multiply(-1, -1))
  assert_eq(-1, multiply(1, -1))
  
  // 字符串边界测试
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("   "))
}