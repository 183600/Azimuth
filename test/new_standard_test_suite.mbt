// 标准 MoonBit 测试套件
// 针对 azimuth 库的10个核心测试用例

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
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))  // 相等数
  assert_eq(1, divide_with_ceil(1, 100))  // 被除数小于除数
  assert_eq(0, divide_with_ceil(0, 5))  // 零被除数
  assert_eq(0, divide_with_ceil(100, 0))  // 零除数
  assert_eq(-3, divide_with_ceil(-10, 3))  // 负数被除数
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
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

test "mathematical_series_sum" {
  // 数学级数求和：等差数列前 n 项和
  let first_term = 3
  let common_diff = 2
  let n_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(n_terms, add(multiply(2, first_term), multiply(n_terms - 1, common_diff))),
    2
  )
  
  assert_eq(120, sum)
}

test "inventory_packaging_optimization" {
  // 库存包装优化：计算需要的箱子数量
  let items_per_box = 24
  let product_a_qty = 100
  let product_b_qty = 250
  let product_c_qty = 1000
  
  let boxes_a = divide_with_ceil(product_a_qty, items_per_box)
  let boxes_b = divide_with_ceil(product_b_qty, items_per_box)
  let boxes_c = divide_with_ceil(product_c_qty, items_per_box)
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(5, boxes_a)
  assert_eq(11, boxes_b)
  assert_eq(42, boxes_c)
  assert_eq(58, total_boxes)
}

test "time_project_management" {
  // 时间项目管理：计算项目完成时间
  let tasks_per_day = 8
  let total_tasks = 45
  let work_days_per_week = 5
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  
  assert_eq(6, days_needed)
  assert_eq(2, weeks_needed)
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let interest_rate = 5
  
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "data_processing_pipeline" {
  // 数据处理管道：模拟数据处理流程
  let raw_data = 1000
  let processing_factor = 3
  let filter_rate = 20  // 20% 被过滤掉
  let enhancement_factor = 2
  
  // 处理数据
  let processed = multiply(raw_data, processing_factor)
  // 过滤数据
  let filtered = divide_with_ceil(multiply(processed, 100 - filter_rate), 100)
  // 增强数据
  let enhanced = multiply(filtered, enhancement_factor)
  
  assert_eq(3000, processed)
  assert_eq(2400, filtered)
  assert_eq(4800, enhanced)
}