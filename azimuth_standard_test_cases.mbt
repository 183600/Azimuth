// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含10个高质量测试用例，覆盖各种场景和边界情况

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let x = -15
  let y = 8
  assert_eq(add(x, y), add(y, x))
  
  // 测试零的情况
  assert_eq(add(0, 25), add(25, 0))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 7
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 测试负数情况
  let x = -4
  let y = 6
  let z = 2
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法功能
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))  // 10/3 = 3.33 -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.33 -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // -10/-3 = 3.33 -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为0
  assert_eq(0, divide_with_ceil(0, 5))     // 被除数为0
  assert_eq(1, divide_with_ceil(5, 5))     // 相等数
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  
  // 边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping = 15
  let discount = 30
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价（含运费）
  let total_with_shipping = add(add(subtotal, tax), shipping)
  
  // 应用折扣
  let final_total = add(total_with_shipping, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax)
  assert_eq(400, total_with_shipping)
  assert_eq(370, final_total)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let common_diff = 2
  let num_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  // 手动验证前几项
  let manual_sum = add(add(add(add(3, 5), 7), 9), 11)  // 前5项和
  
  assert_eq(120, sum_formula)
  assert_eq(35, manual_sum)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除零保护
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  
  // 测试极值
  assert_eq(0, multiply(0, -2147483648))
  assert_eq(2147483647, add(2147483646, 1))
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景
  let small_items = 25
  let medium_items = 47
  let large_items = 83
  
  let small_box_capacity = 6
  let medium_box_capacity = 12
  let large_box_capacity = 18
  
  // 计算每种尺寸需要的箱子数量
  let small_boxes = divide_with_ceil(small_items, small_box_capacity)
  let medium_boxes = divide_with_ceil(medium_items, medium_box_capacity)
  let large_boxes = divide_with_ceil(large_items, large_box_capacity)
  
  // 计算总箱子数
  let total_boxes = add(add(small_boxes, medium_boxes), large_boxes)
  
  assert_eq(5, small_boxes)    // ceil(25/6) = 5
  assert_eq(4, medium_boxes)   // ceil(47/12) = 4
  assert_eq(5, large_boxes)    // ceil(83/18) = 5
  assert_eq(14, total_boxes)   // 5 + 4 + 5 = 14
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "张"
  let last_name = "三"
  let title = "工程师"
  
  // 构建完整名称
  let full_name = first_name + last_name + " (" + title + ")"
  let formal_greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三 (工程师)!", formal_greeting)
  
  // 测试特殊字符组合
  let special_chars = "!@#$%^&*()"
  let special_greeting = greet(special_chars)
  assert_eq_string("Hello, !@#$%^&*()!", special_greeting)
}

test "time_and_resource_calculation" {
  // 时间和资源计算
  let tasks_total = 127
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  // 计算需要的工作天数
  let work_days_needed = divide_with_ceil(tasks_total, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = multiply(work_days_needed, 8)
  
  assert_eq(16, work_days_needed)  // ceil(127/8) = 16
  assert_eq(4, weeks_needed)       // ceil(16/5) = 4
  assert_eq(128, total_hours)      // 16 * 8 = 128
}