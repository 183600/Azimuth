// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "addition_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))  // 15 + 27 = 27 + 15
}

test "multiplication_zero_property" {
  // 测试乘法零性质
  let x = 42
  assert_eq(0, multiply(x, 0))  // 任何数乘以0等于0
  assert_eq(0, multiply(0, x))  // 0乘以任何数等于0
}

test "divide_with_ceil_positive_fractions" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))    // 10/3 = 3.33... -> 4
  assert_eq(3, divide_with_ceil(9, 3))     // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(5, 8))     // 5/8 = 0.625 -> 1
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.33... -> -3
  assert_eq(3, divide_with_ceil(10, -3))   // 10/-3 = -3.33... -> -3
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.66... -> 4
}

test "divide_with_ceil_zero_divisor" {
  // 测试除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))   // 除数为0时返回0
  assert_eq(0, divide_with_ceil(-50, 0))   // 负数除以0返回0
  assert_eq(0, divide_with_ceil(0, 0))     // 0除以0返回0
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))   // 空字符串测试
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)           // 25 * 8 = 200
  assert_eq(215, total_before_discount)  // 200 + 15 = 215
  assert_eq(195, final_total)        // 215 - 20 = 195
}

test "packaging_optimization" {
  // 包装优化计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "mathematical_distributive_property" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "edge_case_operations" {
  // 测试边界情况
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(3, 4), 2)
  assert_eq(14, result2)  // (3 + 4) * 2 = 7 * 2 = 14
}

test "practical_applications" {
  // 测试实际应用场景
  // 包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 价格计算
  let price = 15
  let quantity = 3
  let total_cost = multiply(price, quantity)
  assert_eq(45, total_cost)
}

test "string_and_unicode_support" {
  // 测试字符串和Unicode支持
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, María!", greet("María"))
}

// 新增的测试用例
test "large_number_operations" {
  // 测试大数运算
  assert_eq(20000, add(10000, 10000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1, divide_with_ceil(10000, 10000))
}

test "complex_division_scenarios" {
  // 测试复杂除法场景
  assert_eq(1, divide_with_ceil(3, 10))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(-2, divide_with_ceil(-15, 8))
  assert_eq(2, divide_with_ceil(-15, -8))
}

test "mathematical_laws_verification" {
  // 测试数学定律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "practical_business_calculations" {
  // 测试实际业务计算
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_cost = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax_amount)
  assert_eq(231, total_cost)
}

test "resource_planning_calculations" {
  // 测试资源规划计算
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  let total_hours = multiply(work_days_needed, 8)
  
  assert_eq(11, work_days_needed)
  assert_eq(3, weeks_needed)
  assert_eq(88, total_hours)
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, <>&\"'!", greet("<>&\"'"))
}

test "error_handling_robustness" {
  // 测试错误处理健壮性
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试极值运算
  assert_eq(0, multiply(0, 1000000))
  assert_eq(0, divide_with_ceil(0, 1000000))
}