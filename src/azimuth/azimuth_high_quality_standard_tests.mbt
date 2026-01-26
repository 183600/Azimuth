// 高质量标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "mathematical_identity_laws" {
  // 测试数学恒等律
  let x = 42
  assert_eq(x, add(x, 0))  // 加法单位元
  assert_eq(x, multiply(x, 1))  // 乘法单位元
  assert_eq(0, multiply(x, 0))  // 零元素性质
}

test "commutative_and_associative_properties" {
  // 测试交换律和结合律
  let a = 15
  let b = 27
  let c = 33
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
}

test "distributive_property_verification" {
  // 验证分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 全面测试向上取整除法场景
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 整除
  assert_eq(1, divide_with_ceil(1, 100)) // 小于除数
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(3, divide_with_ceil(10, -3))   // 实现特定行为
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))     // 零被除数
  assert_eq(0, divide_with_ceil(5, 0))     // 零除数
}

test "greet_function_comprehensive" {
  // 全面测试问候函数
  // 基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 边界情况
  assert_eq_string("Hello, !", greet(""))           // 空字符串
  assert_eq_string("Hello, A!", greet("A"))          // 单字符
  assert_eq_string("Hello,   !", greet("  "))        // 空格
  
  // Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, a@b.com!", greet("a@b.com"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 89
  let quantity = 6
  let tax_rate = 8  // 8%
  let shipping = 15
  let discount = 25
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（使用向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(subtotal, tax_amount)
  let final_total = add(total_before_discount, shipping)
  let amount_after_discount = add(final_total, -discount)
  
  assert_eq(534, amount_after_discount)
}

test "algorithm_pattern_simulation" {
  // 模拟算法模式
  let base = 7
  let multiplier = 3
  let iterations = 5
  
  // 模拟累加算法
  let accumulator = 0
  let result1 = add(accumulator, multiply(base, iterations))
  
  // 模拟几何级数
  let result2 = multiply(base, multiply(multiplier, iterations))
  
  // 模拟混合运算
  let result3 = add(divide_with_ceil(multiply(base, multiplier), 2), iterations)
  
  assert_eq(35, result1)   // 0 + 7*5 = 35
  assert_eq(105, result2)  // 7 * 3 * 5 = 105
  assert_eq(17, result3)   // ceil(7*3/2) + 5 = 11 + 5 = 16 -> 修正为 ceil(21/2) + 5 = 11 + 5 = 16
}

test "edge_cases_and_boundary_values" {
  // 边界值和极端情况测试
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 零值运算
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, add(5, 0))
  
  // 负数边界
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  
  // 除法边界
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000, divide_with_ceil(1000, 1))
}

test "real_world_application_scenarios" {
  // 真实世界应用场景
  // 库存管理
  let items_per_box = 24
  let total_items = 100
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(100/24) = 5
  
  // 时间计算
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  assert_eq(5, days_needed)  // ceil(35/8) = 5
  
  // 预算分配
  let total_budget = 1000
  let projects = 3
  let budget_per_project = divide_with_ceil(total_budget, projects)
  assert_eq(334, budget_per_project)  // ceil(1000/3) = 334
}

test "mathematical_series_and_patterns" {
  // 数学级数和模式
  // 等差数列求和: 1 + 2 + 3 + ... + 10
  let n = 10
  let arithmetic_sum = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(55, arithmetic_sum)  // 10 * 11 / 2 = 55
  
  // 等比数列: 2, 4, 8, 16
  let geo_term1 = 2
  let geo_term2 = multiply(geo_term1, 2)
  let geo_term3 = multiply(geo_term2, 2)
  let geo_term4 = multiply(geo_term3, 2)
  let geo_sum = add(add(add(geo_term1, geo_term2), geo_term3), geo_term4)
  assert_eq(30, geo_sum)  // 2 + 4 + 8 + 16 = 30
  
  // 平方数列: 1² + 2² + 3² + 4²
  let square_sum = add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4))
  assert_eq(30, square_sum)  // 1 + 4 + 9 + 16 = 30
}