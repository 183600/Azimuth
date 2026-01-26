// 标准 MoonBit 测试用例 - 核心功能测试
// 测试 azimuth 库的主要功能，包括数学运算和字符串处理

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let x = -10
  let y = 8
  assert_eq(add(x, y), add(y, x))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 12
  let c = 8
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_precision_testing" {
  // 测试向上取整除法的精度
  // 测试小数部分向上取整
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(4, divide_with_ceil(11, 3)) // 3.666... -> 4
  assert_eq(7, divide_with_ceil(20, 3)) // 6.666... -> 7
  
  // 测试负数向上取整
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(4, divide_with_ceil(-11, -3))  // 3.666... -> 4
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 7
  let tax_rate = 8  // 8%
  let shipping = 12
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(315, subtotal)  // 45 * 7
  assert_eq(26, tax)        // ceil(315 * 8 / 100) = ceil(25.2) = 26
  assert_eq(353, total)     // 315 + 26 + 12
}

test "mathematical_identity_properties" {
  // 测试数学单位元性质
  // 加法单位元：a + 0 = a
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  
  // 乘法单位元：a * 1 = a
  assert_eq(42, multiply(42, 1))
  assert_eq(-17, multiply(-17, 1))
  
  // 乘法零元：a * 0 = 0
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(-17, 0))
}

test "resource_allocation_scenario" {
  // 资源分配场景：计算项目所需资源
  let total_tasks = 67
  let tasks_per_team = 8
  let teams_needed = divide_with_ceil(total_tasks, tasks_per_team)
  
  let budget_per_team = 5000
  let total_budget = multiply(budget_per_team, teams_needed)
  
  assert_eq(9, teams_needed)      // ceil(67 / 8) = 9
  assert_eq(45000, total_budget)  // 9 * 5000
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  // 空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 单字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 特殊字符和符号
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, a+b=c!", greet("a+b=c"))
  
  // 数字字符串
  assert_eq_string("Hello, 12345!", greet("12345"))
}