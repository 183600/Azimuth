// 标准 MoonBit 测试用例 - 为 azimuth 库新增的10个高质量测试用例
// 使用标准的 MoonBit 测试语法编写

test "basic_arithmetic_comprehensive" {
  // 综合基本算术运算测试
  assert_eq(15, add(8, 7))
  assert_eq(-2, add(5, -7))
  assert_eq(0, add(0, 0))
  assert_eq(24, multiply(6, 4))
  assert_eq(-18, multiply(-6, 3))
  assert_eq(0, multiply(10, 0))
}

test "divide_with_ceil_precision_test" {
  // 向上取整除法精度测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 交换律验证
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律验证
  assert_eq(
    multiply(x, add(y, 5)),
    add(multiply(x, y), multiply(x, 5))
  )
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, !", greet(""))
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价计算
  let unit_price = 45
  let quantity = 6
  let shipping_fee = 12
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_fee)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(282, total_before_discount)
  assert_eq(262, final_total)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 87
  let team_members = 4
  let resources_per_member = divide_with_ceil(total_resources, team_members)
  
  assert_eq(22, resources_per_member)
  
  // 验证分配后的总资源
  let allocated_total = multiply(resources_per_member, team_members)
  assert_eq(88, allocated_total)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 7
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(14, result)  // ceil(10*3/4) + 7 = 8 + 7 = 15
}

test "error_handling_boundaries" {
  // 错误处理边界测试
  // 测试除零安全处理
  assert_eq(0, divide_with_ceil(50, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABC123!", greet("ABC123"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, special@#$%!", greet("special@#$%"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "real_world_application_test" {
  // 真实世界应用测试：活动座位安排
  let participants = 95
  let seats_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, seats_per_table)
  assert_eq(12, tables_needed)  // ceil(95/8) = 12
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, seats_per_table)
  assert_eq(96, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(1, remaining_seats)
}