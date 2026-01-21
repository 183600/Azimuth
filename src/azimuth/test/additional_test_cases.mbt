// 额外的测试用例 - 针对 azimuth 库的补充测试
// 使用标准的 MoonBit 测试语法

test "add_function_commutative_property" {
  // 测试加法交换律
  @azimuth.assert_eq(15, @azimuth.add(8, 7))
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 3
  let b = 4
  let c = 5
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "divide_with_ceil_fractional_cases" {
  // 测试除法向上取整的小数情况
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))    // 1.5 -> 2
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))    // 2.666... -> 3
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(13, 2))   // 6.5 -> 7
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(99, 100)) // 0.99 -> 1
}

test "greet_function_multilingual_support" {
  // 测试多语言支持
  @azimuth.assert_eq_string("Hello, Mundo!", @azimuth.greet("Mundo"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Мир!", @azimuth.greet("Мир"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算订单总价
  let unit_price = 25
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping = 12
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(120, total)  // 25*4 + ceil(100*8/100) + 12 = 100 + 8 + 12 = 120
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_resources = 47
  let team_size = 8
  let teams_needed = @azimuth.divide_with_ceil(total_resources, team_size)
  
  @azimuth.assert_eq(6, teams_needed)  // ceil(47/8) = 6
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_hour = 4
  let total_tasks = 15
  let hours_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_hour)
  
  let total_minutes = @azimuth.multiply(hours_needed, 60)
  
  @azimuth.assert_eq(4, hours_needed)     // ceil(15/4) = 4
  @azimuth.assert_eq(240, total_minutes)  // 4 * 60 = 240
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等比数列前n项和
  let first_term = 2
  let ratio = 3
  let _terms = 4  // 使用下划线前缀标记未使用的变量
  
  // 计算 2 + 6 + 18 + 54
  let term1 = first_term
  let term2 = @azimuth.multiply(term1, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  
  @azimuth.assert_eq(80, sum)  // 2 + 6 + 18 + 54 = 80
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))   // 除数为零
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0)) // 除数为零
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0)) // 除数为零
  
  // 极端数值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
}

test "string_concatenation_with_greet" {
  // 字符串连接与问候函数结合测试
  let prefix = "Dr. "
  let name = "Smith"
  let full_name = prefix + name
  let formal_greeting = @azimuth.greet(full_name)
  
  @azimuth.assert_eq_string("Hello, Dr. Smith!", formal_greeting)
  
  // 测试数字字符串
  let numeric_name = "User123"
  let numeric_greeting = @azimuth.greet(numeric_name)
  @azimuth.assert_eq_string("Hello, User123!", numeric_greeting)
}