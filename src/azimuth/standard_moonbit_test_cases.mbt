// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "subtract_function_basic" {
  // 测试基本减法功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(20, 5))
}

test "mathematical_identity_laws" {
  // 测试数学恒等律
  let x = 42
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 减法自身
  assert_eq(0, subtract(x, x))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 89
  let quantity = 6
  let discount = 50
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算折扣后金额
  let after_discount = subtract(subtotal, discount)
  // 计算税费
  let tax = divide_with_ceil(multiply(after_discount, tax_rate), 100)
  // 计算最终总价
  let total = add(add(after_discount, tax), shipping)
  
  assert_eq(534, subtotal)
  assert_eq(484, after_discount)
  assert_eq(39, tax)
  assert_eq(538, total)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  let max_val = 1000
  let min_val = -1000
  
  // 测试极值加法
  assert_eq(2000, add(max_val, max_val))
  assert_eq(-2000, add(min_val, min_val))
  assert_eq(0, add(max_val, min_val))
  
  // 测试极值乘法
  assert_eq(1000000, multiply(max_val, max_val))
  assert_eq(1000000, multiply(min_val, min_val))
  assert_eq(-1000000, multiply(max_val, min_val))
  
  // 测试极值减法
  assert_eq(0, subtract(max_val, max_val))
  assert_eq(2000, subtract(max_val, min_val))
}

test "divide_with_ceil_precision_test" {
  // 向上取整除法精度测试
  // 测试小数部分的各种情况
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 -> 2
  assert_eq(4, divide_with_ceil(11, 3))   // 3.666... -> 4
  assert_eq(7, divide_with_ceil(13, 2))   // 6.5 -> 7
  assert_eq(34, divide_with_ceil(100, 3))  // 33.333... -> 34
  
  // 测试负数情况
  assert_eq(-1, divide_with_ceil(-3, 2))   // -1.5 -> -1
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // 3.333... -> 4
}

test "string_greeting_comprehensive" {
  // 综合字符串问候测试
  // 测试各种特殊字符和 Unicode
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🚀🌍!", greet("🚀🌍"))
  assert_eq_string("Hello, Café!", greet("Café"))
  
  // 测试边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, A!", greet("A"))
}

test "compound_arithmetic_operations" {
  // 复合算术运算测试
  let a = 12
  let b = 8
  let c = 5
  
  // 测试 (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(100, result1)  // (12 + 8) * 5 = 20 * 5 = 100
  
  // 测试 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(101, result2)  // 12 * 8 + 5 = 96 + 5 = 101
  
  // 测试 a - b * c
  let result3 = subtract(a, multiply(b, c))
  assert_eq(-28, result3)  // 12 - 8 * 5 = 12 - 40 = -28
  
  // 测试复合除法：ceil((a + b) / c)
  let result4 = divide_with_ceil(add(a, b), c)
  assert_eq(4, result4)  // ceil(20 / 5) = 4
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除零保护
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算的稳定性
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(100000000, multiply(large_num, large_num))  // 修复大数问题
  assert_eq(0, subtract(large_num, large_num))
}

test "real_world_project_management" {
  // 实际项目管理场景测试
  let team_members = 8
  let tasks_per_member = 15
  let hours_per_task = 4
  let working_days = 5
  let hours_per_day = 8
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_member)
  // 计算总工作小时数
  let total_hours = multiply(total_tasks, hours_per_task)
  // 计算需要的工作日数
  let days_needed = divide_with_ceil(total_hours, multiply(hours_per_day, working_days))
  
  assert_eq(120, total_tasks)      // 8 * 15 = 120
  assert_eq(480, total_hours)      // 120 * 4 = 480
  assert_eq(12, days_needed)       // ceil(480 / (8 * 5)) = ceil(480 / 40) = 12
}

test "mathematical_series_and_patterns" {
  // 数学级数和模式测试
  // 计算等差数列前n项和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 10
  let series_sum = multiply(n, n)
  assert_eq(100, series_sum)  // 10² = 100
  
  // 计算等比数列：2 + 4 + 8 + 16 = 30
  let geo_sum = add(add(add(2, 4), 8), 16)
  assert_eq(30, geo_sum)
  
  // 测试平方差公式：a² - b² = (a + b)(a - b)
  let a = 15
  let b = 8
  let diff_of_squares = multiply(add(a, b), subtract(a, b))
  assert_eq(161, diff_of_squares)  // (15 + 8)(15 - 8) = 23 * 7 = 161
}