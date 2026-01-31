// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1, add(2147483647, -2147483646))
}

test "multiply_function_boundary_cases" {
  // 测试乘法函数的边界情况
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(12345, multiply(-1, -12345))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2147483647, multiply(46340, 46340))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let product_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping = 12
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(subtotal, tax)
  let final_total = add(total_before_discount, add(shipping, -discount))
  
  assert_eq(356, subtotal)
  assert_eq(29, tax)
  assert_eq(385, total_before_discount)
  assert_eq(377, final_total)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  let min_budget_per_member = 1500
  
  // 使用向上取整确保覆盖所有情况
  let max_share = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, max_share)
  
  // 验证每个成员都能获得最低预算
  assert_true(max_share >= min_budget_per_member)
  
  // 验证总分配不超过预算的110%
  let total_allocated = multiply(max_share, team_members)
  assert_true(total_allocated <= multiply(total_budget, 110) / 100)
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：活动安排
  let participants = 87
  let tables_available = 8
  let chairs_per_table = 10
  let cost_per_chair = 15
  let total_budget = 4000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(1350, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
}

test "sequential_operations_pipeline" {
  // 连续操作流水线测试
  let initial_value = 15
  
  // 第一步：加法
  let step1 = add(initial_value, 8)
  assert_eq(23, step1)
  
  // 第二步：乘法
  let step2 = multiply(step1, 3)
  assert_eq(69, step2)
  
  // 第三步：除法
  let step3 = divide_with_ceil(step2, 10)
  assert_eq(7, step3)
  
  // 第四步：减法
  let step4 = subtract(step3, 2)
  assert_eq(5, step4)
  
  // 验证最终结果
  assert_eq(5, step4)
}