// Azimuth 项目高质量 MoonBit 测试用例
// 测试核心数学函数和字符串处理功能

test "basic_arithmetic_operations_validation" {
  // 基本算术运算验证测试
  assert_eq(10, add(6, 4))
  assert_eq(-2, add(-5, 3))
  assert_eq(0, add(0, 0))
  
  assert_eq(24, multiply(6, 4))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(0, multiply(100, 0))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数情况测试
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
}

test "greet_function_multilingual_support" {
  // 问候函数多语言支持测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 15
  let y = 7
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "real_world_packaging_calculation" {
  // 真实世界包装计算测试
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  let base_value = 20
  let multiplier = 3
  let divisor = 5
  let addition = 7
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(19, result)  // ceil(20*3/5) + 7 = 12 + 7 = 19
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_tasks = 65
  let team_members = 4
  let tasks_per_person_per_day = 3
  let working_days = 8
  
  // 计算团队日处理能力
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  
  // 计算需要的总天数
  let days_needed = divide_with_ceil(total_tasks, daily_capacity)
  
  // 验证时间安排
  assert_true(days_needed <= working_days)
  assert_eq(6, days_needed)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(15, 0))
  assert_eq(0, divide_with_ceil(-15, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "business_financial_calculation" {
  // 业务金融计算测试
  let product_price = 89
  let quantity = 7
  let tax_rate = 8
  let discount_threshold = 600
  let discount_amount = 50
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(subtotal, tax_amount)
  
  // 应用折扣
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(623, subtotal)
  assert_eq(50, tax_amount)
  assert_eq(673, pre_discount_total)
  assert_eq(623, final_total)
}