// Azimuth 增强标准测试用例
// 使用标准 MoonBit 测试语法编写的高质量测试用例

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_negative_scenarios" {
  // 测试向上取整除法的负数场景
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(-5, 10))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 场景：计算项目资源分配
  let team_members = 8
  let daily_hours = 8
  let project_days = 15
  let total_tasks = multiply(team_members, multiply(daily_hours, project_days))
  let tasks_per_sprint = 100
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(10, sprints_needed)  // ceil(960/100) = 10
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "resource_optimization_packaging" {
  // 测试资源优化包装问题
  // 产品1：47个，每箱容量12
  let product_1 = 47
  let box_size_1 = 12
  let boxes_1 = divide_with_ceil(product_1, box_size_1)  // ceil(47/12) = 4
  
  // 产品2：89个，每箱容量15
  let product_2 = 89
  let box_size_2 = 15
  let boxes_2 = divide_with_ceil(product_2, box_size_2)  // ceil(89/15) = 6
  
  // 产品3：156个，每箱容量20
  let product_3 = 156
  let box_size_3 = 20
  let boxes_3 = divide_with_ceil(product_3, box_size_3)  // ceil(156/20) = 8
  
  // 产品4：23个，每箱容量8
  let product_4 = 23
  let box_size_4 = 8
  let boxes_4 = divide_with_ceil(product_4, box_size_4)  // ceil(23/8) = 3
  
  let total_boxes = add(add(boxes_1, boxes_2), add(boxes_3, boxes_4))
  assert_eq(21, total_boxes)
}

test "error_handling_boundary_conditions" {
  // 测试错误处理和边界条件
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "sequential_calculation_pipeline" {
  // 测试连续计算流水线
  let initial_value = 12
  
  // 计算流程：((initial * 3) + 15) / 8 + 2
  let step1 = multiply(initial_value, 3)        // 36
  let step2 = add(step1, 15)                    // 51
  let step3 = divide_with_ceil(step2, 8)        // 7
  let result = add(step3, 2)                    // 9
  
  assert_eq(9, result)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景
  // 场景：计算贷款分期
  let loan_amount = 50000
  let monthly_payment = 2200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(23, months_needed)  // ceil(50000/2200) = 23
  
  // 验证总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)
  
  // 验证最后一个月的支付金额
  let final_payment = subtract(total_payment, multiply(months_needed - 1, monthly_payment))
  assert_eq(200, final_payment)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边缘情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, MixedCASE!", greet("MixedCASE"))
}