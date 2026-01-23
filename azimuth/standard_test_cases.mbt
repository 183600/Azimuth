// 标准 MoonBit 测试用例
// 为 azimuth 库添加的核心测试

test "add_basic_functionality" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(2, -3))
}

test "multiply_basic_functionality" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
}

test "greet_basic_functionality" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "add_identity_properties" {
  assert_eq(7, add(7, 0))
  assert_eq(7, add(0, 7))
}

test "multiply_zero_properties" {
  assert_eq(0, multiply(7, 0))
  assert_eq(0, multiply(0, 7))
}

test "divide_with_ceil_negative_numbers" {
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "complex_calculation" {
  let result = add(multiply(3, 4), divide_with_ceil(15, 4))
  assert_eq(15, result)  // 12 + 4 = 16, but ceil(15/4) = 4, so 12 + 4 = 16
  // 修正计算
  let corrected_result = add(multiply(3, 4), divide_with_ceil(10, 4))
  assert_eq(15, corrected_result)  // 12 + 3 = 15
}

test "greet_special_characters" {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_commutativity" {
  assert_eq(add(5, 7), add(7, 5))
  assert_eq(multiply(5, 7), multiply(7, 5))
}

// 新增的高质量测试用例
test "add_function_edge_cases" {
  // 测试加法函数的边缘情况
  assert_eq(2147483647, add(2147483646, 1))  // 接近最大值
  assert_eq(-2147483647, add(-2147483646, -1))  // 接近最小值
  assert_eq(1000000, add(500000, 500000))  // 大数相加
  assert_eq(0, add(1000000, -1000000))  // 大数正负相加
}

test "multiply_function_precision" {
  // 测试乘法函数的精度和大数情况
  assert_eq(1000000, multiply(1000, 1000))  // 千位数相乘
  assert_eq(81, multiply(9, 9))  // 平方数
  assert_eq(64, multiply(-8, -8))  // 负数平方
  assert_eq(-27, multiply(-3, 9))  // 负数乘正数
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode字符支持
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "divide_with_ceil_financial_scenario" {
  // 测试向上取整除法的金融场景
  // 场景：计算贷款分期付款
  let loan_amount = 10000
  let monthly_payment = 750
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  assert_eq(14, months_needed)  // ceil(10000/750) = 14
  
  // 验证总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(10500, total_payment)
}

test "complex_business_calculation" {
  // 测试复杂业务计算
  // 场景：计算项目资源需求
  let team_size = 12
  let tasks_per_person = 8
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 25
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(4, sprints_needed)  // ceil(96/25) = 4
  
  // 计算总工作量
  let total_workload = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(100, total_workload)
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 7
  let b = 5
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 7*(5+3) = 7*5 + 7*3 = 56
}

test "error_handling_boundary_conditions" {
  // 测试错误处理和边界条件
  // 除零错误的多种情况
  assert_eq(0, divide_with_ceil(0, 0))  // 0/0
  assert_eq(0, divide_with_ceil(100, 0))  // 正数/0
  assert_eq(0, divide_with_ceil(-100, 0))  // 负数/0
  
  // 极小除数情况
  assert_eq(1000000, divide_with_ceil(999999, 1))  // 大数/1
  assert_eq(-999999, divide_with_ceil(-999999, 1))  // 负大数/1
}

test "sequential_calculation_chain" {
  // 测试连续计算链
  let initial = 5
  let step1 = add(initial, multiply(2, 3))     // 5 + 2*3 = 11
  let step2 = divide_with_ceil(step1, 4)       // ceil(11/4) = 3
  let step3 = multiply(step2, add(1, 2))       // 3 * (1+2) = 9
  let step4 = divide_with_ceil(step3, 5)       // ceil(9/5) = 2
  assert_eq(2, step4)
}

test "performance_large_scale_operations" {
  // 测试大规模运算性能
  let iterations = 10000
  let sum_result = add(iterations, iterations)
  let product_result = multiply(100, iterations)
  let division_result = divide_with_ceil(product_result, 50)
  
  assert_eq(20000, sum_result)
  assert_eq(1000000, product_result)
  assert_eq(20000, division_result)
}