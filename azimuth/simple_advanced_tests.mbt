// 简单高级 MoonBit 测试用例 - azimuth 库
// 包含 8 个高质量测试用例，涵盖高级功能和特殊场景

test "performance_calculation_test" {
  // 性能计算测试
  let mut sum = 0
  let mut i = 0
  while i < 100 {
    sum = add(sum, i)
    i = i + 1
  }
  assert_eq(4950, sum)  // 0+1+2+...+99 = 99*100/2 = 4950
  
  let mut product = 1
  let mut j = 1
  while j < 6 {
    product = multiply(product, j)
    j = j + 1
  }
  assert_eq(120, product)  // 1*2*3*4*5 = 120
}

test "precision_analysis_test" {
  // 精度分析测试
  let precision_test_1 = divide_with_ceil(multiply(100, 3), 10)  // ceil(300/10) = 30
  let precision_test_2 = divide_with_ceil(multiply(100, 7), 10)  // ceil(700/10) = 70
  
  assert_eq(30, precision_test_1)
  assert_eq(70, precision_test_2)
  
  let small_number = 1
  let large_divisor = 1000
  let precision_loss_test = divide_with_ceil(small_number, large_divisor)
  assert_eq(1, precision_loss_test)  // ceil(1/1000) = 1
}

test "function_composition_test" {
  // 函数组合测试
  let a = 15
  let b = 25
  let c = 8
  let d = 12
  let e = 7
  
  let step1 = add(a, b)  // 40
  let step2 = add(c, d)  // 20
  let step3 = multiply(step1, step2)  // 800
  let final_result = divide_with_ceil(step3, e)  // ceil(800/7) = 115
  
  assert_eq(40, step1)
  assert_eq(20, step2)
  assert_eq(800, step3)
  assert_eq(115, final_result)
}

test "boundary_condition_test" {
  // 边界条件测试
  let max_safe_int = 10000
  let min_safe_int = -10000
  
  // 极值加法
  assert_eq(20000, add(max_safe_int, max_safe_int))
  assert_eq(-20000, add(min_safe_int, min_safe_int))
  assert_eq(0, add(max_safe_int, min_safe_int))
  
  // 极值乘法
  assert_eq(100000000, multiply(max_safe_int, max_safe_int))
  assert_eq(100000000, multiply(min_safe_int, min_safe_int))
  assert_eq(-100000000, multiply(max_safe_int, min_safe_int))
  
  // 零值边界测试
  assert_eq(0, divide_with_ceil(0, max_safe_int))
  assert_eq(0, divide_with_ceil(0, min_safe_int))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "string_processing_test" {
  // 字符串处理测试
  let special_chars = "!@#$%^&*()"
  assert_eq_string("Hello, !@#$%^&*()!", greet(special_chars))
  
  let numeric_string = "1234567890"
  assert_eq_string("Hello, 1234567890!", greet(numeric_string))
  
  let mixed_string = "ABC123!@#"
  assert_eq_string("Hello, ABC123!@#!", greet(mixed_string))
  
  let emoji_string = "🎉🚀🌟"
  assert_eq_string("Hello, 🎉🚀🌟!", greet(emoji_string))
}

test "mathematical_properties_test" {
  // 数学性质验证测试
  // 验证分配律: a * (b + c) = a * b + a * c
  let a1 = 2
  let b1 = 3
  let c1 = 5
  let left_result1 = multiply(a1, add(b1, c1))
  let right_result1 = add(multiply(a1, b1), multiply(a1, c1))
  assert_eq(left_result1, right_result1)
  
  // 验证结合律: (a + b) + c = a + (b + c)
  let assoc_a = 15
  let assoc_b = 25
  let assoc_c = 35
  let assoc_left = add(add(assoc_a, assoc_b), assoc_c)
  let assoc_right = add(assoc_a, add(assoc_b, assoc_c))
  assert_eq(assoc_left, assoc_right)
}

test "real_world_scenarios_test" {
  // 真实世界场景测试
  // 场景1：项目管理
  let project_tasks = 125
  let team_members = 8
  let tasks_per_person_per_week = 5
  let weeks_per_sprint = 2
  
  let weekly_capacity = multiply(team_members, tasks_per_person_per_week)
  assert_eq(40, weekly_capacity)
  
  let sprints_needed = divide_with_ceil(project_tasks, multiply(weekly_capacity, weeks_per_sprint))
  assert_eq(2, sprints_needed)  // ceil(125/(40*2)) = 2
  
  // 场景2：库存管理
  let product_a = 47  // 每箱装8个
  let product_b = 63  // 每箱装12个
  let product_c = 89  // 每箱装15个
  
  let boxes_a = divide_with_ceil(product_a, 8)   // ceil(47/8) = 6
  let boxes_b = divide_with_ceil(product_b, 12)  // ceil(63/12) = 6
  let boxes_c = divide_with_ceil(product_c, 15)  // ceil(89/15) = 6
  
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(18, total_boxes)
}

test "error_handling_test" {
  // 错误处理测试
  // 除零错误处理
  let positive_dividend = 100
  let negative_dividend = -100
  let zero_dividend = 0
  let zero_divisor = 0
  
  assert_eq(0, divide_with_ceil(positive_dividend, zero_divisor))
  assert_eq(0, divide_with_ceil(negative_dividend, zero_divisor))
  assert_eq(0, divide_with_ceil(zero_dividend, zero_divisor))
  
  // 字符串错误处理
  let empty_string = ""
  let null_like_string = "null"
  let undefined_like_string = "undefined"
  
  assert_eq_string("Hello, !", greet(empty_string))
  assert_eq_string("Hello, null!", greet(null_like_string))
  assert_eq_string("Hello, undefined!", greet(undefined_like_string))
  
  // 复合错误恢复测试
  let error_recovery_1 = add(divide_with_ceil(10, 0), 5)  // 0 + 5 = 5
  let error_recovery_2 = multiply(divide_with_ceil(-10, 0), 3)  // 0 * 3 = 0
  let error_recovery_3 = divide_with_ceil(add(5, multiply(3, 0)), 2)  // ceil(5/2) = 3
  
  assert_eq(5, error_recovery_1)
  assert_eq(0, error_recovery_2)
  assert_eq(3, error_recovery_3)
}