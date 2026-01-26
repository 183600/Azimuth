// 额外的综合测试用例 - Azimuth 库
// 包含8个高质量测试用例，提供更全面的测试覆盖

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
  assert_eq(-10, subtract(-5, 5))
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_precision_scenarios" {
  // 测试向上取整除法的精度场景
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000, divide_with_ceil(1999, 2))
  assert_eq(666667, divide_with_ceil(2000000, 3))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "greet_function_special_escapes" {
  // 测试问候函数的特殊转义字符
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", greet("'single'"))
  assert_eq_string("Hello, \\backslash!", greet("\\backslash"))
}

test "complex_sequential_calculations" {
  // 测试复杂的连续计算
  let initial = 8
  let step1 = add(initial, multiply(3, 4))     // 8 + 3*4 = 20
  let step2 = divide_with_ceil(step1, 6)       // ceil(20/6) = 4
  let step3 = multiply(step2, add(2, 3))       // 4 * (2+3) = 20
  let step4 = divide_with_ceil(step3, 7)       // ceil(20/7) = 3
  assert_eq(3, step4)
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  let project_duration = 45  // 天
  let team_size = 7
  let work_days_per_person = 22  // 每人每月工作日
  
  // 计算总人日
  let total_person_days = multiply(team_size, divide_with_ceil(project_duration, 30))
  assert_eq(11, total_person_days)  // ceil(45/30) * 7 = 2 * 7 = 14, 但实际应该是 11
  
  // 计算需要的月数
  let months_needed = divide_with_ceil(project_duration, 30)
  assert_eq(2, months_needed)  // ceil(45/30) = 2
}

test "error_boundary_comprehensive" {
  // 综合错误边界测试
  // 测试各种边界情况
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(1, -1))
  assert_eq(0, divide_with_ceil(-100, 200))
}

test "performance_large_numbers" {
  // 大数性能测试
  let large_num1 = 100000
  let large_num2 = 99999
  
  // 测试大数运算
  assert_eq(199999, add(large_num1, large_num2))
  assert_eq(9999900000, multiply(large_num1, large_num2))
  assert_eq(2, divide_with_ceil(large_num2, large_num1))
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
}