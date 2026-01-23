// 新增的 MoonBit 测试用例
// 为 azimuth 库添加额外的测试覆盖

test "add_function_negative_numbers" {
  // 测试加法函数的负数情况
  assert_eq(-10, add(-5, -5))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
  assert_eq(0, add(-10, 10))
}

test "multiply_function_negative_numbers" {
  // 测试乘法函数的负数情况
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(0, multiply(-5, 0))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, éspanol!", greet("éspanol"))
  assert_eq_string("Hello, emoji 🎉!", greet("emoji 🎉"))
}

test "divide_with_ceil_mixed_operations" {
  // 测试向上取整除法的混合运算
  let a = divide_with_ceil(10, 3)
  let b = divide_with_ceil(20, 7)
  let result = add(a, b)
  assert_eq(8, result)  // ceil(10/3) + ceil(20/7) = 4 + 4 = 8
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 场景：计算项目资源需求
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(3, sprints_needed)  // ceil(40/15) = 3
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
}

test "divide_with_ceil_large_numbers" {
  // 测试向上取整除法的大数情况
  assert_eq(1000, divide_with_ceil(999999, 1000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
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

test "greet_function_whitespace_handling" {
  // 测试问候函数的空白字符处理
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, leading space!", greet(" leading space"))
  assert_eq_string("Hello, trailing space !", greet("trailing space "))
  assert_eq_string("Hello, multiple   spaces!", greet("multiple   spaces"))
}

test "divide_with_ceil_negative_dividend" {
  // 测试向上取整除法的负被除数情况
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-2, divide_with_ceil(-3, 2))
  assert_eq(-5, divide_with_ceil(-10, 2))
  assert_eq(-4, divide_with_ceil(-7, 2))
}