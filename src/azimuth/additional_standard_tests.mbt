// 额外的标准 MoonBit 测试用例
// 针对 azimuth 库的补充测试，不超过10个测试用例

test "add_zero_identity" {
  // 测试加法的零单位元性质
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  assert_eq(0, add(0, 0))
}

test "multiply_by_one_identity" {
  // 测试乘法的单位元性质
  assert_eq(25, multiply(25, 1))
  assert_eq(-8, multiply(-8, 1))
  assert_eq(0, multiply(0, 1))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(0, divide_with_ceil(0, 50))
}

test "greet_function_special_inputs" {
  // 测试问候函数的特殊输入
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let a = 8
  let b = 12
  let c = 3
  
  // 测试 (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(60, result1)  // (8 + 12) * 3 = 20 * 3 = 60
  
  // 测试 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(99, result2)  // 8 * 12 + 3 = 96 + 3 = 99
}

test "negative_division_precision" {
  // 测试负数除法的精度
  assert_eq(-2, divide_with_ceil(-8, 4))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(2, divide_with_ceil(-8, -4))
  assert_eq(1, divide_with_ceil(-3, -4))
}

test "practical_calculation_scenario" {
  // 实际计算场景：计算项目预算
  let daily_cost = 150
  let project_days = 14
  let contingency_rate = 10
  
  let base_budget = multiply(daily_cost, project_days)
  let contingency = divide_with_ceil(multiply(base_budget, contingency_rate), 100)
  let total_budget = add(base_budget, contingency)
  
  assert_eq(2100, base_budget)
  assert_eq(210, contingency)
  assert_eq(2310, total_budget)
}

test "mathematical_distributive_law" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 7
  let c = 9
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 5 * (7 + 9) = 5 * 7 + 5 * 9 = 80
}

test "string_concatenation_with_greet" {
  // 测试字符串连接与问候函数
  let prefix = "Dr."
  let name = "Smith"
  let full_name = prefix + " " + name
  let formal_greeting = greet(full_name)
  
  assert_eq_string("Hello, Dr. Smith!", formal_greeting)
}

test "resource_allocation_calculation" {
  // 资源分配计算测试
  let total_resources = 87
  let team_size = 8
  let teams_needed = divide_with_ceil(total_resources, team_size)
  
  let resources_per_team = divide_with_ceil(total_resources, teams_needed)
  
  assert_eq(11, teams_needed)  // ceil(87/8) = 11
  assert_eq(8, resources_per_team)  // ceil(87/11) = 8
}