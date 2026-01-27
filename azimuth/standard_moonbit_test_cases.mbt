// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含 8 个高质量测试用例，覆盖各种功能和场景

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
}

test "multiplication_with_zero" {
  // 测试乘法与零的特殊情况
  assert_eq(0, multiply(0, 10))
  assert_eq(0, multiply(10, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(-5, 0))
}

test "greet_function_with_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @user123!", greet("@user123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, phone+123!", greet("phone+123"))
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果处理
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(5, divide_with_ceil(21, 4))
  assert_eq(2, divide_with_ceil(7, 4))
  assert_eq(1, divide_with_ceil(3, 2))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边缘情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 场景：计算项目成本分配
  let total_cost = 5000
  let team_members = 3
  let base_cost_per_person = divide_with_ceil(total_cost, team_members)
  assert_eq(1667, base_cost_per_person)
  
  // 添加管理费用
  let management_fee = 500
  let total_with_fee = add(total_cost, management_fee)
  let final_cost_per_person = divide_with_ceil(total_with_fee, team_members)
  assert_eq(1834, final_cost_per_person)
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 加法交换律验证
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律验证
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 减法非交换律验证
  assert_true(subtract(a, b) != subtract(b, a))
}

test "error_handling_boundary_conditions" {
  // 测试错误处理和边界条件
  // 除零错误处理
  assert_eq(0, divide_with_ceil(15, 0))
  assert_eq(0, divide_with_ceil(-15, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1, divide_with_ceil(1, 2))
}