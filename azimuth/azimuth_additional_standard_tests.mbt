// 额外的标准 MoonBit 测试用例
// 为 Azimuth 项目添加高质量的测试覆盖

test "add_function_commutative_property" {
  // 测试加法函数的交换律性质
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_identity_property" {
  // 测试乘法函数的恒等性质
  let x = 42
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "subtract_function_negative_results" {
  // 测试减法函数的负数结果
  assert_eq(-5, subtract(3, 8))
  assert_eq(-10, subtract(0, 10))
  assert_eq(-100, subtract(50, 150))
  assert_eq(-1, subtract(99, 100))
}

test "complex_calculation_workflow" {
  // 测试复杂计算工作流
  let base_value = 10
  let multiplier = 3
  let increment = 5
  let divisor = 4
  
  let step1 = multiply(base_value, multiplier)  // 30
  let step2 = add(step1, increment)             // 35
  let result = divide_with_ceil(step2, divisor) // 9
  
  assert_eq(9, result)
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 5
  let b = 3
  let c = 2
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
  assert_eq(25, left_side)
}

test "edge_case_zero_operations" {
  // 测试零操作的边缘情况
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "large_number_arithmetic" {
  // 测试大数算术运算
  let big_num = 100000
  assert_eq(200000, add(big_num, big_num))
  assert_eq(10000000000, multiply(big_num, big_num))
  assert_eq(1, divide_with_ceil(big_num, big_num))
  assert_eq(2, divide_with_ceil(big_num + 1, big_num))
}

test "business_resource_allocation" {
  // 测试业务资源分配场景
  let total_tasks = 87
  let team_capacity = 20
  let teams_needed = divide_with_ceil(total_tasks, team_capacity)
  
  assert_eq(5, teams_needed)
  
  let total_capacity = multiply(teams_needed, team_capacity)
  let unused_capacity = subtract(total_capacity, total_tasks)
  
  assert_eq(100, total_capacity)
  assert_eq(13, unused_capacity)
}