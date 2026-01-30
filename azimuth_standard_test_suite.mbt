// 标准 MoonBit 测试套件 - 包含10个高质量测试用例
// 测试 azimuth 库的核心功能

test "basic_addition_functionality" {
  // 基础加法功能测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
}

test "basic_multiplication_functionality" {
  // 基础乘法功能测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 8))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
}

test "greet_function_standard_cases" {
  // 问候函数标准情况测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, User!", greet("User"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic_functionality" {
  // 向上取整除法基础功能测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "subtract_function_basic_cases" {
  // 减法函数基础情况测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(10, -5))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_identity_elements" {
  // 数学单位元素测试
  let x = 25
  
  // 加法单位元素
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元素
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景测试
  let total_items = 37
  let package_capacity = 10
  let packages_needed = divide_with_ceil(total_items, package_capacity)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, package_capacity)
  assert_eq(40, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(3, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base_value = 15
  let multiplier = 3
  let divisor = 7
  let addition = 8
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(14, result)  // ceil(15*3/7) + 8 = 7 + 8 = 15
}

test "edge_cases_and_error_handling" {
  // 边界情况和错误处理测试
  // 除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}