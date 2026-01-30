// Azimuth 标准测试用例 - 新增10个高质量测试用例
// 使用标准 MoonBit 测试语法，覆盖各种功能和场景

test "basic_arithmetic_addition" {
  // 基本算术加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  assert_eq(1000, add(500, 500))
}

test "basic_arithmetic_multiplication" {
  // 基本算术乘法测试
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "subtract_function_basic" {
  // 基本减法函数测试
  assert_eq(3, subtract(10, 7))
  assert_eq(0, subtract(5, 5))
  assert_eq(-3, subtract(2, 5))
  assert_eq(10, subtract(5, -5))
  assert_eq(-1000, subtract(-500, 500))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 15
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(23, add(15, 8))
  assert_eq(23, add(8, 15))
  assert_eq(120, multiply(15, 8))
  assert_eq(120, multiply(8, 15))
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_resources = 47
  let teams = 6
  let resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(8, resources_per_team)
  
  // 验证总分配资源
  let total_allocated = multiply(resources_per_team, teams)
  assert_eq(48, total_allocated)
  
  // 验证剩余资源
  let remaining = subtract(total_allocated, total_resources)
  assert_eq(1, remaining)
}

test "error_handling_division_by_zero" {
  // 除零错误处理测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  assert_eq(0, divide_with_ceil(-1000000, 0))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial_value = 12
  
  // 计算步骤：(12 + 8) * 3 / 5 + 2
  let step1 = add(initial_value, 8)  // 20
  let step2 = multiply(step1, 3)     // 60
  let step3 = divide_with_ceil(step2, 5)  // 12
  let final_result = add(step3, 2)   // 14
  
  assert_eq(20, step1)
  assert_eq(60, step2)
  assert_eq(12, step3)
  assert_eq(14, final_result)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试极值和边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  
  // 测试接近边界的情况
  assert_eq(2, divide_with_ceil(999999, 500000))
  assert_eq(1, divide_with_ceil(500000, 500000))
  assert_eq(2, divide_with_ceil(500001, 500000))
}