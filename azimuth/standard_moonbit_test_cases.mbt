// 标准 MoonBit 测试用例
// 为 azimuth 库添加的高质量测试用例

test "add_function_basic_operations" {
  // 测试加法函数的基本操作
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_zero_properties" {
  // 测试乘法函数的零性质
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  assert_eq(0, multiply(0, -12345))
  assert_eq(0, multiply(-12345, 0))
  assert_eq(0, multiply(0, 0))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "divide_with_ceil_zero_divisor" {
  // 测试向上取整除法的零除数处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
}

test "subtract_function_basic_operations" {
  // 测试减法函数的基本操作
  assert_eq(4, subtract(7, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-4, subtract(3, 7))
  assert_eq(10, subtract(-5, -15))
  assert_eq(1000000, subtract(1500000, 500000))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_resources = 100
  let teams = 3
  let resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(34, resources_per_team)
  
  // 验证总分配资源
  let total_allocated = multiply(resources_per_team, teams)
  assert_eq(102, total_allocated)
  
  // 验证剩余资源
  let remaining = subtract(total_allocated, total_resources)
  assert_eq(2, remaining)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let addition = 8
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(16, result)  // ceil(15*4/7) + 8 = 9 + 8 = 17
}

test "string_edge_case_handling" {
  // 字符串边缘情况处理测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABC123!", greet("ABC123"))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // 分配律: a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
}