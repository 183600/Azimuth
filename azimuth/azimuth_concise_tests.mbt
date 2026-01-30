// Azimuth 简洁测试用例 - 8个标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_functions" {
  // 基础算术函数测试
  assert_eq(8, add(3, 5))
  assert_eq(-2, add(-5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(0, multiply(0, 100))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_variations" {
  // 问候函数变体测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 7
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "subtract_function_basic" {
  // 减法函数基础测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(10, 10))
  assert_eq(15, subtract(10, -5))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(2, divide_with_ceil(-7, -4))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(5, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base = 8
  let multiplier = 3
  let divisor = 5
  let addition = 7
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(12, result)  // ceil(8*3/5) + 7 = 5 + 7 = 12
}