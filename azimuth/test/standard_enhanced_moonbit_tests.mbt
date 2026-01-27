// 标准 MoonBit 增强测试用例
// 包含9个高质量测试用例，覆盖基本功能和边界情况

test "add_function_standard_cases" {
  // 加法函数标准测试用例
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(-5, -5))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_variations" {
  // 问候函数变化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_commutative_law" {
  // 数学交换律验证
  let a = 7
  let b = 11
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  let packages = 29
  let capacity = 6
  let needed = divide_with_ceil(packages, capacity)
  assert_eq(5, needed)
  
  let total_capacity = multiply(needed, capacity)
  assert_eq(30, total_capacity)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(2, divide_with_ceil(-8, -3))
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let base = 10
  let step1 = add(base, 5)        // 15
  let step2 = multiply(step1, 2)  // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = add(step3, 1)        // 5
  assert_eq(5, step4)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}