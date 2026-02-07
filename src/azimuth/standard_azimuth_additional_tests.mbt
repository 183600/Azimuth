// 标准 Azimuth 额外测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

// 导入 azimuth 包中的函数
test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_comprehensive" {
  // 测试 multiply 函数的综合情况
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "subtract_function_edge_cases" {
  // 测试 subtract 函数的边界情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "divide_with_ceil_function_various_cases" {
  // 测试 divide_with_ceil 函数的各种情况
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  
  // 零值情况
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_different_inputs" {
  // 测试 greet 函数的不同输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let base = 10
  let multiplier = 3
  let offset = 5
  let divisor = 4
  
  // 计算流程
  let step1 = multiply(base, multiplier)  // 10 * 3 = 30
  let step2 = add(step1, offset)          // 30 + 5 = 35
  let step3 = subtract(step2, divisor)   // 35 - 4 = 31
  let final_result = divide_with_ceil(step3, 3)  // ceil(31/3) = 11
  
  assert_eq(30, step1)
  assert_eq(35, step2)
  assert_eq(31, step3)
  assert_eq(11, final_result)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 单位元性质
  assert_eq(add(a, 0), a)
  assert_eq(multiply(a, 1), a)
}

test "boundary_value_comprehensive_test" {
  // 边界值综合测试
  // 零值边界
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 100), 0)
  assert_eq(subtract(0, 0), 0)
  assert_eq(divide_with_ceil(0, 10), 0)
  
  // 单位值边界
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 100), 100)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(100, 1), 100)
  
  // 负单位边界
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 100), -100)
  assert_eq(subtract(0, -1), 1)
  assert_eq(divide_with_ceil(-100, 1), -100)
  
  // 大数边界
  let large_num = 100000
  assert_eq(add(large_num, large_num), 200000)
  assert_eq(divide_with_ceil(large_num, 3), 33334)
}