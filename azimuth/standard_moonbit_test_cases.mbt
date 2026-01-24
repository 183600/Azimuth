// 标准 MoonBit 测试用例 - azimuth 库
// 包含10个高质量的测试用例，覆盖主要功能

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
  assert_eq(1, multiply(1, 100))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  assert_eq(add(5, 7), add(7, 5))
  assert_eq(multiply(5, 7), multiply(7, 5))
  assert_eq(add(-3, 8), add(8, -3))
  assert_eq(multiply(-3, 8), multiply(8, -3))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  let result1 = add(multiply(3, 4), divide_with_ceil(10, 4))
  assert_eq(15, result1)  // 12 + 3 = 15
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 16
  
  let result3 = divide_with_ceil(multiply(7, 3), 5)
  assert_eq(5, result3)  // ceil(21/5) = 5
}

test "edge_cases_and_boundary_values" {
  // 测试边缘情况和边界值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(1000000, multiply(1000, 1000))
}

test "business_logic_applications" {
  // 测试业务逻辑应用场景
  // 场景：计算需要的包装箱数量
  let total_items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
}

test "error_handling_and_special_cases" {
  // 测试错误处理和特殊情况
  assert_eq(0, divide_with_ceil(10, 0))  // 除零处理
  assert_eq(0, divide_with_ceil(-10, 0))  // 负数除零
  assert_eq(0, divide_with_ceil(0, 0))  // 零除零
  
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "unicode_and_internationalization" {
  // 测试Unicode和国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}