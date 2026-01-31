// 标准 Azimuth 测试用例 - 包含10个高质量测试用例

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(-5, -5))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边缘情况
  assert_eq(3, subtract(5, 2))
  assert_eq(-3, subtract(2, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_string_handling" {
  // 测试问候函数的字符串处理
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = subtract(step3, 1)          // 3
  assert_eq(3, step4)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-5, -3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-2, subtract(-5, -3))
  assert_eq(-2, divide_with_ceil(-7, 3))
}

test "business_logic_packaging_calculation" {
  // 测试业务逻辑：包装计算
  let items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(items, items_per_box)
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(30, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(5, remaining_space)
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}