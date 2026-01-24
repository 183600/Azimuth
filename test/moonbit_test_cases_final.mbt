// MoonBit 标准测试用例 - 为 azimuth 库创建的高质量测试
// 包含10个测试用例，使用标准的 MoonBit 测试语法

test "add_function_basic_operations" {
  // 测试加法函数的基本操作
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(15, -5))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(1, multiply(1, 100))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 2))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
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
  let step4 = add(step3, 1)          // 5
  assert_eq(5, step4)
}

test "edge_case_handling" {
  // 测试边缘情况处理
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq_string("Hello, !", greet(""))
}

test "business_logic_scenario" {
  // 测试业务逻辑场景：计算装盒问题
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(30, total_capacity)
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "mathematical_identity_properties" {
  // 测试数学恒等式
  let x = 42
  assert_eq(x, add(x, 0))      // 加法恒等式
  assert_eq(x, multiply(x, 1)) // 乘法恒等式
  assert_eq(0, multiply(x, 0)) // 零乘法性质
  assert_eq(-42, multiply(x, -1)) // 负一乘法性质
}