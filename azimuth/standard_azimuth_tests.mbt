// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含10个高质量测试用例，覆盖主要功能

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-2, 3))
  assert_eq(-1, add(2, -3))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的全面情况
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(15, multiply(3, 5))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试向上取整除法的正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
  assert_eq(add(-5, 10), add(10, -5))
  assert_eq(multiply(-4, 6), multiply(6, -4))
}

test "mathematical_properties_identity" {
  // 测试数学恒等式性质
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(0, multiply(5, 0))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 场景：计算装盒问题
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(30, total_capacity)
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "sequential_operations_chain" {
  // 测试连续操作链
  let initial = 5
  let step1 = add(initial, 3)        // 8
  let step2 = multiply(step1, 2)     // 16
  let step3 = divide_with_ceil(step2, 5)  // 4
  let step4 = add(step3, 1)          // 5
  
  assert_eq(8, step1)
  assert_eq(16, step2)
  assert_eq(4, step3)
  assert_eq(5, step4)
}