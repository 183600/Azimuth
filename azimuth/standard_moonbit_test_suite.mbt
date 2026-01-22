// 标准 MoonBit 测试套件
// 包含8个高质量的测试用例，使用标准 MoonBit 测试语法

test "add_function_comprehensive" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  // 零值测试
  assert_eq(0, add(0, 0))
  // 负数测试
  assert_eq(-5, add(-2, -3))
  // 混合正负数测试
  assert_eq(0, add(-5, 5))
  // 大数测试
  assert_eq(2000, add(1000, 1000))
}

test "multiply_function_edge_cases" {
  // 基本乘法测试
  assert_eq(6, multiply(2, 3))
  // 零乘法测试
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  // 负数乘法测试
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  // 单位元测试
  assert_eq(5, multiply(5, 1))
}

test "greet_function_variations" {
  // 基本问候测试
  assert_eq_string("Hello, World!", greet("World"))
  // 空字符串测试
  assert_eq_string("Hello, !", greet(""))
  // 特殊字符测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  // 数字字符串测试
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_comprehensive" {
  // 正数除法向上取整
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  // 负数除法向上取整
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  // 零除数处理
  assert_eq(0, divide_with_ceil(5, 0))
  // 边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "mathematical_properties" {
  // 加法交换律
  assert_eq(add(7, 13), add(13, 7))
  // 乘法交换律
  assert_eq(multiply(7, 13), multiply(13, 7))
  // 加法单位元
  assert_eq(42, add(42, 0))
  // 乘法零元
  assert_eq(0, multiply(42, 0))
}

test "complex_calculation_scenarios" {
  // 复合计算测试
  let step1 = add(5, 3)        // 8
  let step2 = multiply(step1, 2)  // 16
  let step3 = divide_with_ceil(step2, 5)  // 4
  assert_eq(4, step3)
  
  // 包装计算场景
  let total_items = 47
  let box_capacity = 12
  let needed_boxes = divide_with_ceil(total_items, box_capacity)
  assert_eq(4, needed_boxes)  // ceil(47/12) = 4
}

test "string_processing_edge_cases" {
  // Unicode字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  // 特殊符号测试
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  // 长字符串测试
  let long_name = "VeryLongNameThatTestsFunction"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
}

test "arithmetic_boundary_conditions" {
  // 大数运算测试
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1000))
  assert_eq(1000, divide_with_ceil(1000000, 1000))
  assert_eq(1001, divide_with_ceil(1000001, 1000))
  
  // 负数边界测试
  assert_eq(-1, divide_with_ceil(-1, 1000))
  assert_eq(-1000, divide_with_ceil(-1000000, 1000))
}